-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Core
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Core
( -- * Client configuration
  LambdaConfiguration(..)
  -- ** Lenses
, lcRegion
, lcAccessKeyId
, lcSecretAccessKey

  -- * Client query
, LambdaQuery(..)
, lambdaQuery
  -- ** Lenses
, lqMethod
, lqEndpoint
, lqParams
, lqBody

  -- * Transaction machinery
, LambdaTransaction(..)
, ExhaustiveLambdaTransaction(..)

  -- * Exceptions
, InvalidHttpMethodException
, _InvalidHttpMethodException
, InvalidRegionException
, _InvalidRegionException

  -- ** Exception Patterns
, pattern InvalidParameterValueException
, pattern ResourceNotFoundException
, pattern ServiceException
) where

import Aws.General

import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Unicode

import qualified Data.Aeson as AE
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Monoid.Unicode
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Types
import Network.HTTP.Client
import qualified Network.Wreq as W
import Prelude.Unicode

data LambdaConfiguration
  = LambdaConfiguration
  { _lcRegion ∷ !Region
  , _lcAccessKeyId ∷ !B.ByteString
  , _lcSecretAccessKey ∷ !B.ByteString
  } deriving (Eq, Show)

makeLenses ''LambdaConfiguration

lambdaOptions
  ∷ LambdaConfiguration
  → W.Options
  → W.Options
lambdaOptions lc =
  foldr (∘) id $
    [ W.auth ?~ W.awsAuth W.AWSv4 (lc ^. lcAccessKeyId) (lc ^. lcSecretAccessKey)
    , W.header "Accept" .~ ["application/json"]
    ]

data LambdaQuery
  = LambdaQuery
  { _lqBody ∷ !(Maybe AE.Value)
  , _lqParams ∷ !(M.Map T.Text T.Text)
  , _lqEndpoint ∷ !T.Text
  , _lqMethod ∷ !StdMethod
  } deriving (Eq, Show)

makeLenses ''LambdaQuery

-- | A convenience constructor for a basic query.
--
lambdaQuery
  ∷ StdMethod
  → T.Text
  → LambdaQuery
lambdaQuery meth ep = LambdaQuery
  { _lqMethod = meth
  , _lqEndpoint = ep
  , _lqBody = Nothing
  , _lqParams = M.empty
  }

newtype InvalidRegionException
  = InvalidRegionException
  { _ireRegion ∷ Region
  } deriving (Eq, Show, Typeable)
instance Exception InvalidRegionException

makePrisms ''InvalidRegionException

lambdaEndpointUrl
  ∷ MonadThrow m
  ⇒ Region -- ^ The AWS region to target
  → T.Text -- ^ The name of the AWS Lambda endpoint
  → m String
lambdaEndpointUrl r e = do
  subdomain ← case r of
    UsWest2 → return "us-west-2"
    UsEast1 → return "us-east-1"
    EuWest1 → return "eu-west-1"
    _ → throwM $ InvalidRegionException r
  return $
    "https://lambda." ⊕ subdomain ⊕ ".amazonaws.com/2014-11-13/" ⊕ T.unpack e

liftThrow
  ∷ ( MonadThrow m
    , MonadIO m
    )
  ⇒ IO α
  → m α
liftThrow m = do
  either throwM return =≪ do
    liftIO $ catch
      (fmap Right m)
      (\(e ∷ SomeException) → return $ Left e)

newtype InvalidHttpMethodException
  = InvalidHttpMethodException
  { _ihmeMethod ∷ StdMethod
  } deriving (Eq, Show, Typeable)
instance Exception InvalidHttpMethodException

makePrisms ''InvalidHttpMethodException

pattern InvalidParameterValueException msg
  ← StatusCodeException (Status 400 msg) _ _
pattern ResourceNotFoundException msg
  ← StatusCodeException (Status 400 msg) _ _
pattern ServiceException msg
  ← StatusCodeException (Status 500 msg) _ _

-- | A class for associating a request type with a response type.
--
class AE.FromJSON resp ⇒ LambdaTransaction req resp | req → resp, resp → req where

  -- | Construct a 'LambdaQuery' object from the request data.
  --
  buildQuery
    ∷ req
    → LambdaQuery

  -- | Send the request to AWS Lambda.
  --
  runLambda
    ∷ ( MonadThrow m
      , MonadIO m
      )
    ⇒ LambdaConfiguration
    → req
    → m resp
  runLambda cfg req = do
    let query = buildQuery req
        opts = lambdaOptions cfg W.defaults
          & W.params <>~ query ^. lqParams ∘ to M.toList
        body = query ^. lqBody ∘ to (fromMaybe AE.Null)
    url ← lambdaEndpointUrl (cfg ^. lcRegion) (query ^. lqEndpoint)
    resp ← case query ^. lqMethod of
      GET → liftThrow $ W.getWith opts url
      POST → liftThrow $ W.postWith opts url body
      PUT → liftThrow $ W.putWith opts url body
      DELETE → liftThrow $ W.deleteWith opts url
      meth → throwM $ InvalidHttpMethodException meth
    resp ^! act W.asJSON ∘ W.responseBody

class (LambdaTransaction req resp, Monoid acc) ⇒ ExhaustiveLambdaTransaction req resp cur acc | req → resp cur acc where
  -- | To set the cursor in subsequent requests.
  --
  requestCursor ∷ Setter' req (Maybe cur)

  -- | To get the cursor in respones.
  --
  responseCursor ∷ Getter resp (Maybe cur)

  -- | To get the accumulating portion of the response data.
  --
  responseAccum ∷ Getter resp acc

  -- | Exhaustively iterates a request to AWS lambda and returns the
  -- accumulated results.
  --
  exhaustLambda
    ∷ ( MonadThrow m
      , MonadIO m
      , Functor m
      )
    ⇒ LambdaConfiguration
    → req
    → m acc
  exhaustLambda cfg req = do
    resp ← runLambda cfg req
    case resp ^. responseCursor of
      Just cur →
        mappend (resp ^. responseAccum)
          <$> exhaustLambda cfg (req & requestCursor ?~ cur)
      Nothing →
        return $
          resp ^. responseAccum
