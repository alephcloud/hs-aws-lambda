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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Core
( -- * Client configuration
  LambdaConfiguration(..)
  -- ** Lenses
, lcRegion

  -- * Client metadata
, LambdaMetadata(..)
  -- ** Lenses
, lmdRequestId

  -- * Response errors
, LambdaErrorResponseData(..)
, LambdaOtherErrorData(..)
, LambdaResponseJsonErrorData(..)
, LambdaErrorResponse(..)
  -- ** Lenses
, lerdErrorCode
, lerdErrorMessage
, loedStatus
, loedMessage
, lrjedMessage
, lrjedJSON
  -- ** Prisms
, _LambdaResponseJsonError
, _LambdaErrorResponse
, _LambdaOtherError

  -- * Response consumers
, lambdaResponseConsumer
, jsonResponseConsumer
, errorResponseConsumer

  -- * Internal
, LambdaAction(..)
, lambdaServiceEndpoint

  -- ** Queries
, LambdaQuery(..)
, lambdaSignQuery
  -- *** Lenses
, lqAction
, lqBody
) where

import Aws.Core
import Aws.General

import Control.Applicative
import Control.Applicative.Unicode
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Resource (throwM)

import qualified Crypto.Hash as CH
import qualified Data.Aeson as AE
import Data.Aeson ((.:))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LB
import qualified Data.Byteable as DB
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import Data.Conduit.Binary (sinkLbs)
import Data.Function (on)
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Monoid.Unicode
import Data.Profunctor
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP
import Prelude.Unicode

import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P
import Text.Parser.Combinators ((<?>))

data LambdaConfiguration qt
  = LambdaConfiguration
  { _lcRegion ∷ !Region
  } deriving (Eq, Show)

-- | A lens for '_lcRegion'.
--
-- @
-- lcRegion ∷ Lens' ('LambdaConfiguration' qt) 'Region'
-- @
--
lcRegion
  ∷ Functor f
  ⇒ (Region → f Region)
  → LambdaConfiguration qt
  → f (LambdaConfiguration qt)
lcRegion i LambdaConfiguration{..} =
  LambdaConfiguration <$> i _lcRegion


data LambdaMetadata
  = LambdaMetadata
  { _lmdRequestId ∷ !(Maybe T.Text)
  } deriving (Eq, Show)

instance Monoid LambdaMetadata where
  mempty = LambdaMetadata Nothing
  mappend (LambdaMetadata rid) (LambdaMetadata rid') =
    LambdaMetadata $ rid <|> rid'

instance Loggable LambdaMetadata where
  toLogText LambdaMetadata{..} =
    "Lambda: request ID=" <> fromMaybe "<none>" _lmdRequestId

-- | A lens for '_lmdRequestId'.
--
-- @
-- lmdRequestId ∷ Lens' 'LambdaMetadata' ('Maybe' 'T.Text')
-- @
--
lmdRequestId
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → LambdaMetadata
  → f LambdaMetadata
lmdRequestId i LambdaMetadata{..} =
  LambdaMetadata <$> i _lmdRequestId

data LambdaAction
  = LambdaAddEventSource
  | LambdaDeleteFunction
  | LambdaGetEventSource
  | LambdaGetFunction
  | LambdaGetFunctionConfiguration
  | LambdaInvokeAsync
  | LambdaListEventSources
  | LambdaListFunctions
  | LambdaRemoveEventSource
  | LambdaUpdateFunctionConfiguration
  | LambdaUploadFunction
  deriving
    ( Eq
    , Ord
    , Enum
    , Bounded
    , Typeable
    , Read
    , Show
    )

lambdaActionToText
  ∷ IsString s
  ⇒ LambdaAction
  → s
lambdaActionToText = \case
  LambdaAddEventSource → "AddEventSource"
  LambdaDeleteFunction → "DeleteFunction"
  LambdaGetEventSource → "GetEventSource"
  LambdaGetFunction → "GetFunction"
  LambdaGetFunctionConfiguration → "GetFunctionConfiguration"
  LambdaInvokeAsync → "InvokeAsync"
  LambdaListEventSources → "ListEventSources"
  LambdaListFunctions → "ListFunctions"
  LambdaRemoveEventSource → "RemoveEventSource"
  LambdaUpdateFunctionConfiguration → "UpdateFunctionConfiguration"
  LambdaUploadFunction → "UploadFunction"

parseLambdaAction
  ∷ P.CharParsing m
  ⇒ m LambdaAction
parseLambdaAction = P.choice parsers <?> "LambdaAction"
  where
    actionToParser a =
      a <$ P.text (lambdaActionToText a)

    parsers = actionToParser <$>
      [ LambdaAddEventSource
      , LambdaDeleteFunction
      , LambdaGetEventSource
      , LambdaGetFunction
      , LambdaGetFunctionConfiguration
      , LambdaInvokeAsync
      , LambdaListEventSources
      , LambdaListFunctions
      , LambdaRemoveEventSource
      , LambdaUpdateFunctionConfiguration
      , LambdaUploadFunction
      ]

instance AwsType LambdaAction where
  toText = lambdaActionToText
  parse = parseLambdaAction

lambdaServiceEndpoint
  ∷ Region
  → B8.ByteString
lambdaServiceEndpoint = \case
  UsEast1 → "lambda.us-east-1.amazonaws.com"
  UsWest2 → "lambda.us-west-2.amazonaws.com"
  EuWest1 → "lambda.eu-west-1.amazonaws.com"
  _ → error "Unsupported region for AWS lambda"

data LambdaQuery
  = LambdaQuery
  { _lqAction ∷ !LambdaAction
  , _lqBody ∷ !LB.ByteString
  } deriving (Eq, Show)

-- | A lens for '_lqAction'.
--
-- @
-- 'lqAction' ∷ Lens' 'LambdaQuery' 'LambdaAction'
-- @
--
lqAction
  ∷ Functor f
  ⇒ (LambdaAction → f LambdaAction)
  → LambdaQuery
  → f LambdaQuery
lqAction i LambdaQuery{..} =
  (\_lqAction → LambdaQuery{..})
    <$> i _lqAction

-- | A lens for '_lqBody'.
--
-- @
-- 'lqBody' ∷ Lens' 'LambdaQuery' 'LB.ByteString'
-- @
--
lqBody
  ∷ Functor f
  ⇒ (LB.ByteString → f LB.ByteString)
  → LambdaQuery
  → f LambdaQuery
lqBody i LambdaQuery{..} =
  (\_lqBody → LambdaQuery{..})
    <$> i _lqBody

lambdaTargetVersion ∷ IsString a ⇒ a
lambdaTargetVersion = "Lambda_20120810"

lambdaTargetHeader
  ∷ LambdaAction
  → HTTP.Header
lambdaTargetHeader a =
  ( "X-Amz-Target"
  , lambdaTargetVersion ⊕ "." ⊕ toText a
  )


-- | Creates a signed query.
--
-- Uses AWS Signature V4. All requests are POST requests
-- with the signature placed in an HTTP header
--
lambdaSignQuery
  ∷ LambdaQuery
  → LambdaConfiguration qt
  → SignatureData
  → SignedQuery
lambdaSignQuery LambdaQuery{..} LambdaConfiguration{..} sigData = SignedQuery
  { sqMethod = Post
  , sqProtocol = HTTP
  , sqHost = host
  , sqPort = 80
  , sqPath = "/"
  , sqQuery = []
  , sqDate = Just $ signatureTime sigData
  , sqAuthorization = Just auth
  , sqContentType = Just "application/x-amz-json-1.0"
  , sqContentMd5 = Nothing
  , sqAmzHeaders = amzHeaders ⊕ maybe [] pure securityTokenHeader
  , sqOtherHeaders = []
  , sqBody = Just $ HTTP.RequestBodyLBS _lqBody
  , sqStringToSign = canonicalRequest
  }
    where
      credentials = signatureCredentials sigData
      host = lambdaServiceEndpoint _lcRegion
      sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime sigData

      -- for some reason AWS doesn't want the x-amz-security-token in the canonical request
      amzHeaders =
        [ ("x-amz-date", sigTime)
        , lambdaTargetHeader _lqAction
        ]

      securityTokenHeader =
        ("x-amz-security-token",)
          <$> iamToken credentials

      canonicalHeaders =
        L.sortBy (compare `on` fst) $
          amzHeaders ⊕
            [ ("host", host)
            , ("content-type", "application/x-amz-json-1.0")
            ]

      canonicalRequest =
        let bodyHash = B16.encode $ DB.toBytes (CH.hashlazy _lqBody :: CH.Digest CH.SHA256)
            headers =
              flip fmap canonicalHeaders $ \(a,b) →
                [ CI.foldedCase a
                , ":"
                , b
                ]
        in
          B.concat ∘ L.intercalate ["\n"] $
            [ [ "POST" ]
            , [ "/" ]
            , [] -- query string
            ] ⊕ headers ⊕
              [ [] -- end headers
              , L.intersperse ";" ((CI.foldedCase ∘ fst) <$> canonicalHeaders)
              , [ bodyHash ]
              ]

      auth =
        authorizationV4
          sigData
          HmacSHA256
          (regionToText _lcRegion)
          "lambda"
          "content-type;host;x-amz-date;x-amz-target"
          canonicalRequest

data LambdaErrorResponseData
  = LambdaErrorResponseData
  { _lerdErrorCode ∷ !T.Text
  , _lerdErrorMessage ∷ !T.Text
  } deriving (Eq, Show, Typeable)

-- | A lens for '_lerdErrorCode'.
--
-- @
-- 'lerdErrorCode' ∷ Lens' 'LambdaErrorResponseData' 'T.Text'
-- @
--
lerdErrorCode
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → LambdaErrorResponseData
  → f LambdaErrorResponseData
lerdErrorCode i LambdaErrorResponseData{..} =
  (\_lerdErrorCode → LambdaErrorResponseData{..})
    <$> i _lerdErrorCode
{-# INLINE lerdErrorCode #-}

-- | A lens for '_lerdErrorMessage'.
--
-- @
-- 'lerdErrorMessage' ∷ Lens' 'LambdaErrorResponseData' 'T.Text'
-- @
--
lerdErrorMessage
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → LambdaErrorResponseData
  → f LambdaErrorResponseData
lerdErrorMessage i LambdaErrorResponseData{..} =
  (\_lerdErrorMessage → LambdaErrorResponseData{..})
    <$> i _lerdErrorMessage
{-# INLINE lerdErrorMessage #-}

data LambdaOtherErrorData
  = LambdaOtherErrorData
  { _loedStatus ∷ !HTTP.Status
  , _loedMessage ∷ !T.Text
  } deriving (Eq, Show, Typeable)

-- | A lens for '_loedStatus'
--
-- @
-- 'loedStatus' ∷ Lens' 'LambdaOtherErrorData' 'HTTP.Status'
-- @
--
loedStatus
  ∷ Functor f
  ⇒ (HTTP.Status → f HTTP.Status)
  → LambdaOtherErrorData
  → f LambdaOtherErrorData
loedStatus i LambdaOtherErrorData{..} =
  (\_loedStatus → LambdaOtherErrorData{..})
    <$> i _loedStatus
{-# INLINE loedStatus #-}

-- | A lens for '_loedMessage'
--
-- @
-- 'loedMessage' ∷ Lens' 'LambdaOtherErrorData' 'T.Text'
-- @
--
loedMessage
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → LambdaOtherErrorData
  → f LambdaOtherErrorData
loedMessage i LambdaOtherErrorData{..} =
  (\_loedMessage → LambdaOtherErrorData{..})
    <$> i _loedMessage
{-# INLINE loedMessage #-}

data LambdaResponseJsonErrorData
  = LambdaResponseJsonErrorData
  { _lrjedMessage ∷ !T.Text
  , _lrjedJSON ∷ !LB.ByteString
  } deriving (Eq, Show, Typeable)

-- | A lens for '_lrjedMessage'.
--
-- @
-- 'lrjedMessage' ∷ Lens' 'LambdaResponseJsonErrorData' 'T.Text'
-- @
lrjedMessage
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → LambdaResponseJsonErrorData
  → f LambdaResponseJsonErrorData
lrjedMessage i LambdaResponseJsonErrorData{..} =
  (\_lrjedMessage → LambdaResponseJsonErrorData{..})
    <$> i _lrjedMessage

-- | A lens for '_lrjedJSON'.
--
-- @
-- 'lrjedJSON' ∷ Lens' 'LambdaResponseJsonErrorData' 'LB.ByteString'
-- @
lrjedJSON
  ∷ Functor f
  ⇒ (LB.ByteString → f LB.ByteString)
  → LambdaResponseJsonErrorData
  → f LambdaResponseJsonErrorData
lrjedJSON i LambdaResponseJsonErrorData{..} =
  (\_lrjedJSON → LambdaResponseJsonErrorData{..})
    <$> i _lrjedJSON

data LambdaErrorResponse
  = LambdaResponseJsonError LambdaResponseJsonErrorData
  | LambdaErrorResponse LambdaErrorResponseData
  | LambdaOtherError LambdaOtherErrorData
  deriving (Eq, Show, Typeable)

-- | A prism for 'LambdaResponseJsonError'.
--
-- @
-- '_LambdaResponseJsonError' ∷ Prism' 'LambdaErrorResponse' 'LambdaResponseJsonErrorData'
-- @
_LambdaResponseJsonError
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p LambdaResponseJsonErrorData (f LambdaResponseJsonErrorData)
  → p LambdaErrorResponse (f LambdaErrorResponse)
_LambdaResponseJsonError =
  dimap to fro ∘ right'
    where
      to = \case
        LambdaResponseJsonError e → Right e
        e → Left e
      fro = either pure (fmap LambdaResponseJsonError)
{-# INLINE _LambdaResponseJsonError #-}

-- | A prism for 'LambdaErrorResponse'.
--
-- @
-- '_LambdaErrorResponse' ∷ Prism' 'LambdaErrorResponse' 'LambdaErrorResponseData'
-- @
_LambdaErrorResponse
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p LambdaErrorResponseData (f LambdaErrorResponseData)
  → p LambdaErrorResponse (f LambdaErrorResponse)
_LambdaErrorResponse =
  dimap to fro ∘ right'
    where
      to = \case
        LambdaErrorResponse e → Right e
        e → Left e
      fro = either pure (fmap LambdaErrorResponse)
{-# INLINE _LambdaErrorResponse #-}

-- | A prism for 'LambdaOtherError'.
--
-- @
-- '_LambdaOtherError' ∷ Prism' 'LambdaErrorResponse' 'LambdaOtherErrorData'
-- @
_LambdaOtherError
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p LambdaOtherErrorData (f LambdaOtherErrorData)
  → p LambdaErrorResponse (f LambdaErrorResponse)
_LambdaOtherError =
  dimap to fro ∘ right'
    where
      to = \case
        LambdaOtherError e → Right e
        e → Left e
      fro = either pure (fmap LambdaOtherError)
{-# INLINE _LambdaOtherError #-}

instance Exception LambdaErrorResponse

-- | This instance captures only the 'LambdaErrorResponse' constructor
--
instance AE.FromJSON LambdaErrorResponse where
  parseJSON =
    AE.withObject "LambdaErrorResponse" $ \o →
      fmap LambdaErrorResponse $
        pure LambdaErrorResponseData
          ⊛ o .: "__type"
          ⊛ o .: "message"


-- | Create a complete 'HTTPResponseConsumer' for response types with an
-- 'AE.FromJSON' instance
--
jsonResponseConsumer
  ∷ AE.FromJSON α
  ⇒ HTTPResponseConsumer α
jsonResponseConsumer res = do
  doc ← HTTP.responseBody res $$+- sinkLbs
  case AE.eitherDecode (if doc ≡ mempty then "{}" else doc) of
    Left e → throwM $ LambdaResponseJsonError LambdaResponseJsonErrorData
      { _lrjedMessage = T.pack e
      , _lrjedJSON = doc
      }
    Right v → return v


lambdaResponseConsumer
  ∷ AE.FromJSON a
  ⇒ IORef LambdaMetadata
  → HTTPResponseConsumer a
lambdaResponseConsumer metadata resp = do

  let headerString = fmap T.decodeUtf8 ∘ flip lookup (HTTP.responseHeaders resp)
      requestId = headerString "x-amz-request-id"

  liftIO $ tellMetadataRef metadata LambdaMetadata
    { _lmdRequestId = requestId
    }

  if HTTP.responseStatus resp ≥ HTTP.status400
    then errorResponseConsumer resp
    else jsonResponseConsumer resp

errorResponseConsumer ∷ HTTPResponseConsumer a
errorResponseConsumer resp = do
  doc ← HTTP.responseBody resp $$+- sinkLbs
  if HTTP.responseStatus resp ≡ HTTP.status400
    then kinesisError doc
    else throwM $ LambdaOtherError LambdaOtherErrorData
        { _loedStatus = HTTP.responseStatus resp
        , _loedMessage = T.decodeUtf8 $ LB.toStrict doc
        }
  where
    kinesisError doc =
      case AE.eitherDecode doc of
        Left e → throwM $ LambdaResponseJsonError LambdaResponseJsonErrorData
          { _lrjedMessage = T.pack e
          , _lrjedJSON = doc
          }
        Right a → throwM (a ∷ LambdaErrorResponse)

