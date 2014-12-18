-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.AddEventSource
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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Commands.AddEventSource
( -- * Request
  AddEventSource(..)
, addEventSource

  -- ** Lenses
, aesBatchSize
, aesEventSource
, aesFunctionName
, aesParameters
, aesRole

  -- * Response
, AddEventSourceResponse(..)

  -- ** Lenses
, aesrConfiguration
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Types

data AddEventSource
  = AddEventSource
  { _aesBatchSize ∷ !(Maybe Integer)
  , _aesEventSource ∷ !T.Text
  , _aesFunctionName ∷ !T.Text
  , _aesParameters ∷ !(Maybe EventSourceParameters)
  , _aesRole ∷ !T.Text
  } deriving (Eq, Show)

-- | A minimal 'AddEventSource' request.
--
addEventSource
  ∷ T.Text -- ^ '_aesEventSource'
  → T.Text -- ^ '_aesFunctionName'
  → T.Text -- ^ '_aesRole'
  → AddEventSource
addEventSource es fn rl = AddEventSource
  { _aesBatchSize = Nothing
  , _aesEventSource = es
  , _aesFunctionName = fn
  , _aesParameters = Nothing
  , _aesRole = rl
  }

instance ToJSON AddEventSource where
  toJSON AddEventSource{..} = object
    [ "BatchSize" .= _aesBatchSize
    , "EventSource" .= _aesEventSource
    , "FunctionName" .= _aesFunctionName
    , "Parameters" .= _aesParameters
    , "Role" .= _aesRole
    ]

makeLenses ''AddEventSource

data AddEventSourceResponse
  = AddEventSourceResponse
  { _aesrConfiguration ∷ !EventSourceConfiguration
  } deriving (Eq, Show)

makeLenses ''AddEventSourceResponse

instance FromJSON AddEventSourceResponse where
  parseJSON =
    withObject "AddEventSourceResponse" $ \o →
      pure AddEventSourceResponse
        ⊛ parseJSON (Object o)

instance LambdaTransaction AddEventSource AddEventSourceResponse where
  buildQuery aes =
    lambdaQuery POST ["event-source-mappings"]
      & lqBody ?~ toJSON aes
