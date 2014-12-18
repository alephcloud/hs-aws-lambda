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
, AddEventSourceParameters(..)
, addEventSource

  -- ** Lenses
, aesBatchSize
, aesEventSource
, aesFunctionName
, aesParameters
, aesRole
, aespInitialPositionInStream

  -- * Response
, AddEventSourceResponse(..)

  -- ** Lenses
, aesrBatchSize
, aesrEventSource
, aesrFunctionName
, aesrParameters
, aesrRole
, aesrStatus
, aesrIsActive
, aesrLastModified
, aesrUuid
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Types

data AddEventSourceParameters
  = AddEventSourceParameters
  { _aespInitialPositionInStream ∷ !(Maybe StreamPosition)
  } deriving (Eq, Show)

makeLenses ''AddEventSourceParameters

instance FromJSON AddEventSourceParameters where
  parseJSON =
    withObject "AddEventSourceParameters" $ \o →
      pure AddEventSourceParameters
        ⊛ o .:? "InitialPositionInStream"

instance ToJSON AddEventSourceParameters where
  toJSON AddEventSourceParameters{..} = object
    [ "InitialPositionInStream" .= _aespInitialPositionInStream
    ]

data AddEventSource
  = AddEventSource
  { _aesBatchSize ∷ !(Maybe Integer)
  , _aesEventSource ∷ !T.Text
  , _aesFunctionName ∷ !T.Text
  , _aesParameters ∷ !(Maybe AddEventSourceParameters)
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
  { _aesrBatchSize ∷ !(Maybe Int)
  , _aesrEventSource ∷ !T.Text
  , _aesrFunctionName ∷ !T.Text
  , _aesrIsActive ∷ !Bool
  , _aesrLastModified ∷ !UTCTime
  , _aesrParameters ∷ !(Maybe AddEventSourceParameters)
  , _aesrRole ∷ !T.Text
  , _aesrStatus ∷ !EventSourceStatus
  , _aesrUuid ∷ !LambdaUuid
  } deriving (Eq, Show)

makeLenses ''AddEventSourceResponse

instance FromJSON AddEventSourceResponse where
  parseJSON =
    withObject "AddEventSourceResponse" $ \o →
      pure AddEventSourceResponse
        ⊛ o .:? "BatchSize"
        ⊛ o .: "EventSource"
        ⊛ o .: "FunctionName"
        ⊛ o .: "IsActive"
        ⊛ o .: "LastModified"
        ⊛ o .:? "Parameters"
        ⊛ o .: "Role"
        ⊛ o .: "Status"
        ⊛ o .: "UUID"

instance LambdaTransaction AddEventSource AddEventSourceResponse where
  buildQuery aes =
    lambdaQuery POST "event-source-mappings"
      & lqBody ?~ toJSON aes
