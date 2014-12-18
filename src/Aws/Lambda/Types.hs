-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Types
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Types
( -- * Abstract types
  PaginationToken
, LambdaUuid
, ConfigurationId

  -- ** Lenses
, ptText
, cidText
, luText

  -- * Event Source Configuration
, EventSourceConfiguration(..)
, EventSourceParameters(..)
, EventSourceStatus

  -- ** Lenses
, escBatchSize
, escEventSource
, escFunctionName
, escIsActive
, escLastModified
, escParameters
, escRole
, escStatus
, escUuid
, espInitialPositionInStream

  -- ** Prisms
, _EventSourceStatusPending
, _EventSourceStatusOk
, _EventSourceStatusProblem

  -- * Function Configuration
, FunctionConfiguration(..)
, FunctionMode(..)
, FunctionRuntime(..)

-- ** Lenses
, fcCodeSize
, fcConfigurationId
, fcDescription
, fcFunctionArn
, fcFunctionName
, fcHandler
, fcLastModified
, fcMemorySize
, fcMode
, fcRole
, fcRuntime
, fcTimeout

  -- ** Prisms
, _FunctionModeEvent
, _FunctionRuntimeNodeJs

-- * Misc
, StreamPosition(..)
, _StreamPositionTrimHorizon
, _StreamPositionLatest
, FunctionCodeLocation(..)
, fclLocation
, fclRepositoryType
) where

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Monoid.Unicode
import qualified Data.Text as T
import Data.Time
import Prelude.Unicode

newtype PaginationToken
  = PaginationToken
  { _ptText ∷ T.Text
  } deriving (Eq, Show)

makeLenses ''PaginationToken

instance FromJSON PaginationToken where
  parseJSON =
    withText "PaginationToken" $
      pure ∘ PaginationToken

instance ToJSON PaginationToken where
  toJSON = String ∘ _ptText

newtype LambdaUuid
  = LambdaUuid
  { _luText ∷ T.Text
  } deriving (Eq, Show)

makeLenses ''LambdaUuid

instance FromJSON LambdaUuid where
  parseJSON =
    withText "LambdaUuid" $
      pure ∘ LambdaUuid

instance ToJSON LambdaUuid where
  toJSON = String ∘ _luText


data EventSourceStatus
  = EventSourceStatusPending
  | EventSourceStatusOk
  | EventSourceStatusProblem T.Text
  deriving (Eq, Show)

makePrisms ''EventSourceStatus

instance FromJSON EventSourceStatus where
  parseJSON =
    withText "EventSourceStatus" $ \txt →
      case (T.unpack txt) of
        "PENDING" → pure EventSourceStatusPending
        "OK" → pure EventSourceStatusOk
        'P':'R':'O':'B':'L':'E':'M':':':msg → pure ∘ EventSourceStatusProblem $ T.pack msg
        st → fail $ "Invalid EventSourceStatus: " ⊕ st

data StreamPosition
  = StreamPositionTrimHorizon
  | StreamPositionLatest
  deriving (Eq, Show)

makePrisms ''StreamPosition

instance FromJSON StreamPosition where
  parseJSON = \case
    String "TRIM_HORIZON" → return StreamPositionTrimHorizon
    String "LATEST" → return StreamPositionLatest
    xs → fail $ "Invalid StreamPosition: " ++ show xs

instance ToJSON StreamPosition where
  toJSON = \case
    StreamPositionTrimHorizon → "TRIM_HORIZON"
    StreamPositionLatest → "LATEST"

data EventSourceParameters
  = EventSourceParameters
  { _espInitialPositionInStream ∷ !(Maybe StreamPosition)
  } deriving (Eq, Show)

makeLenses ''EventSourceParameters

instance FromJSON EventSourceParameters where
  parseJSON =
    withObject "EventSourceParameters" $ \o →
      pure EventSourceParameters
        ⊛ o .:? "InitialPositionInStream"

instance ToJSON EventSourceParameters where
  toJSON EventSourceParameters{..} = object
    [ "InitialPositionInStream" .= _espInitialPositionInStream
    ]

data EventSourceConfiguration
  = EventSourceConfiguration
  { _escBatchSize ∷ !(Maybe Int)
  , _escEventSource ∷ !(Maybe T.Text)
  , _escFunctionName ∷ !(Maybe T.Text)
  , _escIsActive ∷ !(Maybe Bool)
  , _escLastModified ∷ !(Maybe UTCTime)
  , _escParameters ∷ !(Maybe EventSourceParameters)
  , _escRole ∷ !(Maybe T.Text)
  , _escStatus ∷ !(Maybe EventSourceStatus)
  , _escUuid ∷ !(Maybe LambdaUuid)
  } deriving (Eq, Show)

makeLenses ''EventSourceConfiguration

instance FromJSON EventSourceConfiguration where
  parseJSON =
    withObject "EventSourceConfiguration" $ \o →
      pure EventSourceConfiguration
        ⊛ o .:? "BatchSize"
        ⊛ o .:? "EventSource"
        ⊛ o .:? "FunctionName"
        ⊛ o .:? "IsActive"
        ⊛ o .:? "LastModified"
        ⊛ o .:? "Parameters"
        ⊛ o .:? "Role"
        ⊛ o .:? "Status"
        ⊛ o .:? "Uuid"

newtype ConfigurationId
  = ConfigurationId
  { _cidText ∷ T.Text
  } deriving (Eq, Show)

makeLenses ''ConfigurationId

instance FromJSON ConfigurationId where
  parseJSON =
    withText "ConfigurationId" $
      pure ∘ ConfigurationId

instance ToJSON ConfigurationId where
  toJSON = String ∘ _cidText

data FunctionMode
  = FunctionModeEvent
  deriving (Eq, Show)

makePrisms ''FunctionMode

instance FromJSON FunctionMode where
  parseJSON (String "event") = return FunctionModeEvent
  parseJSON xs = fail $ "Invalid FunctionMode: " ++ show xs

data FunctionRuntime
  = FunctionRuntimeNodeJs
  deriving (Eq, Show)

makePrisms ''FunctionRuntime

instance FromJSON FunctionRuntime where
  parseJSON = \case
    String "nodejs" → return FunctionRuntimeNodeJs
    xs → fail $ "Invalid FunctionRuntime: " ++ show xs

data FunctionConfiguration
  = FunctionConfiguration
  { _fcCodeSize ∷ !(Maybe Integer)
  , _fcConfigurationId ∷ !(Maybe ConfigurationId)
  , _fcDescription ∷ !(Maybe T.Text)
  , _fcFunctionArn ∷ !(Maybe T.Text)
  , _fcFunctionName ∷ !(Maybe T.Text)
  , _fcHandler ∷ !(Maybe T.Text)
  , _fcLastModified ∷ !(Maybe UTCTime) -- TODO: make sure this parses right
  , _fcMemorySize ∷ !(Maybe Int)
  , _fcMode ∷ !(Maybe FunctionMode)
  , _fcRole ∷ !(Maybe T.Text)
  , _fcRuntime ∷ !(Maybe FunctionRuntime)
  , _fcTimeout ∷ !(Maybe Int)
  } deriving (Eq, Show)

makeLenses ''FunctionConfiguration

instance FromJSON FunctionConfiguration where
  parseJSON =
    withObject "FunctionConfiguration" $ \o →
      pure FunctionConfiguration
        ⊛ o .:? "CodeSize"
        ⊛ o .:? "ConfigurationId"
        ⊛ o .:? "Description"
        ⊛ o .:? "FunctionARN"
        ⊛ o .:? "FunctionName"
        ⊛ o .:? "Handler"
        ⊛ o .:? "LastModified"
        ⊛ o .:? "MemorySize"
        ⊛ o .:? "Mode"
        ⊛ o .:? "Role"
        ⊛ o .:? "Runtime"
        ⊛ o .:? "Timeout"


data FunctionCodeLocation
  = FunctionCodeLocation
  { _fclLocation ∷ !(Maybe T.Text)
    -- ^ The presigned URL you can use to download the function's @.zip@ file
    -- that you previously uploaded. The URL is valid for up to 10 minutes.

  , _fclRepositoryType ∷ !(Maybe T.Text)
    -- ^ The repository from which you can download the function.
  } deriving (Eq, Show)

makeLenses ''FunctionCodeLocation

instance FromJSON FunctionCodeLocation where
  parseJSON =
    withObject "FunctionCodeLocation" $ \o →
      pure FunctionCodeLocation
        ⊛ o .:? "Location"
        ⊛ o .:? "RepositoryType"
