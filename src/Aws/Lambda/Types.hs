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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
, _TextEventSourceStatus

  -- * Function Configuration
, FunctionConfiguration(..)
, FunctionMode(..)
, FunctionRuntime(..)
, functionModeToText
, functionRuntimeToText

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
, _TextFunctionMode
, _TextFunctionRuntime

-- * Misc
, StreamPosition(..)
, _StreamPositionTrimHorizon
, _StreamPositionLatest
, FunctionCodeLocation(..)
, fclLocation
, fclRepositoryType
, Arn
, arnToText
, LambdaDateTime(..)
, _LambdaDateTime
, _TextLambdaDateTime
) where

import Aws.Lambda.Internal.Utils

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens hiding ((.=))
import Control.Monad.Unicode
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens
import Data.Monoid
import Data.Monoid.Unicode
import qualified Data.Text as T
import Data.Time
import Prelude.Unicode
import System.Locale

type Arn = T.Text

arnToText
  ∷ Arn
  → T.Text
arnToText = id

newtype PaginationToken
  = PaginationToken
  { _ptText ∷ T.Text
  } deriving (Eq, Show)

ptText ∷ Getter PaginationToken T.Text
ptText = to _ptText

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

luText ∷ Getter LambdaUuid T.Text
luText = to _luText

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
    parserWithPrism "EventSourceStatus" $
      _String ∘ _TextEventSourceStatus

eventSourceStatusToText
  ∷ EventSourceStatus
  → T.Text
eventSourceStatusToText = \case
  EventSourceStatusPending → "PENDING"
  EventSourceStatusOk → "OK"
  EventSourceStatusProblem msg → "PROBLEM:" ⊕ msg

_TextEventSourceStatus ∷ Prism' T.Text EventSourceStatus
_TextEventSourceStatus =
  prism eventSourceStatusToText $ \txt →
    case (T.unpack $ T.toUpper txt) of
      "PENDING" → Right EventSourceStatusPending
      "OK" → Right EventSourceStatusOk
      'P':'R':'O':'B':'L':'E':'M':':':msg → Right ∘ EventSourceStatusProblem $ T.pack msg
      _ → Left txt

data StreamPosition
  = StreamPositionTrimHorizon
  | StreamPositionLatest
  deriving (Eq, Show)

makePrisms ''StreamPosition

instance FromJSON StreamPosition where
  parseJSON =
    parserWithPrism "StreamPosition" $
      _String ∘ _TextStreamPosition

instance ToJSON StreamPosition where
  toJSON = review $ _String ∘ _TextStreamPosition

streamPositionToText
  ∷ StreamPosition
  → T.Text
streamPositionToText = \case
  StreamPositionTrimHorizon → "TRIM_HORIZON"
  StreamPositionLatest → "LATEST"

_TextStreamPosition ∷ Prism' T.Text StreamPosition
_TextStreamPosition =
  prism streamPositionToText $ \case
    "TRIM_HORIZON" → Right StreamPositionTrimHorizon
    "LATEST" → Right StreamPositionLatest
    txt → Left txt

newtype LambdaDateTime
  = LambdaDateTime
  { _ldtUTCTime ∷ UTCTime
  } deriving (Eq, Ord, Show, ParseTime, FormatTime)

makePrisms ''LambdaDateTime

lambdaDateTimeFormat ∷ String
lambdaDateTimeFormat = "%FT%T%Q%Z"

_TextLambdaDateTime ∷ Prism' T.Text LambdaDateTime
_TextLambdaDateTime =
  prism (T.pack ∘ formatTime defaultTimeLocale lambdaDateTimeFormat) $ \txt →
    case parseTime defaultTimeLocale lambdaDateTimeFormat (T.unpack txt) of
      Just dt → Right dt
      Nothing → Left txt

instance ToJSON LambdaDateTime where
  toJSON = review $ _String ∘ _TextLambdaDateTime

instance FromJSON LambdaDateTime where
  parseJSON =
    parserWithPrism "LambdaDateTime" $
      _String ∘ _TextLambdaDateTime

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
  toJSON EventSourceParameters{..} = Object $
    mempty
      & at "InitialPositionInStream" .~ _espInitialPositionInStream ^? _Just ∘ to toJSON

data EventSourceConfiguration
  = EventSourceConfiguration
  { _escBatchSize ∷ !(Maybe Int)
  , _escEventSource ∷ !(Maybe Arn)
  , _escFunctionName ∷ !(Maybe T.Text)
  , _escIsActive ∷ !(Maybe Bool)
  , _escLastModified ∷ !(Maybe LambdaDateTime)
  , _escParameters ∷ !(Maybe EventSourceParameters)
  , _escRole ∷ !(Maybe Arn)
  , _escStatus ∷ !(Maybe EventSourceStatus)
  , _escUuid ∷ !(Maybe LambdaUuid)
  } deriving (Eq, Show)

makeLenses ''EventSourceConfiguration

-- | A kludge to get around incorrectly formatted JSON in AWS Lambda responses.
--
boolParserKludge
  ∷ Object
  → T.Text
  → Parser Bool
boolParserKludge o k = o .: k <|> (fromString =≪ o .: k)
  where
    fromString ∷ T.Text → Parser Bool
    fromString = \case
      "true" → return True
      "false" → return False
      _ → empty

instance FromJSON EventSourceConfiguration where
  parseJSON =
    withObject "EventSourceConfiguration" $ \o →
      pure EventSourceConfiguration
        ⊛ o .:? "BatchSize"
        ⊛ o .:? "EventSource"
        ⊛ o .:? "FunctionName"
        ⊛ optional (boolParserKludge o "IsActive")
        ⊛ o .:? "LastModified"
        ⊛ o .:? "Parameters"
        ⊛ o .:? "Role"
        ⊛ o .:? "Status"
        ⊛ o .:? "Uuid"

newtype ConfigurationId
  = ConfigurationId
  { _cidText ∷ T.Text
  } deriving (Eq, Show)

cidText ∷ Getter ConfigurationId T.Text
cidText = to _cidText

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
  parseJSON =
    parserWithPrism "FunctionMode" $
      _String ∘ _TextFunctionMode

functionModeToText
  ∷ FunctionMode
  → T.Text
functionModeToText = \case
  FunctionModeEvent → "event"

_TextFunctionMode ∷ Prism' T.Text FunctionMode
_TextFunctionMode =
  prism functionModeToText $ \case
    "event" → Right FunctionModeEvent
    txt → Left txt

data FunctionRuntime
  = FunctionRuntimeNodeJs
  deriving (Eq, Show)

makePrisms ''FunctionRuntime

instance FromJSON FunctionRuntime where
  parseJSON =
    parserWithPrism "FunctionRuntime" $
      _String ∘ _TextFunctionRuntime

functionRuntimeToText
  ∷ FunctionRuntime
  → T.Text
functionRuntimeToText = \case
  FunctionRuntimeNodeJs → "nodejs"

_TextFunctionRuntime ∷ Prism' T.Text FunctionRuntime
_TextFunctionRuntime =
  prism functionRuntimeToText $ \case
    "nodejs" → Right FunctionRuntimeNodeJs
    txt → Left txt

data FunctionConfiguration
  = FunctionConfiguration
  { _fcCodeSize ∷ !(Maybe Integer)
  , _fcConfigurationId ∷ !(Maybe ConfigurationId)
  , _fcDescription ∷ !(Maybe T.Text)
  , _fcFunctionArn ∷ !(Maybe Arn)
  , _fcFunctionName ∷ !(Maybe T.Text)
  , _fcHandler ∷ !(Maybe T.Text)
  , _fcLastModified ∷ !(Maybe LambdaDateTime)
  , _fcMemorySize ∷ !(Maybe Int)
  , _fcMode ∷ !(Maybe FunctionMode)
  , _fcRole ∷ !(Maybe Arn)
  , _fcRuntime ∷ !(Maybe FunctionRuntime)
  , _fcTimeout ∷ !(Maybe Int)
  } deriving (Eq, Show)

makeLenses ''FunctionConfiguration

instance FromJSON FunctionConfiguration where
  parseJSON =
    withObject "FunctionConfiguration" $ \o → do
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
