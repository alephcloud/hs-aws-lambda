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

  -- ** Lenses
, ptText

  -- * Event Source Configuration
, EventSourceConfiguration(..)
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

  -- ** Prisms
, _EventSourceStatusPending
, _EventSourceStatusOk
, _EventSourceStatusProblem
) where

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens
import Data.Aeson
import Data.Monoid.Unicode
import qualified Data.Map as M
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

data EventSourceConfiguration
  = EventSourceConfiguration
  { _escBatchSize ∷ !(Maybe Int)
  , _escEventSource ∷ !(Maybe T.Text)
  , _escFunctionName ∷ !(Maybe T.Text)
  , _escIsActive ∷ !(Maybe Bool)
  , _escLastModified ∷ !(Maybe UTCTime)
  , _escParameters ∷ !(Maybe (M.Map T.Text T.Text))
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

