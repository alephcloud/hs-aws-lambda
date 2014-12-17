-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.ListEventSources
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Aws.Lambda.Commands.ListEventSources
( -- * Request
  ListEventSources(..)
, listEventSources
  -- ** Lenses
, lesEventSourceArn
, lesFunctionName
, lesMarker
, lesMaxItems

  -- * Response
, ListEventSourcesResponse(..)
  -- ** Lenses
, lesrEventSources
, lesrNextMarker
) where

import Aws.General
import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Types
import Prelude.Unicode

-- | Returns a list of event source mappings you created using the
-- @AddEventSource@ action, where you identify a stream as event source. This
-- list does not include Amazon S3 event sources.
--
-- For each mapping, the API returns configuration information. You can
-- optionally specify filters to retrieve specific event source mappings.
--
-- This operation requires permission for the @lambda:ListEventSources@ action.
--
data ListEventSources
  = ListEventSources
  { _lesEventSourceArn ∷ !T.Text
    -- ^ The Amazon Resource Name (ARN) of the event source.

  , _lesFunctionName ∷ !T.Text
    -- ^ The name of the AWS Lambda function.

  , _lesMarker ∷ !(Maybe PaginationToken)
    -- ^ Optional, returned from previous 'ListEventSources' request; if
    -- present, specifies to continue the list from where the returning call
    -- left off.

  , _lesMaxItems ∷ !(Maybe Int)
    -- ^ Optional, maximum number of event sources to return. Length
    -- constraints: @1 ≤ n ≤ 1000@.
  } deriving (Eq, Show)

makeLenses ''ListEventSources

-- | Create a minimal 'ListEventSources' request.
--
listEventSources
  ∷ T.Text -- ^ The event source ARN
  → T.Text -- ^ The function name
  → ListEventSources
listEventSources arn func = ListEventSources
  { _lesEventSourceArn = arn
  , _lesFunctionName = func
  , _lesMarker = Nothing
  , _lesMaxItems = Nothing
  }

data ListEventSourcesResponse
  = ListEventSourcesResponse
  { _lesrEventSources ∷ ![EventSourceConfiguration]
    -- ^ A list of 'EventSourceConfiguration' objects.

  , _lesrNextMarker ∷ !(Maybe PaginationToken)
    -- ^ Present if there are more event source mappings.
  } deriving (Eq, Show)

makeLenses ''ListEventSourcesResponse

instance FromJSON ListEventSourcesResponse where
  parseJSON =
    withObject "ListEventSourcesResponse" $ \o →
      pure ListEventSourcesResponse
        ⊛ o .:? "EventSources" .!= []
        ⊛ o .:? "NextMarker"

instance LambdaTransaction ListEventSources ListEventSourcesResponse where
  buildQuery les =
    lambdaQuery GET "event-source-mappings"
      & lqParams %~
          (ix "EventSourceArn" .~ les ^. lesEventSourceArn)
        ∘ (ix "FunctionName" .~ les ^. lesFunctionName)
        ∘ (at "Marker" .~ les ^? lesMarker ∘ _Just ∘ ptText)
        ∘ (at "MaxItems" .~ les ^? lesMaxItems ∘ _Just ∘ to (T.pack ∘ show))

instance ExhaustiveLambdaTransaction ListEventSources ListEventSourcesResponse where
  type Cursor ListEventSources ListEventSourcesResponse = PaginationToken
  type Accum ListEventSources ListEventSourcesResponse = [EventSourceConfiguration]

  requestCursor = lesMarker
  responseCursor = lesrNextMarker
  responseAccum = lesrEventSources
