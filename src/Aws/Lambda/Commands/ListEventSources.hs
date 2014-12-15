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
, lesrEventSources
, lesrNextMarker
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import qualified Network.Wreq as W
import Network.HTTP.Types
import Prelude.Unicode

data ListEventSources
  = ListEventSources
  { _lesEventSourceArn ∷ !T.Text
  , _lesFunctionName ∷ !T.Text
  , _lesMarker ∷ !(Maybe PaginationToken)
  , _lesMaxItems ∷ !(Maybe Int)
  } deriving (Eq, Show)

makeLenses ''ListEventSources

-- | Creates a 'ListEventSources' request.
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
  , _lesrNextMarker ∷ !(Maybe PaginationToken)
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

