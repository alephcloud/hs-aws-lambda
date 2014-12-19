-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.GetEventSource
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Commands.GetEventSource
( -- * Request
  GetEventSource(..)
, getEventSource
  -- ** Lenses
, gesUuid

  -- * Response
, GetEventSourceResponse(..)
  -- ** Lenses
, gesrConfiguration
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Lens
import Data.Aeson
import Network.HTTP.Types
import Prelude.Unicode

-- | Returns configuration information for the specified event source mapping.
-- This operation requires permission for the @lambda:GetEventSource@ action.
--
data GetEventSource
  = GetEventSource
  { _gesUuid ∷ !LambdaUuid
  -- ^ The AWS Lambda assigned ID of the event source mapping.
  } deriving (Eq, Show)

makeLenses ''GetEventSource

-- | A minimal 'GetEventSource' request.
--
getEventSource
  ∷ LambdaUuid -- ^ '_gesUuid'
  → GetEventSource
getEventSource = GetEventSource

newtype GetEventSourceResponse
  = GetEventSourceResponse
  { _gesrConfiguration ∷ EventSourceConfiguration
  } deriving (Eq, Show, FromJSON)

makeLenses ''GetEventSourceResponse

instance LambdaTransaction GetEventSource () GetEventSourceResponse where
  buildQuery ges =
    lambdaQuery GET ["event-source-mappings", ges ^. gesUuid ∘ luText]

