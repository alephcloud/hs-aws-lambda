-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.RemoveEventSource
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
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Aws.Lambda.Commands.RemoveEventSource
( -- * Request
  RemoveEventSource(..)
, removeEventSource
  -- ** Lenses
, resUuid

  -- * Response
, RemoveEventSourceResponse(..)
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Data.Aeson
import Control.Lens
import Network.HTTP.Types
import Prelude.Unicode

-- | Removes an event source mapping. This means AWS Lambda will no longer
-- invoke the function for events in the associated source.
--
-- This operation requires permission for the @lambda:RemoveEventSource@ action.
--
data RemoveEventSource
  = RemoveEventSource
  { _resUuid ∷ !LambdaUuid
  -- ^ The event source mapping ID.
  }

-- | A minimal 'RemoveEventSource' request.
--
removeEventSource
  ∷ LambdaUuid -- ^ '_resUuid'
  → RemoveEventSource
removeEventSource = RemoveEventSource

makeLenses ''RemoveEventSource

data RemoveEventSourceResponse
  = RemoveEventSourceResponse
  deriving (Eq, Show)

instance FromJSON RemoveEventSourceResponse where
  parseJSON _ = return RemoveEventSourceResponse

instance LambdaTransaction RemoveEventSource () RemoveEventSourceResponse where
  buildQuery res =
    lambdaQuery DELETE ["event-source-mappings", res ^. resUuid ∘ luText]
