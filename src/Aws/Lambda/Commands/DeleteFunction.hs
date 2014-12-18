-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.DeleteFunction
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

module Aws.Lambda.Commands.DeleteFunction
( -- * Request
  DeleteFunction(..)
, deleteFunction
  -- ** Lenses
, dfFunctionName

  -- * Response
, DeleteFunctionResponse(..)
) where

import Aws.Lambda.Core

import Control.Lens
import Data.Aeson
import Data.Monoid.Unicode
import qualified Data.Text as T
import Network.HTTP.Types

data DeleteFunction
  = DeleteFunction
  { _dfFunctionName ∷ !T.Text
  } deriving (Eq, Show)

-- | A minimal 'DeleteFunction' request.
--
deleteFunction
  ∷ T.Text
  → DeleteFunction
deleteFunction = DeleteFunction

makeLenses ''DeleteFunction

data DeleteFunctionResponse
  = DeleteFunctionResponse
  deriving (Eq, Show)

instance FromJSON DeleteFunctionResponse where
  parseJSON _ = return DeleteFunctionResponse

instance LambdaTransaction DeleteFunction DeleteFunctionResponse where
  buildQuery df =
    lambdaQuery DELETE ("functions/" ⊕ df ^. dfFunctionName)

