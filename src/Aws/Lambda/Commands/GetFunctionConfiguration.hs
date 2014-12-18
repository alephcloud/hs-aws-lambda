-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.GetFunctionConfiguration
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
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Aws.Lambda.Commands.GetFunctionConfiguration
( -- * Request
  GetFunctionConfiguration(..)
, getFunctionConfiguration
  -- ** Lenses
, gfcFunctionName
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Types

data GetFunctionConfiguration
  = GetFunctionConfiguration
  { _gfcFunctionName ∷ !T.Text
  } deriving (Eq, Show)

makeLenses ''GetFunctionConfiguration

getFunctionConfiguration
  ∷ T.Text -- ^ '_gfcFunctionName'
  → GetFunctionConfiguration
getFunctionConfiguration = GetFunctionConfiguration

newtype GetFunctionConfigurationResponse
  = GetFunctionConfigurationResponse
  { _gfcrFunctionConfiguration ∷ FunctionConfiguration
  } deriving (Eq, Show, FromJSON)

instance LambdaTransaction GetFunctionConfiguration GetFunctionConfigurationResponse where
  buildQuery gfc =
    lambdaQuery GET ["functions", gfc ^. gfcFunctionName, "configuration"]
