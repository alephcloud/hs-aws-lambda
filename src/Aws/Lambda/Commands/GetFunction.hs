-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.GetFunction
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

module Aws.Lambda.Commands.GetFunction
( -- * Request
  GetFunction(..)
, getFunction
  -- ** Lenses
, gfFunctionName

  -- * Reponse
, GetFunctionResponse(..)
  -- ** Lenses
, gfrCode
, gfrConfiguration
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens
import Data.Aeson
import Network.HTTP.Types
import qualified Data.Text as T

data GetFunction
  = GetFunction
  { _gfFunctionName ∷ !T.Text
  } deriving (Eq, Show)

makeLenses ''GetFunction

-- | A minimal 'GetFunction' request.
--
getFunction
  ∷ T.Text -- ^ '_gfFunctionName'
  → GetFunction
getFunction = GetFunction

data GetFunctionResponse
  = GetFunctionResponse
  { _gfrCode ∷ !FunctionCodeLocation
  , _gfrConfiguration ∷ !FunctionConfiguration
  }

makeLenses ''GetFunctionResponse

instance FromJSON GetFunctionResponse where
  parseJSON =
    withObject "GetFunctionResponse" $ \o →
      pure GetFunctionResponse
        ⊛ o .: "Code"
        ⊛ o .: "Configuration"

instance LambdaTransaction GetFunction GetFunctionResponse where
  buildQuery gf =
    lambdaQuery GET ["functions", gf ^. gfFunctionName]

