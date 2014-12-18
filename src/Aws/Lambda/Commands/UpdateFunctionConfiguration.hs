-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.UpdateFunctionConfiguration
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Commands.UpdateFunctionConfiguration
( -- * Request
  UpdateFunctionConfiguration
, updateFunctionConfiguration
  -- ** Lenses
, ufcConfiguration
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Data.Aeson
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Types
import Prelude.Unicode

data UpdateFunctionConfiguration
  = UpdateFunctionConfiguration
  { _ufcConfiguration ∷ !FunctionConfiguration
  , _ufcFunctionName ∷ !T.Text
  } deriving (Eq, Show)

ufcConfiguration ∷ Getter UpdateFunctionConfiguration FunctionConfiguration
ufcConfiguration = to _ufcConfiguration

ufcFunctionName ∷ Getter UpdateFunctionConfiguration T.Text
ufcFunctionName = to _ufcFunctionName

data UpdateFunctionConfigurationException
  = MissingFunctionName !FunctionConfiguration
  deriving (Eq, Show, Typeable)

instance Exception UpdateFunctionConfigurationException

-- | A minimal, validated 'UpdateFunctionConfiguration' request. Not providing
-- '_fcFunctionName' will result in an exception.
--
updateFunctionConfiguration
  ∷ MonadThrow m
  ⇒ FunctionConfiguration
  → m UpdateFunctionConfiguration
updateFunctionConfiguration fc =
  case fc ^. fcFunctionName of
    Just fn → return UpdateFunctionConfiguration
      { _ufcConfiguration = fc
      , _ufcFunctionName = fn
      }
    Nothing → throwM $ MissingFunctionName fc

newtype UpdateFunctionConfigurationResponse
  = UpdateFunctionConfigurationResponse
  { _ufcrFunctionConfiguration ∷ FunctionConfiguration
  } deriving (Eq, Show, FromJSON)

instance LambdaTransaction UpdateFunctionConfiguration UpdateFunctionConfigurationResponse where
  buildQuery ufc =
    let fc = ufc ^. ufcConfiguration
    in lambdaQuery PUT ["functions", ufc ^. ufcFunctionName, "configuration"]
      & lqParams
        %~ (at "Description" .~ fc ^. fcDescription)
         ∘ (at "Handler" .~ fc ^. fcHandler)
         ∘ (at "MemorySize" .~ fc ^? fcMemorySize ∘ _Just ∘ to (T.pack ∘ show))
         ∘ (at "Role" .~ fc ^. fcRole)
         ∘ (at "Timeout" .~ fc ^? fcTimeout ∘ _Just ∘ to (T.pack ∘ show))
