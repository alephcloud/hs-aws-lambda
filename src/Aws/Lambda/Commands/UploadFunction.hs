-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.UploadFunction
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

module Aws.Lambda.Commands.UploadFunction
( -- * Request
  UploadFunction(..)
  -- ** Lenses
, ufDescription
, ufFunctionName
, ufHandler
, ufMemorySize
, ufMode
, ufRole
, ufRuntime
, ufTimeout
, ufCodeBundle
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Lens
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.HTTP.Types
import Prelude.Unicode

data UploadFunction
  = UploadFunction
  { _ufDescription ∷ !T.Text
  , _ufFunctionName ∷ !T.Text
  , _ufHandler ∷ !T.Text
  , _ufMemorySize ∷ !Int
  , _ufMode ∷ !FunctionMode
  , _ufRole ∷ !T.Text
  , _ufRuntime ∷ !FunctionRuntime
  , _ufTimeout ∷ !Int
  , _ufCodeBundle ∷ !B.ByteString
  } deriving (Eq, Show)

makeLenses ''UploadFunction

newtype UploadFunctionResponse
  = UploadFunctionResponse
  { _ufrConfiguration ∷ FunctionConfiguration
  } deriving (Eq, Show, FromJSON)

instance LambdaTransaction UploadFunction B.ByteString UploadFunctionResponse where
  buildQuery uf =
    lambdaQuery' PUT ["functions", uf ^. ufFunctionName] (uf ^. ufCodeBundle)
      & lqParams
        %~ (at "Description" ?~ uf ^. ufDescription)
         ∘ (at "Handler" ?~ uf ^. ufHandler)
         ∘ (at "MemorySize" ?~ uf ^. ufMemorySize ∘ to (T.pack ∘ show))
         ∘ (at "Mode" ?~ uf ^. ufMode ∘ re _TextFunctionMode)
         ∘ (at "Role" ?~ uf ^. ufRole)
         ∘ (at "Runtime" ?~ uf ^. ufRuntime ∘ re _TextFunctionRuntime)
         ∘ (at "Timeout" ?~ uf ^. ufTimeout ∘ to (T.pack ∘ show))
