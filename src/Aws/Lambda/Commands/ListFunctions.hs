-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.ListFunctions
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

module Aws.Lambda.Commands.ListFunctions
( -- * Requset
  ListFunctions(..)
, listFunctions
  -- ** Lenses
, lfMarker
, lfMaxItems

  -- * Response
, ListFunctionsResponse
  -- ** Lenses
, lfrNextMarker
, lfrFunctions
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import Control.Applicative
import Control.Applicative.Unicode
import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Types
import Prelude.Unicode


data ListFunctions
  = ListFunctions
  { _lfMarker ∷ !(Maybe PaginationToken)
  , _lfMaxItems ∷ !(Maybe Int)
  } deriving (Eq, Show)

makeLenses ''ListFunctions

-- | A minimal 'ListFunctions' request.
--
listFunctions ∷ ListFunctions
listFunctions = ListFunctions
  { _lfMarker = Nothing
  , _lfMaxItems = Nothing
  }

data ListFunctionsResponse
  = ListFunctionsResponse
  { _lfrFunctions ∷ ![FunctionConfiguration]
  , _lfrNextMarker ∷ !(Maybe PaginationToken)
  } deriving (Eq, Show)

makeLenses ''ListFunctionsResponse

instance FromJSON ListFunctionsResponse where
  parseJSON =
    withObject "ListFunctionsResponse" $ \o →
      pure ListFunctionsResponse
        ⊛ o .:? "Functions" .!= []
        ⊛ o .:? "NextMarker"

instance LambdaTransaction ListFunctions ListFunctionsResponse where
  buildQuery lf =
    lambdaQuery GET ["functions"]
      & lqParams
        %~ (at "Marker" .~ lf ^? lfMarker ∘ _Just ∘ ptText)
         ∘ (at "MaxItems" .~ lf ^? lfMaxItems ∘ _Just ∘ to (T.pack ∘ show))

instance PagedLambdaTransaction ListFunctions ListFunctionsResponse PaginationToken [FunctionConfiguration] where
  requestCursor = lfMarker
  responseCursor = lfrNextMarker
  responseAccum = lfrFunctions

