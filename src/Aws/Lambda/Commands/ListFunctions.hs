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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Commands.ListFunctions
( -- * Request
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

-- | Returns a list of your Lambda functions. For each function, the response
-- includes the function configuration information. You must use @GetFunction@ to
-- retrieve the code for your function.
--
-- This operation requires permission for the @lambda:ListFunctions@ action.
--

data ListFunctions
  = ListFunctions
  { _lfMarker ∷ !(Maybe PaginationToken)
  -- ^  An opaque pagination token returned from a previous 'ListFunctions'
  -- operation. If present, indicates where to continue the listing.

  , _lfMaxItems ∷ !(Maybe Int)
  -- ^ Specifies the maximum number of AWS Lambda functions to return in
  -- response. This parameter value must be greater than @0@ and less than
  -- @10000@.
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
  -- ^ A list of Lambda functions.

  , _lfrNextMarker ∷ !(Maybe PaginationToken)
  -- ^ A token whose presence indicates that more functions are available.
  } deriving (Eq, Show)

makeLenses ''ListFunctionsResponse

instance FromJSON ListFunctionsResponse where
  parseJSON =
    withObject "ListFunctionsResponse" $ \o →
      pure ListFunctionsResponse
        ⊛ o .:? "Functions" .!= []
        ⊛ o .:? "NextMarker"

instance LambdaTransaction ListFunctions () ListFunctionsResponse where
  buildQuery lf =
    lambdaQuery GET ["functions"]
      & lqParams
        %~ (at "Marker" .~ lf ^? lfMarker ∘ _Just ∘ ptText)
         ∘ (at "MaxItems" .~ lf ^? lfMaxItems ∘ _Just ∘ to (T.pack ∘ show))

instance PagedLambdaTransaction ListFunctions () ListFunctionsResponse PaginationToken [FunctionConfiguration] where
  requestCursor = lfMarker
  responseCursor = lfrNextMarker
  responseAccum = lfrFunctions

