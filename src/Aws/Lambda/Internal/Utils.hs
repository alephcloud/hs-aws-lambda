-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Internal.Utils
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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Internal.Utils
( parserWithPrism
) where

import Control.Lens
import qualified Data.Aeson.Types as AE
import Data.Monoid.Unicode
import qualified Data.Text as T

parserWithPrism
  ∷ Show α
  ⇒ T.Text -- ^ A label for error messages
  → Prism' α β -- ^ A prism whence to generate a parser
  → α
  → AE.Parser β
parserWithPrism lbl p x =
  case x ^? p of
    Just v → return v
    Nothing → fail $ "Invalid " ⊕ T.unpack lbl ⊕ ": " ⊕ show x
