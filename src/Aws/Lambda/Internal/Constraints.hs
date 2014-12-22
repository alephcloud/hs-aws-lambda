-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Internal.Constraints
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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Internal.Constraints
( Σ(..)
, spread
, type (⊗)
) where

import GHC.Exts (Constraint)

-- | The pointwise product of two constraint families. This sadly requires
-- @-XUndecidableInstances@.
--
class (γ α, δ α) ⇒ (γ ⊗ δ) α
instance (γ α, δ α) ⇒ (γ ⊗ δ) α

-- | The dependent sum of a constraint family.
--
data Σ (γ ∷ * → Constraint) where
  Pack
    ∷ γ α
    ⇒ α
    → Σ γ

-- | The recursion principle for the constraint family sum.
--
spread
  ∷ Σ γ
  → (∀ α. γ α ⇒ α → β)
  → β
spread (Pack x) e = e x

