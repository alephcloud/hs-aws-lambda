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
, ufRawCode
, ufLastModified
) where

import Aws.Lambda.Core
import Aws.Lambda.Types

import qualified Codec.Archive.Zip as Z
import Control.Lens hiding ((<.>))
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Types
import Prelude.Unicode
import System.FilePath

-- | Creates a new Lambda function or updates an existing function. The
-- function metadata is created from the request parameters, and the code for
-- the function is provided by a @.zip@ file in the request body. If the
-- function name already exists, the existing Lambda function is updated with
-- the new code and metadata.
--
-- This operation requires permission for the @lambda:UploadFunction@ action.
--
data UploadFunction
  = UploadFunction
  { _ufDescription ∷ !T.Text
  -- ^ A short, user-defined function description. Lambda does not use this
  -- value. Assign a meaningful description as you see fit.

  , _ufFunctionName ∷ !T.Text
  -- ^ The name you want to assign to the function you are uploading.

  , _ufHandler ∷ !T.Text
  -- ^ The function that Lambda calls to begin execution. For Node.js, it is
  -- the @module-name.export@ value in your function

  , _ufMemorySize ∷ !Int
  -- ^ The amount of memory, in MB, your Lambda function is given. Lambda uses
  -- this memory size to infer the amount of CPU allocated to your function.
  -- Your function use-case determines your CPU and memory requirements. For
  -- example, database operation might need less memory compared to image
  -- processing function. The default value is 128 MB. The value must be a
  -- multiple of 64 MB.

  , _ufMode ∷ !FunctionMode
  -- ^ How the Lambda function will be invoked.

  , _ufRole ∷ !Arn
  -- ^ The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when
  -- it executes your function to access any other Amazon Web Services (AWS)
  -- resources.

  , _ufRuntime ∷ !FunctionRuntime
  -- ^ The runtime environment for the Lambda function you are uploading.

  , _ufTimeout ∷ !Int
  -- ^ The function execution time at which Lambda should terminate the
  -- function. Because the execution time has cost implications, we recommend
  -- you set this value based on your expected execution time. The default is 3
  -- seconds.

  , _ufRawCode ∷ !B.ByteString
  -- ^ The raw code of the function (which will be packaged into a ZIP archive
  -- automatically).

  , _ufLastModified ∷ !UTCTime
  -- ^ The last-modified date to be assigned to the file in the ZIP archive.
  } deriving (Eq, Show)

makeLenses ''UploadFunction

newtype UploadFunctionResponse
  = UploadFunctionResponse
  { _ufrConfiguration ∷ FunctionConfiguration
  } deriving (Eq, Show, FromJSON)

instance LambdaTransaction UploadFunction B.ByteString UploadFunctionResponse where
  buildQuery uf =
    lambdaQuery' PUT ["functions", uf ^. ufFunctionName] archivedSource
      & lqParams
        %~ (at "Description" ?~ uf ^. ufDescription)
         ∘ (at "Handler" ?~ uf ^. ufHandler)
         ∘ (at "MemorySize" ?~ uf ^. ufMemorySize ∘ to (T.pack ∘ show))
         ∘ (at "Mode" ?~ uf ^. ufMode ∘ re _TextFunctionMode)
         ∘ (at "Role" ?~ uf ^. ufRole ∘ to arnToText)
         ∘ (at "Runtime" ?~ uf ^. ufRuntime ∘ re _TextFunctionRuntime)
         ∘ (at "Timeout" ?~ uf ^. ufTimeout ∘ to (T.pack ∘ show))
    where
      extension =
        case uf ^. ufRuntime of
          FunctionRuntimeNodeJs → "js"

      archivedSource =
        LB.toStrict ∘ Z.fromArchive ∘ flip Z.addEntryToArchive Z.emptyArchive $
          Z.toEntry
            (uf ^. ufFunctionName ∘ to T.unpack <.> extension)
            (uf ^. ufLastModified ∘ to (round ∘ utcTimeToPOSIXSeconds))
            (uf ^. ufRawCode ∘ to LB.fromStrict)

