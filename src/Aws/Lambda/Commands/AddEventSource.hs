-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.Lambda.Commands.AddEventSource
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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Aws.Lambda.Commands.AddEventSource
( -- * Request
  AddEventSource(..)
, addEventSource

  -- ** Lenses
, aesBatchSize
, aesEventSource
, aesFunctionName
, aesParameters
, aesRole

  -- * Response
, AddEventSourceResponse(..)

  -- ** Lenses
, aesrConfiguration
) where

import Aws.Lambda.Core
import Aws.Lambda.Constraints
import Aws.Lambda.Types

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Types
import Prelude.Unicode

-- | Identifies a stream as an event source for an AWS Lambda function. It can
-- be either an Amazon Kinesis stream or a Amazon DynamoDB stream. AWS Lambda
-- invokes the specified function when records are posted to the stream.  This
-- is the pull model, where AWS Lambda invokes the function.
--
-- This association between an Amazon Kinesis stream and an AWS Lambda function
-- is called the event source mapping. You provide the configuration
-- information (for example, which stream to read from and which AWS Lambda
-- function to invoke) for the event source mapping in the request body.
--
-- Each event source, such as a Kinesis stream, can only be associated with one
-- AWS Lambda function. If you call 'AddEventSource' for an event source that is
-- already mapped to another AWS Lambda function, the existing mapping is
-- updated to call the new function instead of the old one.
--
-- This operation requires permission for the @iam:PassRole@ action for the IAM
-- role. It also requires permission for the @lambda:AddEventSource@ action.
--
data AddEventSource
  = AddEventSource
  { _aesBatchSize ∷ !(Maybe Integer)
  -- ^ The largest number of records that AWS Lambda will give to your function
  -- in a single event. The default is 100 records.

  , _aesEventSource ∷ !T.Text
  -- ^ The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
  -- event source. Any record added to this stream causes AWS Lambda to invoke
  -- your Lambda function. AWS Lambda POSTs the Amazon Kinesis event,
  -- containing records, to your Lambda function as JSON.

  , _aesFunctionName ∷ !T.Text
  -- ^ The Lambda function to invoke when AWS Lambda detects an event on the
  -- stream.

  , _aesParameters ∷ !(Maybe EventSourceParameters)

  , _aesRole ∷ !T.Text
  -- ^ The ARN of the IAM role (invocation role) that AWS Lambda can assume to
  -- read from the stream and invoke the function.

  } deriving (Eq, Show)

-- | A minimal 'AddEventSource' request.
--
addEventSource
  ∷ T.Text -- ^ '_aesEventSource'
  → T.Text -- ^ '_aesFunctionName'
  → T.Text -- ^ '_aesRole'
  → AddEventSource
addEventSource es fn rl = AddEventSource
  { _aesBatchSize = Nothing
  , _aesEventSource = es
  , _aesFunctionName = fn
  , _aesParameters = Nothing
  , _aesRole = rl
  }

instance ToJSON AddEventSource where
  toJSON AddEventSource{..} = object
    [ "BatchSize" .= _aesBatchSize
    , "EventSource" .= _aesEventSource
    , "FunctionName" .= _aesFunctionName
    , "Parameters" .= _aesParameters
    , "Role" .= _aesRole
    ]

instance LambdaPayload AddEventSource where
  packagePayload = Pack ∘ toJSON

makeLenses ''AddEventSource

newtype AddEventSourceResponse
  = AddEventSourceResponse
  { _aesrConfiguration ∷ EventSourceConfiguration
  } deriving (Eq, Show, FromJSON)

makeLenses ''AddEventSourceResponse

instance LambdaTransaction AddEventSource AddEventSource AddEventSourceResponse where
  buildQuery aes =
    lambdaQuery' POST ["event-source-mappings"] aes
