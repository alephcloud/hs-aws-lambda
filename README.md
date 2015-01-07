### AWS Lambda: Haskell Bindings

This initially started off as another sister-project to `aws`, `aws-kinesis`
and `aws-dynamodb-streams`, built in the same general pattern. However, once I
had implemented the core along the lines of those libraries, I realized that
there was a massive amount of difficult-to-maintain boilerplate that did not
seem to be gaining me anything.

On the other hand, I realized that it would be very easy to reuse some the core
data types from `aws-general`, but then implement the rest of the machinery
locally using `wreq`, which provides built-in support for AWS V4 signing. I am
considering this project to be an experiment, then, in providing a slightly
more modern and less verbose core to the Haskell AWS bindings. I am confident
that anything which this library does not currently do (reusing connections,
etc.) can be added easily and modularly without significantly increasing the
complexity and burden of the project.

![Travis CI Status](https://travis-ci.org/alephcloud/hs-aws-lambda.svg)
