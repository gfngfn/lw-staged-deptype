module Util.FrontError
  ( FrontError (..),
  )
where

import Util.ParserUtil (ParseError)
import Prelude

data FrontError
  = FrontLexingError String
  | FrontParseError [ParseError]
  deriving stock (Eq, Show)
