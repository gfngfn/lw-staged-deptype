module Util.FrontError
  ( FrontError (..),
  )
where

import Util.ParserUtil (ParseError)
import Prelude

data FrontError token
  = FrontLexingError String
  | FrontParseError [ParseError token]
  deriving stock (Eq, Show)
