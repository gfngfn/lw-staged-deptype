module Util.LocationInFile
  ( LocationInFile (..),
    getLocationInFileFromOffset,
    SourceSpec (..),
    getSpanInFile,
  )
where

import Data.Text (Text)
import Text.Megaparsec (PosState (..), SourcePos (..))
import Text.Megaparsec.Pos qualified as MpPos
import Text.Megaparsec.Stream qualified as MpStream
import Util.TokenUtil (Span (..))
import Prelude

data LocationInFile = LocationInFile
  { line :: Int,
    column :: Int
  }
  deriving stock (Eq, Show)

getLocationInFileFromOffset :: String -> Text -> Int -> (LocationInFile, Maybe String)
getLocationInFileFromOffset inputFilePath source offset =
  let initialState =
        PosState
          { pstateInput = source,
            pstateOffset = 0,
            pstateSourcePos = MpPos.initialPos inputFilePath,
            pstateTabWidth = MpPos.defaultTabWidth,
            pstateLinePrefix = ""
          }
      (maybeLineText, finalState) = MpStream.reachOffset offset initialState
      PosState {pstateSourcePos = finalPos} = finalState
      SourcePos {sourceLine, sourceColumn} = finalPos
      locInFile =
        LocationInFile
          { line = MpPos.unPos sourceLine,
            column = MpPos.unPos sourceColumn
          }
   in (locInFile, maybeLineText)

data SourceSpec = SourceSpec
  { source :: Text,
    inputFilePath :: String
  }

getSpanInFile :: SourceSpec -> Span -> (LocationInFile, LocationInFile, Maybe String)
getSpanInFile SourceSpec {source, inputFilePath} Span {start, end} =
  (locInFileStart, locInFileEnd, maybeLineText)
  where
    (locInFileStart, maybeLineText) = getLocationInFileFromOffset inputFilePath source start
    (locInFileEnd, _) = getLocationInFileFromOffset inputFilePath source end
