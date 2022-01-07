-- |
--
-- Copyright:
--   This file is part of the package turtle. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/turtle
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module Main (main) where

import qualified Text.Megaparsec as Parse
import qualified Text.Megaparsec.Char as Parse
import qualified Text.Megaparsec.Char.Lexer as Lex

-- | Drawing directions.
data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord)

-- | Commands in the turtle mini-language.
data Command
  = PenDown
  | PenUp
  | SelectPen Int
  | Draw Direction Int
  deriving (Show, Eq, Ord)

-- | Specialize the megaparsec 'Parsec' type so that it uses 'Text'
-- and doesn't use any special error type.
type Parser a = Parse.Parsec Void Text a

-- | The turtle mini-language parser.
parser :: Parser [Command]
parser = many go <&> catMaybes
  where
    go :: Parser (Maybe Command)
    go =
      Parse.choice
        [ command <&> Just,
          comment $> Nothing,
          Parse.space1 $> Nothing
        ]

    -- Expect the given character followed by a number.
    charNum :: Char -> Parser Int
    charNum c = Parse.char c *> Parse.space *> Lex.decimal

    command :: Parser Command
    command =
      Parse.choice
        [ Parse.char 'D' $> PenDown,
          Parse.char 'U' $> PenUp,
          charNum 'P' <&> SelectPen,
          charNum 'N' <&> Draw North,
          charNum 'E' <&> Draw East,
          charNum 'S' <&> Draw South,
          charNum 'W' <&> Draw West
        ]

    comment :: Parser ()
    comment = Lex.skipLineComment "#" <* Parse.eol

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs

  file <- case args of
    [] -> die "please give the name of a file"
    file : _ -> pure file

  contents <- readFileText file

  case Parse.parse (parser <* Parse.eof) file contents of
    Left err -> die (Parse.errorBundlePretty err)
    Right cs -> mapM_ print cs
