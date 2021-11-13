module Markup
  ( Document
  , Structure(..)
  )
where

import Numeric.Natural

type Document = [Structure]

data Structure
  = Header Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

parse :: String -> Document
parse text = parseLines [] $ lines text

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph textLines =
  let
    paragraph = Paragraph (unlines(reverse currentParagraph))
  in
    case textLines of
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest
          else
            parseLines (currentLine:currentParagraph) rest

trim :: String -> String
trim = unwords . words
