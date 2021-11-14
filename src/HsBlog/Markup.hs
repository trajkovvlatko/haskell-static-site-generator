module HsBlog.Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Numeric.Natural
import Data.Maybe

type Document = [Structure]

data Structure
  = Header Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

parse :: String -> [Structure]
parse text = parseLines Nothing $ lines text

parseLines :: Maybe Structure -> [String] -> [Structure]
parseLines context textLines =
  case textLines of
    [] ->
      maybeToList context

    -- Header 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Header 1 (trim line) : parseLines Nothing rest)

    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)


trim :: String -> String
trim = unwords . words
