module HsBlog.Html.Internal where

import Numeric.Natural

-- Types

newtype Html = Html String

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) x y = Structure (getStructureString x <> getStructureString y)

instance Monoid Structure where
  mempty = empty_

type Title = String

-- Utilities

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escapeString :: Char -> String
escapeString c =
  case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    _ -> [c]

escape :: String -> String
escape =
  concatMap escapeString

-- EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  let bodyContent = getStructureString content
      titleContent = el "title" (escape title)
      body = el "body" bodyContent
      head = el "head" titleContent
      html = el "html" (head <> body)
  in Html html

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

h_ :: Natural -> String -> Structure
h_ level = Structure . el ("h" ++ show level) . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

li_ :: Structure -> String
li_ = el "li" . getStructureString

ul_ :: [Structure] -> Structure
ul_ list = Structure . el "ul" $ concatMap li_ list

ol_ :: [Structure] -> Structure
ol_ list = Structure . el "ol" $ concatMap li_ list

empty_ :: Structure
empty_ = Structure ""

-- Rendering

concatStructure :: [Structure] -> Structure
concatStructure list =
  case list of
    [] -> empty_
    x : xs -> x <> concatStructure list

render :: Html -> String
render html =
  case html of
    Html str -> str
