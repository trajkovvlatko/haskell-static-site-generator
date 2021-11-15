module HsBlog.Html.Internal where

import Numeric.Natural

-- Types

newtype Html = Html String
newtype Structure = Structure String
newtype Content = Content String

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
      headEl = el "head" titleContent
      html = el "html" (headEl <> body)
  in Html html

p_ :: Content -> Structure
p_ = Structure . el "p" . escape . getContentString

h1_ :: Content -> Structure
h1_ = Structure . el "h1" . escape . getContentString

h_ :: Natural -> Content -> Structure
h_ level = Structure . el ("h" ++ show level) . escape . getContentString

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

li_ :: Structure -> String
li_ = el "li" . getStructureString

ul_ :: [Structure] -> Structure
ul_ list = Structure . el "ul" $ concatMap li_ list

ol_ :: [Structure] -> Structure
ol_ list = Structure . el "ol" $ concatMap li_ list

txt_ :: String -> Content
txt_ text = Content $ escape text

link_ :: FilePath -> Content -> Content
link_ href text = Content $ "<a href='" ++ escape href ++ "'>" ++ getContentString text ++ "</a>"

bold_ :: Content -> Content
bold_ text = Content $ el "b" (getContentString text)

italic_ :: Content -> Content
italic_ text = Content $ el "i" (getContentString text)

img_ :: FilePath -> Content
img_ path = Content $ "<img src='" ++ escape path ++ "'/>"

empty_ :: Structure
empty_ = Structure ""

getContentString :: Content -> String
getContentString content =
  case content of
    Content x -> show x

-- Rendering

concatStructure :: [Structure] -> Structure
concatStructure list =
  case list of
    [] -> empty_
    x : _ -> x <> concatStructure list

render :: Html -> String
render html =
  case html of
    Html str -> str
