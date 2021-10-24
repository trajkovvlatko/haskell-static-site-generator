module Html.Internal where

-- Types

newtype Html = Html String

newtype Structure = Structure String

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

append_ :: Structure -> Structure -> Structure
append_ x y = Structure (getStructureString x <> getStructureString y)

-- Rendering

render :: Html -> String
render html =
  case html of
    Html str -> str
