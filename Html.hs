module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where

newtype Html = Html String
newtype Structure = Structure String

type Title = String

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

html_ :: Title -> Structure -> Html
html_ title content =
  let bodyContent = getStructureString content
      titleContent = el "title" title
      body = el "body" bodyContent
      head = el "head" titleContent
      html = el "html" (head <> body)
  in Html html

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

append_ :: Structure -> Structure -> Structure
append_ x y = Structure (getStructureString x <> getStructureString y)

render :: Html -> String
render html =
  case html of
    Html str -> str

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
