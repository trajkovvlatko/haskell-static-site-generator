module HsBlog.Convert where

import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convert :: Html.Title -> [Markup.Structure] -> Html.Html
convert title doc =
  let htmlStructure = foldMap convertStructure doc
    in Html.html_ title htmlStructure

-- convert' :: Html.Title -> [Markup.Structure] -> Html.Html
-- convert' title doc = Html.html_ title (foldMap convertStructure doc)

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Header level txt -> Html.h_ level txt
    Markup.Paragraph txt -> Html.p_ txt
    Markup.CodeBlock list -> Html.code_ $ unlines list
    Markup.OrderedList list -> Html.ol_ $ map Html.p_ list
    Markup.UnorderedList list -> Html.ul_ $ map Html.p_ list

-- process :: Html.Title -> String -> String
-- process title str = Html.render(convert title (Markup.parse str))

process :: Html.Title -> String -> String
process title markup =
  let parsedMarkup = Markup.parse markup
      html = convert title parsedMarkup
    in Html.render html
