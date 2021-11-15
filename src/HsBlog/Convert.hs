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
    Markup.Header level txt -> Html.h_ level (Html.txt_ txt)
    Markup.Paragraph txt -> Html.p_ (Html.txt_ txt)
    Markup.CodeBlock list -> Html.code_ $ unlines list
    Markup.OrderedList list -> Html.ol_ $ map (Html.p_ . Html.txt_) list
    Markup.UnorderedList list -> Html.ul_ $ map (Html.p_ . Html.txt_) list

-- process :: Html.Title -> String -> String
-- process title str = Html.render(convert title (Markup.parse str))

process :: Html.Title -> String -> String
process title markup =
  let parsedMarkup = Markup.parse markup
      html = convert title parsedMarkup
    in Html.render html

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let
    previews = map (\ (file, doc) ->
      case doc of
        Markup.Header 1 header : article ->
          Html.h_ 3 (Html.link_ file (Html.txt_ header))
            <> foldMap convertStructure (take 3 article)
            <> Html.p_ (Html.link_ file (Html.txt_ "..."))
        _ ->
          Html.h_ 3 (Html.link_ file (Html.txt_ file))
      )
      files
  in
    Html.html_
      "Index page"
      (Html.h1_ (Html.link_ "index.html" (Html.txt_ "Blog"))
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat previews
      )
