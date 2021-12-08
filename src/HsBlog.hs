module HsBlog
  ( convertSingle
  , convertDirectory
  , process
  , buildIndex
  )
where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html.Internal as Html ( render, Title )
import HsBlog.Convert (convert)
import HsBlog.Directory (convertDirectory, buildIndex)

import System.IO ( hPutStrLn, hGetContents, Handle )

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: Html.Title -> String -> String
process title content =
  let markup = Markup.parse content
      html = convert title markup
  in
    Html.render html
