{-# LANGUAGE LambdaCase #-}

module HsBlog where
( main
, process
)
where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    [] -> do
      content <- getContents
      let html = process "No title" content
      putStrLn html

    [input, output] -> do
      content <- readFile input
      exists <- doesFileExist output
      let html = process input content
          writeResult = writeFile output html
      if exists
        then whenIO confirm writeResult
        else writeResult

    _ -> putStrLn "Usage: runhaskell Main.hs <input> <output>"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> putStrLn "Invalid response. use y or n" *> confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond 
  if result
    then action
    else pure ()
