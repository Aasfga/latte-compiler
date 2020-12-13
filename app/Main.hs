module Main where

import Parser.Parser ( parse )


main :: IO ()
main = do
  content <- getContents
  print $ parse content