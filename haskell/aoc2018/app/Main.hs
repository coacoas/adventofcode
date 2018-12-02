module Main where

import Day1

main :: IO ()
main = do
  content <- readFile "data/Day1.data"
  let result = strip <$> (lines content)
      input = fixInput result
  putStrLn $ "Shift: " <> (show . shift) input
  putStrLn $ "Loop: " <> (show . loop) input
