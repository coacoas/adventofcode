{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day1 where

import qualified Data.Text as T
import qualified Data.Set as Set
import Data.List (dropWhile, head, scanl)
import Debug.Trace

shift :: [Integer] -> Integer
shift = sum

loop :: [Integer] -> Integer
loop xs =
  let
    looping = cycle xs
    initialHistory = (Set.empty, 0)
    store (history, shift) item = (Set.insert shift history, shift + item)
    accumulatingHistory = scanl store initialHistory looping
    notRepeating =  not . (uncurry $ flip Set.member)
    (_, result) = (head . dropWhile notRepeating) accumulatingHistory
  in
    result
  

strip :: String -> String
strip = T.unpack . T.strip . T.pack

stripPlus :: String -> String
stripPlus = dropWhile (== '+')

fixInput :: [String] -> [Integer]
fixInput xs = fst <$> ((stripPlus <$> strip <$> xs) >>= reads)
