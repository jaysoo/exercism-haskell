module Bob (responseFor) where

import Data.List
import Data.Char

responseFor :: String -> String
responseFor xs
  | trim xs == "" = "Fine. Be that way!"
  | isYelling xs = "Whoa, chill out!"
  | "?" `isSuffixOf` trim xs = "Sure."
  | otherwise = "Whatever."
  where trim = reverse . dropWhile isSpace . reverse
        isYelling = \s -> someAlpha s && allCaps s
        someAlpha = any isAlpha
        allCaps = all (\s -> toUpper s == s)
        