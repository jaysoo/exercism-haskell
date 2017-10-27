module Bob (responseFor) where

import Data.List
import Data.Char

responseFor :: String -> String
responseFor xs
  | trim xs == "" = "Fine. Be that way!"
  | any isAlpha xs && all isCaps xs = "Whoa, chill out!"
  | "?" `isSuffixOf` trim xs = "Sure."
  | otherwise = "Whatever."
  where trim = reverse . dropWhile isSpace . reverse
        isCaps = (\c -> toUpper c == c)
