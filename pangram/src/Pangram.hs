module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram s = uniqChars s == 26
  where uniqChars = length . uniq . alphaOnly . map toUpper
        alphaOnly = filter isAlpha
        uniq = reverse . nub  .reverse
