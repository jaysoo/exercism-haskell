module RunLength (decode, encode) where

import Data.Char
import Data.List

decode :: String -> String
decode text = run "" text
  where
    run v [] = v
    run v xs =
      let a = takeWhile isNumber xs
          b = head . take 1 . drop (length a) $ xs
          c = drop (length a + 1) xs
          r | length a == 0 = [b]
            | otherwise     = replicate (read a :: Int) b
      in run (v ++ r) c

encode :: String -> String
encode s = xform $ group s
  where xform = foldl (\a b -> a ++ encodeNum b ++ fst b) ""
        encodeNum s
          | num s == "1" = ""
          | otherwise = num s
        num = show . length
        fst =  (:[]) . head