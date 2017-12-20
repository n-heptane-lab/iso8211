module Main where

import Data.S57.ASCII (pISO8211)
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 (parseOnly)

main :: IO ()
main =
  do c <- B.readFile "ENC_ROOT/US2MI01M/US2MI01M.000"
     print $ parseOnly pISO8211 c
