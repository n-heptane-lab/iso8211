module Main where

import Data.S57.ASCII (pISO8211, Module(..), Directory(..))
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 (parseOnly)

main :: IO ()
main =
  do -- c <- B.readFile "ENC_ROOT/US2MI01M/US2MI01M.000"
     -- c <- B.readFile "ENC_ROOT/US4IL10M/US4IL10M.001"
     -- c <- B.readFile "ENC_ROOT/US4IL10M/US4IL10M.000"
     -- c <- B.readFile "ENC_ROOT/US4IN01M/US4IN01M.000"
     c <- B.readFile "ENC_ROOT/US5IL11M/US5IL11M.000"
     -- c <- B.readFile "ENC_ROOT/CATALOG.031"
     let r = parseOnly pISO8211 c
     print r
     case r of
       (Right m) -> print $ length (_directoryEntries (_ddrDirectory m))
       _ -> pure ()

