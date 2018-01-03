module Main where

import Data.S57.ASCII (pISO8211, printUnknown, printDSSI, printDSPM, Module(..), Directory(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8 (parse, parseOnly, endOfInput)
-- import Text.Trifecta.Parser (parseFromFile, parseByteString)
-- import Text.Trifecta.Delta (Delta(..))

main :: IO ()
main =
  do -- c <- B.readFile "ENC_ROOT/US2MI01M/US2MI01M.000"
     -- c <- B.readFile "ENC_ROOT/US4IL10M/US4IL10M.001"
     -- c <- B.readFile "ENC_ROOT/US4IL10M/US4IL10M.000"
     -- c <- B.readFile "ENC_ROOT/US4IN01M/US4IN01M.000"
     c <- B.readFile "ENC_ROOT/US5IL11M/US5IL11M.000"
     -- c <- B.readFile "ENC_ROOT/CATALOG.031"
--     let r = parse pISO8211 c
     let r = parseOnly pISO8211 c
--     r <- parseFromFile pISO8211 "ENC_ROOT/US5IL11M/US5IL11M.000"
--     let r = parseByteString pISO8211 (Directed (UTF8.fromString "some file") 0 0 0 0) c
     case r of
       Left e -> putStrLn $ Prelude.take 1000 e
       (Right m) ->
         do printDSSI m
            printDSPM m
         {-
     case r of
       (Right m) -> print $ length (_directoryEntries (_ddrDirectory m))
       _ -> pure ()
-}
