module Data.S57.ASCII where

import Control.Applicative (liftA2, liftA3)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A

-- https://github.com/freekvw/iso8211/blob/master/iso8211.py

{-
-- https://github.com/naturalatlas/node-gdal/blob/master/deps/libgdal/gdal/frmts/iso8211/teststream.out


  DDFFieldDefn:
      Tag = `0000'
      _fieldName = `1183CEL0.DDF'
      _arrayDescr = `'
      _formatControls = `'
      _data_struct_code = elementary
      _data_type_code = char_string

  DDFFieldDefn:
      Tag = `0001'
      _fieldName = `DDF RECORD IDENTIFIER'
      _arrayDescr = `'
      _formatControls = `'
      _data_struct_code = elementary
      _data_type_code = implicit_point
-}

-- 0001 - Record Identifier?

data DDRLeader = DDRLeader
  { _recordLength                 :: ByteString -- 5 characters?
  , _interchangeLevel             :: Char -- "3"
  , _leaderIdentifier             :: Char -- "L"
  , _inlineCodeExtensionIndicator :: Char -- "E"
  , _versionNumber                :: Char -- "1"
  , _applicationIndicator         :: Char -- " "
  , _fieldControlLength           :: (Char, Char) -- ("0","9")
  , _startAddressOfFieldArea      :: ByteString -- 5 characters, Start address of field area (number of bytes in leader and directory)
  , _extendedCharacterSet         :: (Char, Char, Char) -- (" ", "!", " ")
  , _entryMap                     :: EntryMap
  }
  deriving (Show)

pDDRLeader :: Parser DDRLeader
pDDRLeader =
  do rl  <- A.take 5
     il  <- A.char '3'
     li  <- A.char 'L'
     ice <- A.char 'E'
     vn  <- A.char '1'
     ai  <- A.char ' '
     fcl <- liftA2 (,) (A.char '0') (A.char '9')
     sa  <- A.take 5
     ecs <- liftA3 (,,) (A.char ' ') (A.char '!') (A.char ' ')
     em  <- pEntryMap
     pure $ DDRLeader rl il li ice vn ai fcl sa ecs em

data EntryMap = EntryMap
  { _sizeOfFieldLength   :: Char -- "1" - "9"
  , _sizeOfFieldPosition :: Char -- "1" - "9"
  , _reserved            :: Char -- "0"
  , _sizeOfFieldTag      :: Char -- "4"
  }
  deriving (Show)

pEntryMap :: Parser EntryMap
pEntryMap =
  do sfl <- A.digit
     sfp <- A.digit
     res <- A.char '0'
     sft <- A.char '4'
     pure $ EntryMap sfl sfp res sft
{-

The first field of the DDR is the field control field.  The field tag for the field control field is "0000".

The data description for the "0001" field is mandatory for use in an S-57 conforming file

Directory is [(field tag, field length, field position)]
-}
type FieldTag = String

data DirectoryEntry = DirectoryEntry
  { _fieldTag      :: FieldTag
  , _fieldLength   :: ByteString
  , _fieldPosition :: ByteString
  }

pISO8211 = pDDRLeader

