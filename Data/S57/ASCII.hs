{-# language OverloadedStrings #-}
module Data.S57.ASCII where

import Control.Applicative ((<|>), liftA2, liftA3, optional)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A

-- http://www.dtic.mil/dtic/tr/fulltext/u2/a281594.pdf
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
type FieldTag = ByteString

data DirectoryEntry = DirectoryEntry
  { _fieldTag      :: FieldTag
  , _fieldLength   :: ByteString
  , _fieldPosition :: ByteString
  }
  deriving Show

c2i :: Char -> Int
c2i '0' = 0
c2i '1' = 1
c2i '2' = 2
c2i '3' = 3
c2i '4' = 4
c2i '5' = 5
c2i '6' = 6
c2i '7' = 7
c2i '8' = 8
c2i '9' = 9

implicit2i :: [Char] -> Int
implicit2i cs = implicit2i' 0 cs
  where
    implicit2i' acc [] = acc
    implicit2i' acc (c:cs) = implicit2i' ((acc * 10) + (c2i c)) cs

pDirectoryEntry :: EntryMap -> Parser DirectoryEntry
pDirectoryEntry (EntryMap sfl sfp _ sft) =
  do ft <- A.take $ c2i sft
     fl <- A.take $ c2i sfl
     fp <- A.take $ c2i sfp
     pure $ DirectoryEntry ft fl fp

data Directory = Directory { _directoryEntries :: [DirectoryEntry] }
  deriving Show

pDirectory :: EntryMap -> Parser Directory
pDirectory entryMap =
  do des <- A.manyTill (pDirectoryEntry entryMap) ft
     pure $ Directory des


-- | unit terminator - Δ
ut :: Parser ()
ut =
  do A.char '\x1f'
     pure ()

-- | field terminator - ∇
ft :: Parser ()
ft =
  do A.char '\x1e'
     pure ()

pNull :: Parser ()
pNull =
  do A.char '\0'
     A.char '\0'
     pure ()

pFieldName :: Parser ByteString
pFieldName = A.takeTill ((==) '\x1f') <* ut

data TruncatedEscapeSequence
  = LexicalLevel0
  | LexicalLevel1
  | LexicalLevel2
  deriving Show

pTruncatedEscapeSequence :: Parser TruncatedEscapeSequence
pTruncatedEscapeSequence =
  A.choice [ A.string "   " *> pure LexicalLevel0
           , A.string "-A " *> pure LexicalLevel1
           , A.string "%/A" *> pure LexicalLevel2
           ]

data FieldControlField = FieldControlField
  { _fieldTags :: [(ByteString, ByteString)]
  }
  deriving Show

pFieldPair :: Parser (ByteString, ByteString)
pFieldPair = (,) <$> pFieldTag <*> pFieldTag

pFieldControlFieldTag :: Parser ()
pFieldControlFieldTag =
  A.string "0000" *> pure ()

pFieldControlFieldBody :: Parser FieldControlField
pFieldControlFieldBody =
  do pPrintableGraphics
     _tes <- pTruncatedEscapeSequence
     ut
     pairs <- A.manyTill pFieldPair ft
     pure $ FieldControlField pairs

pFieldControlField :: Parser FieldControlField
pFieldControlField =
  pFieldControlFieldTag *> pFieldControlFieldBody

-- | parse a 4 character field tag
--
-- No validation done, takes any 4 chars
pFieldTag :: Parser ByteString
pFieldTag = A.take 4

pPrintableGraphics :: Parser ()
pPrintableGraphics =
  A.string ";&" *> pure ()

data DataStructureCode
  = Singleton -- ? not specified in the spec
  | Linear
  | MultiDimensional
  deriving Show

pDataStructureCode :: Parser DataStructureCode
pDataStructureCode =
  A.choice [ A.char '0' *> pure Singleton
           , A.char '1' *> pure Linear
           , A.char '2' *> pure MultiDimensional
           ]


data ArrayDescriptor
  = ArrayDescriptor
  deriving Show

pArrayDescriptor :: Parser ArrayDescriptor
pArrayDescriptor =
  do A.manyTill A.anyChar ut
     pure ArrayDescriptor


-- 7.2.2.1 Data format

data DataFormat
 = A (Maybe Int)  -- character data
 | I (Maybe Int)  -- implicit point representation
 | R (Maybe Int)  -- explicit point representation
 | B Int  -- bit string
 | At
 | B1W Int  -- unsigned integer
 | B2W Int  -- signed integer
   deriving Show

pDataFormat :: Parser [DataFormat]
pDataFormat =
  A.choice (map rep [ do A.char 'A'
                         mw <- pWidth
                         pure (A mw)
                    , do A.char 'I'
                         mw <- pWidth
                         pure (I mw)
                    , do A.char 'R'
                         mw <- pWidth
                         pure (R mw)
                    , do A.char 'B'
                         w <- pWidth2
                         pure (B w)
                    , do A.string "b1"
                         i <- A.digit -- should only be 1,2 or 4
                         pure (B1W $ c2i i)
                    , do A.string "b2"
                         i <- A.digit -- should only be 1,2 or 4
                         pure (B2W $ c2i i)
                    ])
  where
    rep p =
      do c <- (c2i <$> A.digit) <|> pure 1
         r <- p
         pure $ replicate c r
    pWidth =
      do A.char '('
         i <- A.digit
         A.char ')'
         pure (Just $ c2i i)
      <|> pure Nothing
    pWidth2 =
      do A.char '('
         i10 <- A.digit
         i1  <- A.digit
         A.char ')'
         pure $ ((c2i i10) * 10) + (c2i i1)

pA :: Int -> Parser ByteString
pA l = A.take l

pI :: Int -> Parser Int
pI l = implicit2i <$> A.count l A.digit

data FormatControls = FormatControls
  { _dataFormats :: [DataFormat]
  }
  deriving Show

pFormatControls :: Parser FormatControls
pFormatControls =
  do A.char '('
     dfs <- A.sepBy pDataFormat (A.char ',')
     A.char ')'
     pure (FormatControls $ concat dfs)

data DataTypeCode
  = CharacterString
  | ImplicitPoint
  | BinaryForm
  | MixedDataTypes
    deriving Show

pDataTypeCode :: Parser DataTypeCode
pDataTypeCode =
  A.choice [ A.char '0' *> pure CharacterString
           , A.char '1' *> pure ImplicitPoint
           , A.char '5' *> pure BinaryForm
           , A.char '6' *> pure MixedDataTypes
           ]

pAuxiliaryControls :: Parser ()
pAuxiliaryControls = A.string "00" *> pure ()

data FieldControls = FieldControls
  { _dataStructureCode       :: DataStructureCode
  , _dataTypeCode            :: DataTypeCode
  , _truncatedEscapeSequence :: TruncatedEscapeSequence
  }
  deriving Show

pFieldControls :: Parser FieldControls
pFieldControls =
  do dsc <- pDataStructureCode
     dtc <- pDataTypeCode
     pAuxiliaryControls
     pPrintableGraphics
     tes <- pTruncatedEscapeSequence
     pure $ FieldControls dsc dtc tes


data DataDescriptiveField = DataDescriptiveField
  { _fieldControls    :: FieldControls
  , _fieldName        :: ByteString
  , _arrayDescription :: ArrayDescriptor
  , _formatControls   :: FormatControls
  }
  deriving Show

pDataDescriptiveField :: Parser DataDescriptiveField
pDataDescriptiveField =
  DataDescriptiveField <$> pFieldControls <*> pFieldName <*> pArrayDescriptor <*> pFormatControls <* ft

data Module = Module
  { _ddrLeader             :: DDRLeader
  , _ddrDirectory          :: Directory
  , _fieldControlField     :: FieldControlField
  , _dataDescriptiveFields :: [DataDescriptiveField]
  }
  deriving Show

pModule :: Parser Module
pModule =
  do l <- pDDRLeader
     d <- pDirectory (_entryMap l)
     fcf <- pFieldControlField
     -- we substract 1 from the directory length because we already parsed the field control field
     ddf <- A.count (pred (length $ _directoryEntries d)) pDataDescriptiveField
     pure $ Module l d fcf ddf

pISO8211 = pModule

