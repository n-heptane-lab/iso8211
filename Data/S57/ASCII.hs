{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
module Data.S57.ASCII where

import Control.Applicative ((<|>), liftA2, liftA3, optional)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.ByteString.Unsafe   as B
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Map as Map
import           Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Bits
import GHC.Base (Int(..), uncheckedShiftL#)
import GHC.Word

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32

shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)


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

data Leader = Leader
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

pDDRLeader :: Parser Leader
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
     pure $ Leader rl il li ice vn ai fcl sa ecs em

pDRLeader :: Parser Leader
pDRLeader =
  do rl  <- A.take 5
     il  <- A.char ' '
     li  <- A.char 'D'
     ice <- A.char ' '
     vn  <- A.char ' '
     ai  <- A.char ' '
     fcl <- liftA2 (,) (A.char ' ') (A.char ' ')
     sa  <- A.take 5
     ecs <- liftA3 (,,) (A.char ' ') (A.char ' ') (A.char ' ')
     em  <- pEntryMap
     pure $ Leader rl il li ice vn ai fcl sa ecs em

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
  do des <- A.manyTill (pDirectoryEntry entryMap) pFt
     pure $ Directory des

ut = '\x1f'

-- | unit terminator - Δ
pUt :: Parser ()
pUt =
  do A.char ut
     pure ()

ft = '\x1e'

-- | field terminator - ∇
pFt :: Parser ()
pFt =
  do A.char ft
     pure ()

skipField :: Parser ()
skipField = A.manyTill A.anyChar pFt *> pure ()

pNull :: Parser ()
pNull =
  do A.char '\0'
     A.char '\0'
     pure ()

pFieldName :: Parser ByteString
pFieldName = A.takeTill ((==) ut) <* pUt

data TruncatedEscapeSequence
  = TES0
  | TES1
  | TES2
  deriving Show

pTruncatedEscapeSequence :: Parser TruncatedEscapeSequence
pTruncatedEscapeSequence =
  A.choice [ A.string "   " *> pure TES0
           , A.string "-A " *> pure TES1
           , A.string "%/A" *> pure TES2
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
     pUt
     pairs <- A.manyTill pFieldPair pFt
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
  do A.manyTill A.anyChar pUt
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

pA :: Maybe Int -> Parser ByteString
pA (Just l) = A.take l

pI :: Int -> Parser Int
pI l = implicit2i <$> A.count l A.digit

pR :: Maybe Int -> Parser ByteString
pR (Just l) = A.take l

pB1W :: Int -> Parser Word32
pB1W 1 =
  do s <- A.take 1
     pure $! fromIntegral (B.unsafeHead s)

pB1W 2 =
  do s <- A.take 2
     pure $! fromIntegral $
             (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w16` 8) .|.
             (fromIntegral (s `B.unsafeIndex` 0) )

pB1W 4 =
  do s <- A.take 4
     return $!
       (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24) .|.
       (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16) .|.
       (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32`  8) .|.
       (fromIntegral (s `B.unsafeIndex` 0) )

data LexicalLevel
  = LexicalLevel0
  | LexicalLevel1
  | LexicalLevel2
    deriving Show

pLexicalLevel :: DataFormat -> Parser LexicalLevel
pLexicalLevel df =
  case df of
    (B1W w) -> do v <- pB1W w
                  case v of
                    0 -> pure LexicalLevel0
                    1 -> pure LexicalLevel1
                    2 -> pure LexicalLevel2

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
  DataDescriptiveField <$> pFieldControls <*> pFieldName <*> pArrayDescriptor <*> pFormatControls <* pFt


data DataRecord = DataRecord
  { _drLeader    :: Leader
  , _drDirectory :: Directory
  , _drFields    :: [Field]
  }
  deriving Show

pDataRecord :: Map FieldTag DataDescriptiveField -> Parser DataRecord
pDataRecord ddf =
  do leader <- pDRLeader
     directory <- pDirectory (_entryMap leader)
     skipField
     dsid <- pDSID (_formatControls $ fromJust (Map.lookup "DSID" ddf))
     dssi <- pDSSI (_formatControls $ fromJust (Map.lookup "DSSI" ddf))
     pure (DataRecord leader directory [FDSID dsid, FDSSI dssi])

data TextDomain
  = BT
  | GT
  | DG
  | DATE
  | INT
  | REAL
  | AN
  | HEX
    deriving Show

data ASCII (dom :: TextDomain) = ASCII ByteString
 deriving Show

data RecordName
  = DS
  deriving Show

pRecordName :: DataFormat -> Parser RecordName
pRecordName df =
  case df of
    (A ml) ->
      do r <- pA ml
         case r of
           "DS" -> pure DS
           _ -> fail $ show r ++ " is not a known record name"
    (B1W i) ->
      do r <- pB1W i
         case r of
           10 -> pure DS
           _ -> fail $ show r ++ " is not a known record name"

newtype RecordID = RecordID { _recordID :: Word32 }
  deriving Show

pRecordID :: DataFormat -> Parser RecordID
pRecordID df =
  case df of
    (B1W w) -> RecordID <$> pB1W w

data ExchangePurpose
  = New
  | Revision
    deriving Show

pExchangePurpose :: DataFormat -> Parser ExchangePurpose
pExchangePurpose df =
  case df of
    (B1W w) ->
      do v <- pB1W w
         case v of
           1 -> pure New
           2 -> pure Revision

pIntu :: DataFormat -> Parser Int
pIntu df =
  case df of
    (B1W 1) ->
      do v <- pB1W 1
         pure (fromIntegral v)

data ProductSpecification
  = ENC -- ENC Electronic Navigation Chart
  | ODD -- IHO Object Catalogue Data Direction
    deriving Show

pProductSpecification :: DataFormat -> Parser ProductSpecification
pProductSpecification df =
  case df of
    (B1W w) ->
       do v <- pB1W w
          case v of
            1 -> pure ENC
            2 -> pure ODD
            _ -> fail $ "Unknown product specification = " ++ show v

data ApplicationProfileIdentification
  = EN -- ENC New
  | ER -- ENC Revision
  | DD -- IHO Data dictionary
    deriving Show

pApplicationProfileIdentification :: DataFormat -> Parser ApplicationProfileIdentification
pApplicationProfileIdentification df =
  case df of
    (B1W w) ->
      do v <- pB1W w
         case v of
           1 -> pure EN
           2 -> pure ER
           3 -> pure DD

data DSID = DSID
  { rcnm :: RecordName
  , rcid :: RecordID
  , expp :: ExchangePurpose
  , intu :: Int
  , dsnm :: ASCII 'BT
  , edtn :: ASCII 'BT
  , updn :: ASCII 'BT
  , uadt :: ASCII 'DATE
  , isdt :: ASCII 'DATE
  , sted :: ASCII 'REAL -- actually R(4)
  , prsp :: ProductSpecification
  , psdn :: ASCII 'BT
  , pred :: ASCII 'BT
  , prof :: ApplicationProfileIdentification
  , agen :: ProducingAgency
  , comt :: ASCII 'BT
  }
 deriving Show

data ProducingAgency
 = NOAA
 | ProducingAgency Word32
 deriving Show


-- need more information to implement correctly
pProducingAgency :: DataFormat -> Parser ProducingAgency
pProducingAgency df =
  case df of
    (B1W w) ->
      do v <- pB1W w
         case v of
           550 -> pure NOAA
           _   -> pure (ProducingAgency v)

pASCII = ASCII <$> A.takeTill ((==) ut) <* pUt

pAN :: DataFormat -> Parser (ASCII 'AN)
pAN df =
  case df of
    (A w) -> ASCII <$> pA w

pBT :: Parser (ASCII 'BT)
pBT = pASCII

pDate :: DataFormat -> Parser (ASCII 'DATE)
pDate df =
  case df of
    (A i) -> ASCII <$> pA i

pReal :: Maybe Int -> Parser (ASCII 'REAL)
pReal i = ASCII <$> (pR i)

pInt :: DataFormat -> Parser Word32
pInt df =
  case df of
    (B1W w) -> pB1W w

pDSID :: FormatControls -> Parser DSID
pDSID (FormatControls fcs) =
  DSID <$> pRecordName      (fcs!!0)
       <*> pRecordID        (fcs!!1)
       <*> pExchangePurpose (fcs!!2)
       <*> pIntu            (fcs!!3)
       <*> pBT              -- 4
       <*> pBT              -- 5
       <*> pBT              -- 6
       <*> pDate            (fcs!!7)
       <*> pDate            (fcs!!8)
       <*> pReal              (Just 4) -- (fcs!!9)
       <*> pProductSpecification (fcs!!10)
       <*> pBT              -- 11
       <*> pBT              -- 12
       <*> pApplicationProfileIdentification (fcs!!13)
       <*> pProducingAgency (fcs!!14)
       <*> pBT              -- 15
       <*  pFt

data DataStructure
  = CS -- ^ Cartographic Spaghetti
  | CN -- ^ Chain-node
  | PG -- ^ Planar Graph
  | FT -- ^ Full Topology
  | NO -- ^ Topology is not relevant
    deriving Show

pDataStructure :: DataFormat -> Parser DataStructure
pDataStructure df =
  case df of
    (B1W w) ->
      do v <- pB1W w
         case v of
           1 -> pure CS
           2 -> pure CN
           3 -> pure PG
           4 -> pure FT
           5 -> pure NO
           _ -> fail $ "Unrecognized Data Structure = " ++ show v

data DSSI = DSSI
  { dstr :: DataStructure
  , aall :: LexicalLevel
  , nall :: LexicalLevel
  , nomr :: Word32
  , nocr :: Word32
  , nogr :: Word32
  , nolr :: Word32
  , noin :: Word32
  , nocn :: Word32
  , noed :: Word32
  , nofa :: Word32
  }
  deriving Show

pDSSI :: FormatControls -> Parser DSSI
pDSSI (FormatControls fcs) =
  DSSI <$> pDataStructure (fcs!!0)
       <*> pLexicalLevel  (fcs!!1)
       <*> pLexicalLevel  (fcs!!2)
       <*> pInt           (fcs!!3)
       <*> pInt           (fcs!!4)
       <*> pInt           (fcs!!5)
       <*> pInt           (fcs!!6)
       <*> pInt           (fcs!!7)
       <*> pInt           (fcs!!8)
       <*> pInt           (fcs!!9)
       <*> pInt           (fcs!!10)
       <* pFt

data Module = Module
  { _ddrLeader             :: Leader
  , _ddrDirectory          :: Directory
  , _fieldControlField     :: FieldControlField
  , _dataDescriptiveFields :: Map FieldTag DataDescriptiveField
  , _drs                   :: [DataRecord]
  }
  deriving Show

data Field
 = FDSID DSID
 | FDSSI DSSI
 deriving Show

pModule :: Parser Module
pModule =
  do l <- pDDRLeader
     d <- pDirectory (_entryMap l)
     fcf <- pFieldControlField
     -- we substract 1 from the directory length because we already parsed the field control field
     ddf' <- A.count (Prelude.pred (length $ _directoryEntries d)) pDataDescriptiveField
     let ddf = Map.fromList $ zipWith (\d df -> (_fieldTag d, df)) (drop 1 $ _directoryEntries d) ddf'
     dr <- pDataRecord ddf
     pure $ Module l d fcf ddf [dr]

pISO8211 = pModule

