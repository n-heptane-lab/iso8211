{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
module Data.S57.ASCII where

import Control.Applicative ((<|>), liftA2, liftA3, optional)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8 (Parser, IResult(..))
import qualified Data.ByteString.Unsafe   as B
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Attoparsec.ByteString.Char8 ((<?>))
import           Data.Attoparsec.ByteString (anyWord8)
import qualified Data.Map as Map
import           Data.Map (Map)
import Data.Foldable (find)
import Data.Int (Int8, Int16, Int32)
import Data.Maybe (fromJust)
import Data.Bits
import Data.List (nub)
import GHC.Base (Int(..), uncheckedShiftL#)
import GHC.Word
import Prelude hiding (GT)
import Debug.Trace (trace)

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32

shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)


-- http://www.dtic.mil/dtic/tr/fulltext/u2/a281594.pdf
-- https://github.com/freekvw/iso8211/blob/master/iso8211.py
-- https://github.com/tburke/iso8211/blob/master/iso8211.go
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
  deriving (Eq, Show)

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
     ecs <- liftA3 (,,) (A.char ' ') (A.char '!' <|> A.char ' ') (A.char ' ')
     em  <- pEntryMap
     pure $ Leader rl il li ice vn ai fcl sa ecs em

pDRLeader :: Parser Leader
pDRLeader =
  do rl  <- A.take 5
     il  <- A.char ' ' <?> "failed to parse space"
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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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

digits2int :: (Integral i) => [Char] -> i
digits2int cs = fromIntegral (digits2int' 0 cs)
  where
    digits2int' acc [] = acc
    digits2int' acc (c:cs) = digits2int' ((acc * 10) + (c2i c)) cs

pDirectoryEntry :: EntryMap -> Parser DirectoryEntry
pDirectoryEntry (EntryMap sfl sfp _ sft) =
  do ft <- A.take $ c2i sft
     fl <- A.take $ c2i sfl
     fp <- A.take $ c2i sfp
     pure $ DirectoryEntry ft fl fp

data Directory = Directory { _directoryEntries :: [DirectoryEntry] }
  deriving (Eq, Show)

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

skipField :: Int -> Parser ()
skipField c = A.count (Prelude.pred c) A.anyChar *> pFt *> pure () -- A.manyTill A.anyChar pFt *> pure ()

skipUnit :: Parser ()
skipUnit = A.manyTill A.anyChar pUt *> pure ()

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
  deriving (Eq, Show)

pTruncatedEscapeSequence :: Parser TruncatedEscapeSequence
pTruncatedEscapeSequence =
  A.choice [ A.string "   " *> pure TES0
           , A.string "-A " *> pure TES1
           , A.string "%/A" *> pure TES2
           ] <|> error "unknown tes"

data FieldControlField = FieldControlField
  { _fieldTags :: [(ByteString, ByteString)]
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

pDataStructureCode :: Parser DataStructureCode
pDataStructureCode =
  A.choice [ A.char '0' *> pure Singleton
           , A.char '1' *> pure Linear
           , A.char '2' *> pure MultiDimensional
           ] <|> error "unknown data structure code"


data ArrayDescriptor
  = ArrayDescriptor
  deriving (Eq, Show)

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
   deriving (Eq, Show)

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
                    ]) <|> error "unknown data format"
  where
    rep p =
      do c <- (c2i <$> A.digit) <|> pure 1
         r <- p
         pure $ replicate c r
    pWidth =
      do A.char '('
         i <- A.many' A.digit
         A.char ')'
         pure (Just $ digits2int i)
      <|> pure Nothing
    pWidth2 =
      do A.char '('
         i10 <- A.digit
         i1  <- A.digit
         A.char ')'
         pure $ ((c2i i10) * 10) + (c2i i1)

pA :: Maybe Int -> Parser ByteString
pA (Just l) = A.take l
pA Nothing = A.takeTill ((==) ut) <* pUt

pI :: Int -> Parser Int
pI l = digits2int <$> A.count l A.digit

pW :: Int -> Parser Word32
pW l = digits2int <$> A.count l A.digit

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

pB2W :: (Integral i) => Int -> Parser i
pB2W 1 =
  do s <- A.take 1
     pure $! fromIntegral (fromIntegral (B.unsafeHead s) :: Int8)
pB2W 2 =
  do s <- A.take 2
     return $! fromIntegral
       ((fromIntegral (s `B.unsafeIndex` 1) `shiftL` 8) .|.
        (fromIntegral (s `B.unsafeIndex` 0) ) :: Int16)
pB2W 4 =
  do s <- A.take 4
     return $! fromIntegral $
        ((fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24) .|.
         (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16) .|.
         (fromIntegral (s `B.unsafeIndex` 1) `shiftL`  8) .|.
         (fromIntegral (s `B.unsafeIndex` 0) ) :: Int32)

pB :: Int -> Parser [Word8]
pB l
  | l `mod` 8 == 0 =
    let c = l `div` 8
    in A.count c anyWord8

data LexicalLevel
  = LexicalLevel0
  | LexicalLevel1
  | LexicalLevel2
    deriving (Eq, Show)

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
  deriving (Eq, Show)

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
    deriving (Eq, Show)

pDataTypeCode :: Parser DataTypeCode
pDataTypeCode =
  A.choice [ A.char '0' *> pure CharacterString
           , A.char '1' *> pure ImplicitPoint
           , A.char '5' *> pure BinaryForm
           , A.char '6' *> pure MixedDataTypes
           ] <|> error "unknown data type code"

pAuxiliaryControls :: Parser ()
pAuxiliaryControls = A.string "00" *> pure ()

data FieldControls = FieldControls
  { _dataStructureCode       :: DataStructureCode
  , _dataTypeCode            :: DataTypeCode
  , _truncatedEscapeSequence :: TruncatedEscapeSequence
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

pDataDescriptiveField :: Parser DataDescriptiveField
pDataDescriptiveField =
  DataDescriptiveField <$> pFieldControls <*> pFieldName <*> pArrayDescriptor <*> pFormatControls <* pFt


data DataRecord = DataRecord
  { _drLeader    :: Leader
  , _drDirectory :: Directory
  , _drFields    :: [Field]
  }
  deriving (Eq, Show)

p0001 :: FormatControls -> Parser RecordID
p0001 (FormatControls fcs) =
  pRecordID (fcs!!0) <* pFt

-- FIXME: should be tail recursive
pFields :: Map FieldTag DataDescriptiveField -> Directory -> Parser [Field]
pFields ddfs (Directory entries) = pFields' entries
  where
    pFields' [] = pure []
    pFields' ((DirectoryEntry ft fl _):ds) =
      do let fcs = _formatControls $ fromJust (Map.lookup ft ddfs)
         f <- case ft of
               "0001" -> RecordIdentifier <$> p0001 fcs
               "DSID" -> FDSID <$> pDSID fcs
               "DSSI" -> FDSSI <$> pDSSI fcs
               "FRID" -> FFRID <$> pFRID fcs
               "FOID" -> FFOID <$> pFOID fcs
               "ATTF" -> FATTF <$> pATTFS fcs (digits2int $ C.unpack fl)
               "ATTV" -> FATTV <$> pATTVS fcs (digits2int $ C.unpack fl)
               "FFPT" -> FFFPT <$> pFFPTS fcs (digits2int $ C.unpack fl)
               "FSPT" -> FFSPT <$> pFSPTS fcs (digits2int $ C.unpack fl)
               "VRID" -> FVRID <$> pVRID fcs
--               "SG3D" -> pure (Unknown ft) <* (A.count 7 skipField)
               "SG2D" -> FSG2D <$> pSG2DS fcs (digits2int $ C.unpack fl)
               "SG3D" -> FSG3D <$> pSG3DS fcs (digits2int $ C.unpack fl)
               "DSPM" -> FDSPM <$> pDSPM fcs
               "CATD" -> FCATD <$> pCATD fcs
               "VRPT" -> FVRPT <$> pVRPT fcs
               _      -> pure (Unknown ft) <* (skipField (digits2int $ C.unpack fl))
--               _      -> fail $ "No parser for " ++ show ft
         fs <- pFields' ds
         pure (f:fs)

pDataRecord :: Map FieldTag DataDescriptiveField -> Parser DataRecord
pDataRecord ddf =
  do leader <- pDRLeader <?> "failed to parse leader"
     directory <- pDirectory (_entryMap leader)
     fields <- pFields ddf directory
     pure (DataRecord leader directory fields)

data TextDomain
  = BT
  | GT
  | DG
  | DATE
  | INT
  | REAL
  | AN
  | HEX
    deriving (Eq, Show)

data ASCII (dom :: TextDomain) = ASCII ByteString
 deriving (Eq, Show)

data RecordName
  = DS -- ^ Data Set General Information
  | DP -- ^ Data Set Geographic Reference
  | DH -- ^ Data Set History
  | DA -- ^ Data Set Accuracy
  | CD -- ^ Catalogue Directory
  | CR -- ^ Catalogue Cross Reference
  | ID -- ^ Data Dictionary Definition
  | IO -- ^ Data Dictionary Domain
  | IS -- ^ Data Dictionary Schema
  | FE -- ^ Feature
  | VI -- ^ Isolated Node
  | VC -- ^ Connected Node
  | VE -- ^ Edge
  | VF -- ^ Face
  deriving (Eq, Show)

pRecordName :: DataFormat -> Parser RecordName
pRecordName df =
  case df of
    (A ml) ->
      do r <- pA ml
         case r of
           "CD" -> pure CD
           "DS" -> pure DS
           _ -> fail $ show r ++ " is not a known record name"
    (B1W i) ->
      do r <- pB1W i
         case r of
           10  -> pure DS
           20  -> pure DP
           30  -> pure DH
           40  -> pure DA
           60  -> pure CR
           70  -> pure ID
           80  -> pure IO
           90  -> pure IS
           100 -> pure FE
           110 -> pure VI -- isolated node
           120 -> pure VC -- connected node
           130 -> pure VE -- edge
           140 -> pure VF -- face
           _ -> fail $ show r ++ " is not a known record name"

newtype RecordID = RecordID { _recordID :: Word32 }
  deriving (Eq, Show)

pRecordID :: DataFormat -> Parser RecordID
pRecordID df =
  case df of
    (B1W w) -> RecordID <$> pB1W w
    (I (Just w))   -> RecordID <$> pW w

data ExchangePurpose
  = New
  | Revision
    deriving (Eq, Show)

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
    deriving (Eq, Show)

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
    deriving (Eq, Show)

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
 deriving (Eq, Show)

data ProducingAgency
 = NOAA
 | ProducingAgency Word32
 deriving (Eq, Show)


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

pGT :: Parser (ASCII 'GT)
pGT = pASCII

pDate :: DataFormat -> Parser (ASCII 'DATE)
pDate df =
  case df of
    (A i) -> ASCII <$> pA i

pReal :: DataFormat -> Parser (ASCII 'REAL)
pReal df =
  case df of
    (R i) -> ASCII <$> (pA i)
--    _ -> trace "not implemented" (error "not implemented.")

pHex :: DataFormat -> Parser (ASCII 'HEX)
pHex df =
  case df of
    (A i) -> ASCII <$> (pA i)

pInt :: DataFormat -> Parser Word32
pInt df =
  case df of
    (B1W w) -> pB1W w

pIntegral :: (Integral i) => DataFormat -> Parser i
pIntegral df = fromIntegral <$> pInt df

pInteger :: (Integral i) => DataFormat -> Parser i
pInteger df =
  case df of
  (B2W w) -> pB2W w

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
       <*> pReal            (fcs!!9)
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
    deriving (Eq, Show)

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
  deriving (Eq, Show)

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

data ObjectGeometricPrimitive
  = Point  -- ^ Point
  | Line  -- ^ Line
  | Area  -- ^ Area
  | NA  -- ^ Object does not directly reference any spatial objects
  deriving (Eq, Show)

pObjectGeometricPrimitive :: DataFormat -> Parser ObjectGeometricPrimitive
pObjectGeometricPrimitive df =
  case df of
    (B1W w) ->
      do r <- pB1W w
         case r of
           1   -> pure Point
           2   -> pure Line
           3   -> pure Area
           255 -> pure NA

data RecordUpdateInstruction
  = Insert
  | Delete
  | Modify
  deriving (Eq, Show)

pRecordUpdateInstruction :: DataFormat -> Parser RecordUpdateInstruction
pRecordUpdateInstruction df =
  case df of
    (B1W w) -> do v <- pB1W w
                  case v of
                    1 -> pure Insert
                    2 -> pure Delete
                    3 -> pure Modify

data FRID = FRID
  { rcnm :: RecordName
  , rcid :: RecordID
  , prim :: ObjectGeometricPrimitive
  , grup :: Word8
  , objl :: Word16
  , rver :: Word16
  , ruin :: RecordUpdateInstruction
  }
  deriving (Eq, Show)

pFRID :: FormatControls -> Parser FRID
pFRID (FormatControls fcs) =
  FRID <$> pRecordName (fcs!!0)
       <*> pRecordID   (fcs!!1)
       <*> pObjectGeometricPrimitive (fcs!!2)
       <*> (fromIntegral <$> pInt (fcs!!3))
       <*> (fromIntegral <$> pInt (fcs!!4))
       <*> (fromIntegral <$> pInt (fcs!!5))
       <*> pRecordUpdateInstruction (fcs!!6)
       <* pFt

data FOID = FOID
  { agen :: ProducingAgency
  , fidn :: Word32
  , fids :: Word16
  }
  deriving (Eq, Show)

pFOID :: FormatControls -> Parser FOID
pFOID (FormatControls fcs) =
  FOID <$> pProducingAgency (fcs!!0)
       <*> pInt (fcs!!1)
       <*> (fromIntegral <$> (pInt (fcs!!2)))
       <* pFt

data ATTFS = ATTFS [ATTF]
 deriving (Eq, Show)

data ATTF = ATTF
  { attl :: Word16
  , atvl :: ASCII 'GT
  }
  deriving (Eq, Show)

pATTF :: FormatControls -> Parser ATTF
pATTF (FormatControls fcs) =
  ATTF <$> pIntegral (fcs!!0) <*> pGT

pATTFS :: FormatControls -> Int -> Parser ATTFS
pATTFS fcs len = ATTFS <$> pRepeated len (pATTF fcs)

data ATTVS = ATTVS [ATTV]
 deriving (Eq, Show)

data ATTV = ATTV
  { attl :: Word16
  , atvl :: ASCII 'BT
  }
  deriving (Eq, Show)

pATTV :: FormatControls -> Parser ATTV
pATTV (FormatControls fcs) =
  ATTV <$> pIntegral (fcs!!0) <*> pBT

pATTVS :: FormatControls -> Int -> Parser ATTVS
pATTVS fcs len = ATTVS <$> pRepeated len (pATTV fcs)

-- FIXME: we can not just repeat a parser until we see a 0x1f because a value in the field could start with 0x1f
-- we need to look at the field length
pRepeated :: (Show a) => Int -> Parser a -> Parser [a]
pRepeated len p =
  do bs <- A.take (len - 1)
     pFt
     r <- A.parseWith (pure mempty) (A.many' p) bs
     case r of
       f@(Fail {}) -> fail $ show f
       Partial {} -> fail "pRepeated ended with a Partial."
       Done i r
         | mempty == i -> pure r
         | otherwise -> fail $ "pRepeated terminated with left over data " ++ show i

data ForeignPointer = ForeignPointer [Word8]
                    deriving (Eq, Show)

pForeignPointer :: DataFormat -> Parser ForeignPointer
pForeignPointer df =
  case df of
    (B w) -> ForeignPointer <$> (pB w)

data Orientation
  = Forward
  | Reverse
  | NullOrientation
    deriving (Eq, Show)

pOrientation :: DataFormat -> Parser Orientation
pOrientation df =
  case df of
    (B1W w) -> do v <- pB1W w
                  case v of
                    1   -> pure Forward
                    2   -> pure Reverse
                    255 -> pure NullOrientation
                    _   -> fail $ "Invalid orientation = " ++ show v

data UsageIndicator
  = Exterior
  | Interior
  | ExteriorBoundary
  | NullUsageIndicator
    deriving (Eq, Show)

pUsageIndicator :: DataFormat -> Parser UsageIndicator
pUsageIndicator df =
  case df of
    (B1W w) -> do v <- pB1W w
                  case v of
                    1   -> pure Exterior
                    2   -> pure Interior
                    3   -> pure ExteriorBoundary
                    255 -> pure NullUsageIndicator
                    _   -> fail $ "Invalid usage indicator = " ++ show v

data MaskingIndicator
  = Mask
  | Show
  | NullMaskingIndicator
    deriving (Eq, Show)

pMaskingIndicator :: DataFormat -> Parser MaskingIndicator
pMaskingIndicator df =
  case df of
    (B1W w) -> do v <- pB1W w
                  case v of
                    1   -> pure Mask
                    2   -> pure Show
                    255 -> pure NullMaskingIndicator
                    _   -> fail $ "Invalid masking indicator = " ++ show v


data FSPT = FSPT
  { name :: ForeignPointer
  , ornt :: Orientation
  , usag :: UsageIndicator
  , mask :: MaskingIndicator
  }
  deriving (Eq, Show)

pFSPT :: FormatControls -> Parser FSPT
pFSPT (FormatControls fcs) =
  FSPT <$> pForeignPointer   (fcs!!0)
       <*> pOrientation      (fcs!!1)
       <*> pUsageIndicator   (fcs!!2)
       <*> pMaskingIndicator (fcs!!3)

data FSPTS = FSPTS { _fspts :: [FSPT] }
  deriving (Eq, Show)

pFSPTS :: FormatControls -> Int -> Parser FSPTS
pFSPTS fcs len = FSPTS <$> pRepeated len (pFSPT fcs)

data RelationshipIndicator
  = Master
  | Slave
  | Peer
  deriving (Eq, Show)

pRelationshipIndicator :: DataFormat -> Parser RelationshipIndicator
pRelationshipIndicator df =
  case df of
    (B1W w) -> do v <- pB1W w
                  case v of
                    1   -> pure Master
                    2   -> pure Slave
                    3   -> pure Peer
                    _   -> fail $ "Invalid relationship indicator = " ++ show v

data FFPT = FFPT
  { lnam :: ForeignPointer
  , rind :: RelationshipIndicator
  , comt :: ASCII 'BT
  }
  deriving (Eq, Show)

pFFPT :: FormatControls -> Parser FFPT
pFFPT (FormatControls fcs) =
  FFPT <$> pForeignPointer (fcs!!0)
       <*> pRelationshipIndicator (fcs!!1)
       <*> pBT

data FFPTS = FFPTS { _ffpts :: [FFPT] }
  deriving (Eq, Show)

pFFPTS :: FormatControls -> Int -> Parser FFPTS
pFFPTS fcs len = FFPTS <$> pRepeated len (pFFPT fcs)

-- incompleted
data VRID = VRID
  { rcnm :: RecordName
  , rcid :: RecordID
  , rver :: Word16
  , ruin :: RecordUpdateInstruction
  }
 deriving (Eq, Show)

pVRID :: FormatControls -> Parser VRID
pVRID (FormatControls fcs) =
  VRID <$> pRecordName (fcs!!0)
       <*> pRecordID   (fcs!!1)
       <*> pIntegral   (fcs!!2)
       <*> pRecordUpdateInstruction (fcs!!3)
       <*  pFt

data SG3D = SG3D
  { ycoo :: Int32
  , xcoo :: Int32
  , ve3d :: Int32
  }
  deriving (Eq, Show)

pSG3D :: FormatControls -> Parser SG3D
pSG3D (FormatControls fcs) =
  SG3D <$> pInteger (fcs!!0)
       <*> pInteger (fcs!!1)
       <*> pInteger (fcs!!2)

data SG3DS = SG3DS { _sg3ds :: [SG3D] }
  deriving (Eq, Show)

pSG3DS :: FormatControls -> Int -> Parser SG3DS
pSG3DS fcs len = SG3DS <$> pRepeated len (pSG3D fcs)

data SG2D = SG2D
  { ycoo :: Int32
  , xcoo :: Int32
  }
  deriving (Eq, Show)

pSG2D :: FormatControls -> Parser SG2D
pSG2D (FormatControls fcs) =
  SG2D <$> pInteger (fcs!!0)
       <*> pInteger (fcs!!1)

data SG2DS = SG2DS { _sg2ds :: [SG2D] }
  deriving (Eq, Show)

pSG2DS :: FormatControls -> Int -> Parser SG2DS
pSG2DS fcs len = SG2DS <$> pRepeated len (pSG2D fcs)

data CoordinateUnits
  = LatitudeLongitude -- ^ LatitudeLongitude
  | EastingNorthing-- ^ Easting/Northing
  | UnitsChart -- ^ Units on the chart/map
    deriving (Eq, Show)

pCoordinateUnits :: DataFormat -> Parser CoordinateUnits
pCoordinateUnits df =
  case df of
    (B1W w) -> do v <- pB1W w
                  case v of
                    1 -> pure LatitudeLongitude
                    2 -> pure EastingNorthing
                    3 -> pure UnitsChart

data DSPM = DSPM
  { rcnm :: RecordName
  , rcid :: RecordID
  , hdat :: Word8
  , vdat :: Word8
  , sdat :: Word8
  , cscl :: Word32
  , duni :: Word8
  , huni :: Word8
  , puni :: Word8
  , coun :: CoordinateUnits
  , comf :: Word32
  , somf :: Word32
  , comt :: ASCII 'BT
  }
  deriving (Eq, Show)

pDSPM :: FormatControls -> Parser DSPM
pDSPM (FormatControls fcs) =
  DSPM <$> pRecordName (fcs!!0)
       <*> pRecordID   (fcs!!1)
       <*> pIntegral   (fcs!!2)
       <*> pIntegral   (fcs!!3)
       <*> pIntegral   (fcs!!4)
       <*> pIntegral   (fcs!!5)
       <*> pIntegral   (fcs!!6)
       <*> pIntegral   (fcs!!7)
       <*> pIntegral   (fcs!!8)
       <*> pCoordinateUnits (fcs!!9)
       <*> pIntegral   (fcs!!10)
       <*> pIntegral   (fcs!!11)
       <*> pBT
       <*  pFt


data Implementation
  = ASC
  | BIN
  | TXT
    deriving (Eq, Show)

pImplementation :: DataFormat -> Parser Implementation
pImplementation df =
  case df of
    (A w) -> do r <- pA w
                case r of
                  "ASC" -> pure ASC
                  "BIN" -> pure BIN
                  "TXT" -> pure TXT
                  _ -> error $ "unknown case =" ++ show r

data CATD = CATD
  { rcnm :: RecordName
  , rcid :: RecordID
  , file :: ASCII 'BT
  , lfil :: ASCII 'BT
  , volm :: ASCII 'BT
  , impl :: Implementation
  , slat :: ASCII 'REAL
  , wlon :: ASCII 'REAL
  , nlat :: ASCII 'REAL
  , elat :: ASCII 'REAL
  , crcs :: ASCII 'HEX
  , comt :: ASCII 'BT
  }
  deriving (Eq, Show)

pCATD :: FormatControls -> Parser CATD
pCATD (FormatControls fcs) =
  CATD <$> pRecordName (fcs!!0)
       <*> pRecordID   (fcs!!1)
       <*> pBT -- 2
       <*> pBT -- 3
       <*> pBT -- 4
       <*> pImplementation (fcs!!5)
       <*> pReal (fcs!!6)
       <*> pReal (fcs!!7)
       <*> pReal (fcs!!8)
       <*> pReal (fcs!!9)
       <*> pHex (fcs!!10)
       <*> pBT
       <* pFt

data TopologyIndicator
  = BeginningNode
  | EndNode
  | LeftFace
  | RightFace
  | ContainingFace
  | NullTopologyIndicator
    deriving (Eq, Show)

pTopologyIndicator :: DataFormat -> Parser TopologyIndicator
pTopologyIndicator df =
  case df of
    (B1W w) -> do v <- pB1W w
                  case v of
                    1   -> pure BeginningNode
                    2   -> pure EndNode
                    3   -> pure LeftFace
                    4   -> pure RightFace
                    5   -> pure ContainingFace
                    255 -> pure NullTopologyIndicator
                    _   -> fail $ "Invalid masking indicator = " ++ show v

data VRPT = VRPT
  { name :: ForeignPointer
  , ornt :: Orientation
  , usag :: UsageIndicator
  , topi :: TopologyIndicator
  , mask :: MaskingIndicator
  }
  deriving (Eq, Show)

pVRPT :: FormatControls -> Parser VRPT
pVRPT (FormatControls fcs) =
  VRPT <$> pForeignPointer    (fcs!!0)
       <*> pOrientation       (fcs!!1)
       <*> pUsageIndicator    (fcs!!1)
       <*> pTopologyIndicator (fcs!!2)
       <*> pMaskingIndicator  (fcs!!3)

data Field
 = RecordIdentifier RecordID
 | FDSID DSID
 | FDSSI DSSI
 | FFRID FRID
 | FFOID FOID
 | FATTF ATTFS
 | FATTV ATTVS
 | FFSPT FSPTS
 | FFFPT FFPTS
 | FVRID VRID
 | FSG2D SG2DS
 | FSG3D SG3DS
 | FDSPM DSPM
 | FCATD CATD
 | FVRPT VRPT
 | Unknown FieldTag
 deriving (Eq, Show)

data Module = Module
  { _ddrLeader             :: Leader
  , _ddrDirectory          :: Directory
  , _fieldControlField     :: FieldControlField
  , _dataDescriptiveFields :: Map FieldTag DataDescriptiveField
  , _drs                   :: [DataRecord]
  }
  deriving (Eq, Show)

pModule :: Parser Module
pModule =
  do l <- pDDRLeader
     d <- pDirectory (_entryMap l)
     fcf <- pFieldControlField
     -- we substract 1 from the directory length because we already parsed the field control field
     ddf' <- A.count (Prelude.pred (length $ _directoryEntries d)) pDataDescriptiveField
     let ddf = Map.fromList $ zipWith (\d df -> (_fieldTag d, df)) (drop 1 $ _directoryEntries d) ddf'
     drs <- A.many' (pDataRecord ddf)
     pure $ Module l d fcf ddf drs

pISO8211 = pModule

printUnknown :: [DataRecord] -> IO ()
printUnknown drs =
  let allFields = concatMap _drFields drs
      isUnknown :: Field -> Bool
      isUnknown (Unknown {}) = True
      isUnknown _ = False
  in print $ nub $ filter isUnknown allFields

printDSSI :: Module -> IO ()
printDSSI m =
  case _drs m of
    (dr:_) ->
      case find (\df -> case df of
                    (FDSSI {}) -> True
                    _          -> False) (_drFields dr) of
        Nothing -> putStrLn "Could not ofind DSSI"
        (Just dssi) -> print dssi

printDSPM :: Module -> IO ()
printDSPM m = printDSPM' (_drs m)
  where
  printDSPM' (dr:drs) =
      case find (\df -> case df of
                    (FDSPM {}) -> True
                    _          -> False) (_drFields dr) of
        (Just dssi) -> print dssi
        Nothing -> printDSPM' drs


