{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.S57.ObjectAttribute where

import Data.ByteString (ByteString)
import Data.Coerce
import Data.Text (Text)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word (Word16)

data Acronym
  = AGENCY
  | DUNITS
  | HORDAT
  | HUNITS
  | PUNITS
  | VERDAT
  deriving (Eq, Ord, Show)
{-
 :: ObjectAttribute 
 = ObjectAttribute
  { name           = ""
  , acronym        = 
  , code           = 
  , value          = 
  , definition     = Nothing
  , definitions    = []
  , referencesInt1 = []
  , referencesM4   = []
  , range          = Nothing
  , remarks        = Nothing
  , distinction    = Nothing
  }
-}
data Range
  = Range
    deriving (Eq, Ord, Show)

data ObjectAttribute (acronym :: Acronym) = ObjectAttribute
  { name           :: Text
  , acronym        :: Acronym
  , code           :: Word16
  , value          :: AttributeValue
  , definition     :: Maybe Text
  , definitions    :: [(Text, Text)]
  , referencesInt1 :: [Text]
  , referencesM4   :: [Text]
  , range          :: Maybe Range
  , remarks        :: Maybe Text
  , distinction    :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data AttributeValue
  = A [ByteString]
  | E (IntMap Text)
  deriving (Eq, Ord, Show)

-- FIXME: value list incomplete
agency :: ObjectAttribute AGENCY
agency = ObjectAttribute
  { name           = "Agency responsible for production"
  , acronym        = AGENCY
  , code           = 1
  , definition     = Just "This attribute identifies the agency which produced the data."
  , definitions    = []
  , value          = A [ "CA", "US" ]
  , referencesInt1 = []
  , referencesM4   = []
  , range          = Nothing
  , remarks        = Nothing
  , distinction    = Nothing
  }

dunits :: ObjectAttribute DUNITS
dunits = ObjectAttribute
  { name           = "Depth units"
  , acronym        = DUNITS
  , code           = 89
  , value          = E (IntMap.fromList
                        [ (1, "metres")
                        , (2, "fathoms and feet")
                        , (3, "feet")
                        , (4, "fathoms and fractions")
                        ])
  , definition     = Nothing
  , definitions    = [ ("metres", "depths are specified in metres (SI units of length).")
                     , ("fathoms and feet", "depths are specified in fathoms (units of six feet of depth) and feet.")
                     , ("feet", "depths are specified in feet (imperial units of length).")
                     , ("fathoms and fractions", "depths are specified in fathoms (units of six feet of depth) and fractions of fathoms.")
                     ]
  , referencesInt1 = []
  , referencesM4   = []
  , range          = Nothing
  , remarks        = Nothing
  , distinction    = Nothing
  }

hordat :: ObjectAttribute HORDAT
hordat = ObjectAttribute
  { name           = "Horizontal datum"
  , acronym        = HORDAT
  , code           = 400
  , definition     = Nothing
  , definitions    = []
  , value          = E (IntMap.fromList
                        [ (1, "WGS 72")
                        , (2, "WGS 84")
                        , (3, "European 1950")
                        , (4, "Potsdam Datum")
                        , (5, "Adindan")
                        , (6, "Afgooye")
                        , (7, "Ain el Abd 1970")
                        , (8, "Anna 1 Astro 1965")
                        , (9, "Antigua Island Astro 1943")
                        , (10, "Arc 1950")
                        , (11, "Arc 1960")
                        , (12, "Ascension Island 1958")
                        , (13, "Astro beacon \"E\" 1945")
                        , (14, "Astro DOS 71/4")
                        , (15, "Astro Tern Island (FRIG) 1961")
                        , (16, "Astronomical Station 1952")
                        , (17, "Australian Geodetic 1966")
                        , (18, "Australian Geodetic 1984")
                        , (19, "Ayabelle Lighthouse")
                        , (20, "Bellevue (IGN)")
                        , (21, "Bermuda 1957")
                        , (22, "Bissau")
                        , (23, "Bogota Observatory")
                        , (24, "Bukit Rimpah")
                        , (25, "Camp Area Astro")
                        , (26, "Campo Inchauspe 1969")
                        , (27, "Canton Astro 1966")
                        , (28, "Cape")
                        , (29, "Cape Canaveral")
                        , (30, "Carthage")
                        , (31, "Chatam Island Astro 1971")
                        , (32, "Chua Astro")
                        , (33, "Corrego Alegre")
                        , (34, "Dabola")
                        , (35, "Djakarta (Batavia)")
                        , (36, "DOS 1968")
                        , (37, "Easter Island 1967")
                        , (38, "European 1979")
                        , (39, "Fort Thomas 1955")
                        , (40, "Gan 1970")
                        , (41, "Geodetic Datum 1949")
                        , (42, "Graciosa Base SW 1948")
                        , (43, "Guam 1963")
                        , (44, "Gunung Segara")
                        , (45, "GUX 1 Astro")
                        , (46, "Herat North")
                        , (47, "Hjorsey 1955")
                        , (48, "Hong Kong 1963")
                        , (49, "Hu-Tzu-Shan")
                        , (50, "Indian")
                        , (51, "Indian 1954")
                        , (52, "Indian 1975")
                        , (53, "Ireland 1965")
                        , (54, "ISTS 061 Astro 1968")
                        , (55, "ISTS 073 Astro 1969")
                        , (56, "Johnston Island 1961")
                        , (57, "Kandawala")
                        , (58, "Kerguelen Island 1949")
                        , (59, "Kertau 1948")
                        , (60, "Kusaie Astro 1951")
                        , (61, "L. C. 5 Astro 1961")
                        , (62, "Leigon")
                        , (63, "Liberia 1964")
                        , (64, "Luzon")
                        , (65, "Mahe 1971")
                        , (66, "Massawa")
                        , (67, "Merchich")
                        , (68, "Midway Astro 1961")
                        , (69, "Minna")
                        , (70, "Montserrat Island Astro 1958")
                        , (71, "M=Poraloko")
                        , (72, "Nahrwan")
                        , (73, "Naparima, BWI")
                        , (74, "North American 1927")
                        , (75, "North American 1983")
                        , (76, "Observatorio Meteorologico 1939")
                        , (77, "Old Egyptian 1907")
                        , (78, "Old Hawaiian")
                        , (79, "Oman")
                        , (80, "Ordnance Survey of Great Britain 1936")
                        , (81, "Pico de las Nieves")
                        , (82, "Pitcairn Astro 1967")
                        , (83, "Point 58")
                        , (84, "Pointe Noire 1948")
                        , (85, "Porto Santo 1936")
                        , (86, "Provisional South American 1956")
                        , (87, "Provisional South Chilean 1963 (also known as Hito XVIII 1963)")
                        , (88, "Puerto Rico")
                        , (89, "Qatar national")
                        , (90, "Qornoq")
                        , (91, "Reunion")
                        , (92, "Rome 1940")
                        , (93, "Santo (DOS) 1965")
                        , (94, "Sao Braz")
                        , (95, "Sapper Hill 1943")
                        , (96, "Schwarzeck")
                        , (97, "Selvagem Grande 1938")
                        , (98, "South American 1969")
                        , (99, "South Asia")
                        , (100, "Tananarive Observatory 1925")
                        , (101, "Timbalai 1948")
                        , (102, "Tokyo")
                        , (103, "Tristan Astro 1968")
                        , (104, "Viti Levu 1916")
                        , (105, "Wake-Eniwetok 1960")
                        , (106, "Wake Island Astro 1952")
                        , (107, "Yacare")
                        , (108, "Zanderij")
                        , (109, "American Samoa 1962")
                        , (110, "Deception Island")
                        , (111, "Indian 1960")
                        , (112, "Indonesian 1974")
                        , (113, "North Sahara 1959")
                        , (114, "Pulkovo 1942")
                        , (115, "S-42 (Pulkovo 1942)")
                        , (116, "S-JYSK")
                        , (117, "Voirol 1950")
                        , (118, "Average Terrestrial System 1977")
                        , (119, "Compensation Géodésique du Québec 1977")
                        , (120, "Finnish (KKJ)")
                        , (121, "Ordnance Survey of Ireland")
                        , (122, "Revised Kertau")
                        , (123, "Revised Nahrwan")
                        , (124, "GGRS 76 (Greece)")
                        , (125, "Nouvelle Triangulation de France")
                        , (126, "RT 90 (Sweden)")
                        , (127, "Geocentric Datum of Australia (GDA)")
                        , (128, "BJZ54 (A954 Beijing Coordinates)")
                        , (129, "Modified BJZ54")
                        , (130, "GDZ80")
                        , (131, "Local datum")
                        ])
  , referencesInt1 = [ "IS 50" ]
  , referencesM4   = []
  , range          = Nothing
  , remarks        = Just "All necessary information for conversion of geographic coordinates from most of the Geodetic Datums in the above list to WGS-84 is contained in the \"User=s Handbook on Datum Transformations involving WGS-84\", prepared by the US Defense Mapping Agency and which is available from the IHB as IHO Publication S-60 (English and French Versions), along with an associated standard datum transformation software on floppy disk called \"MADTRAN\". The resulting latitude and longitude offsets can be encoded in the attribute SHIPAM."
  , distinction    = Nothing
  }

hunits :: ObjectAttribute HUNITS
hunits = ObjectAttribute
  { name           = "Height/length units"
  , acronym        = HUNITS
  , code           = 96
  , value          = E (IntMap.fromList
                        [ (1, "metres")
                        , (2, "feet")
                        ])
  , definition     = Nothing
  , definitions    = [ ("metres", "heights/lengths are specified in metres (SI units of length).")
                     , ("feet", "heights/lengths are specified in feet (imperial units of length).")
                     ]
  , referencesInt1 = []
  , referencesM4   = []
  , range          = Nothing
  , remarks        = Just "This attribute encodes the units of measurement for heights and lengths, but not depths for which the attribute depth units (DUNITS) is used."
  , distinction    = Nothing
  }

punits :: ObjectAttribute PUNITS
punits = ObjectAttribute
  { name           = "Positional accuracy units"
  , acronym        = PUNITS
  , code           = 189
  , value          = E (IntMap.fromList
                        [ (1, "metres")
                        , (2, "degrees of arc")
                        , (3, "millimeters")
                        , (4, "feet")
                        , (5, "cables")
                        ])
  , definition     = Nothing
  , definitions    = [ ("metres", "Positional accuracy is specified in metres (SI units of positional accuracy).")
                     , ("degrees of arc", "Positional accuracy is specified in degrees of arc.")
                     , ("millimeters",  "Positional accuracy is specified in millimeters.")
                     , ("feet", "Positional accuracy is specified in feet (imperial units of positional accuracy).")
                     , ("cables", "a unit of distance originally equal to the length of a ship’s anchor cable, but now generally considered to be about 600 feet. In the British Navy it is 608 feet, or exactly one-tenth of a nautical mile. In the United States Navy it is 720 feet but is infrequently used. Sometimes called cable length. (IHO Dictionary, S-32, 5th Edition, 589).")
                     ]
  , referencesInt1 = []
  , referencesM4   = []
  , range          = Nothing
  , remarks        = Just "This attribute encodes the units for positional accuracy which may be different from the unit for coordinates. The latter is specified at the dataset level in the COUN subfield of the DSPM record."
  , distinction    = Nothing
  }

-- FIXME: definitions incomplete
verdat :: ObjectAttribute VERDAT
verdat = ObjectAttribute
  { name = "Vertical datum"
  , acronym        = VERDAT
  , code           = 185
  , value          = E (IntMap.fromList
                        [ (1, "Mean low water springs")
                        , (2, "Mean lower low water springs")
                        , (3, "Mean sea level")
                        , (4, "Lowest low water")
                        , (5, "Mean low water")
                        , (6, "Lowest low water springs")
                        , (7, "Approximate mean low water springs")
                        , (8, "Indian spring low water")
                        , (9, "Low water springs")
                        , (10, "Approximate lowest astronomical tide")
                        , (11, "Nearly lowest low water")
                        , (12, "Mean lower low water")
                        , (13, "Low water")
                        , (14, "Approximate mean low water")
                        , (15, "Approximate mean lower low water")
                        , (16, "Mean high water")
                        , (17, "Mean high water springs")
                        , (18, "High water")
                        , (19, "Approximate mean sea level")
                        , (20, "High water springs")
                        , (21, "Mean higher high water")
                        , (22, "Equinoctial spring low water")
                        , (23, "Lowest astronomical tide")
                        , (24, "Local datum")
                        , (25, "International Great Lakes Datum 1985")
                        , (26, "Mean water level")
                        , (27, "Lower low water large tide")
                        , (28, "Higher high water large tide")
                        , (29, "Nearly highest high water")
                        , (30, "Highest astronomical tide (HAT)")
                        ])
  , definition     = Nothing
  , definitions    = []
  , referencesInt1 = []
  , referencesM4   = []
  , range          = Nothing
  , remarks        = Just "This attribute is used to specify the datum to which both heights (vertical datum, see S-57 Part 3) and soundings (sounding datum, see S-57 Part 3) are referred."
  , distinction    = Nothing
  }
