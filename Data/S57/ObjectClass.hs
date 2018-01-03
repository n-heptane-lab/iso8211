{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.S57.ObjectClass where

import Data.Coerce
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16)

data Acronym
  = ADMARE
  | AIRARE
  | ACHBRT
  | M_HDAT
  | M_VDAT
  | AnyAcronym
    deriving (Eq, Ord, Show)

data AttributeA
  = CATACH
  | CATAIR
  | CONDTN
  | CONVIS
  | DATEND
  | DATSTA
  | HORDAT
  | JRSDTN
  | NATION
  | NOBJNM
  | OBJNAM
  | PEREND
  | PERSTA
  | RADIUS
  | RESTRN
  | STATUS
  | VERDAT
    deriving (Eq, Ord, Show)

data AttributeB
  = INFORM
  | NINFOM
  | NTXTDS
  | PICREP
  | SCAMAX
  | SCAMIN
  | TXTDSC
    deriving (Eq, Ord, Show)

data AttributeC
  = RECDAT
  | RECIND
  | SORDAT
  | SORIND
    deriving (Eq, Ord, Show)

data ObjectClass (acronym :: Acronym) = ObjectClass
  { name           :: Text
  , acronym        :: Acronym
  , code           :: Word16
  , attributeA     :: Set AttributeA
  , attributeB     :: Set AttributeB
  , attributeC     :: Set AttributeC
  , definition     :: Text
  , referencesInt1 :: [Text]
  , referencesM4   :: [Text]
  , remarks        :: Maybe Text
  , distinction    :: Maybe Text
  }
  deriving (Eq, Show)

untag :: ObjectClass (a :: Acronym) -> ObjectClass AnyAcronym
untag oc = coerce oc

{-
 :: ObjectClass
 = ObjectClass
  { name           = ""
  , acronym        = 
  , code           = 
  , attributeA     = Set.fromList [  ]
  , attributeB     = Set.fromList []
  , attributeC     = Set.fromList []
  , definition     = ""
  , referencesInt1 = [  ]
  , referencesM4   = [  ]
  , remarks        = 
  , distinction    = 
  }
-}

admare :: ObjectClass ADMARE
admare = ObjectClass
  { name           = "Administration Area (Named)"
  , acronym        = ADMARE
  , code           = 1
  , attributeA     = Set.fromList [ JRSDTN, NATION, NOBJNM, OBJNAM ]
  , attributeB     = Set.fromList [ INFORM, NINFOM, NTXTDS, PICREP, SCAMAX, SCAMIN, TXTDSC ]
  , attributeC     = Set.fromList [ RECDAT, RECIND, SORDAT, SORIND ]
  , definition     = "A defined (and possibly named) administrative area."
  , referencesInt1 = [  ]
  , referencesM4   = [  ]
  , remarks        = Nothing
  , distinction    = Just "land region; contiguous zone; continental shelf area; exclusive economic zone; fishery zone; territorial sea area;"
  }

airare :: ObjectClass AIRARE
airare = ObjectClass
  { name           = "Airport/airfield"
  , acronym        = AIRARE
  , code           = 2
  , attributeA     = Set.fromList [ CATAIR, CONDTN, CONVIS, NOBJNM, OBJNAM, STATUS ]
  , attributeB     = Set.fromList []
  , attributeC     = Set.fromList []
  , definition     = "An area containing at least one runway, used for landing, take-off, and movement of aircraft."
  , referencesInt1 = [ "ID 17" ]
  , referencesM4   = [ "366" ]
  , remarks        = Nothing
  , distinction    = Just "runway; sea-plane landing area;"
  }

achbrt :: ObjectClass ACHBRT
achbrt = ObjectClass
  { name           = "Anchor berth"
  , acronym        = ACHBRT
  , code           = 3
  , attributeA     = Set.fromList [ CATACH, DATEND, DATSTA, NOBJNM, OBJNAM, PEREND, PERSTA, RADIUS, STATUS ]
  , attributeB     = Set.fromList [ INFORM, NINFOM, NTXTDS, SCAMAX, SCAMIN, TXTDSC ]
  , attributeC     = Set.fromList [ RECDAT, RECIND, SORDAT, SORIND ]
  , definition     = "A designated area of water where a single vessel, sea plane, etc... may anchor."
  , referencesInt1 = [ "IN 11.1-2" ]
  , referencesM4   = [ "431.2" ]
  , remarks        = Just "In general the anchor berth is defined by the centre point and a swinging circle."
  , distinction    = Just "anchorage area; berth; mooring/warping facility;"
  }

m_hdat :: ObjectClass M_HDAT
m_hdat = ObjectClass
  { name           = "Horizontal datum of data"
  , acronym        = M_HDAT
  , code           = 303
  , attributeA     = Set.fromList [ HORDAT ]
  , attributeB     = Set.fromList [ INFORM, NINFOM, NTXTDS, TXTDSC ]
  , attributeC     = Set.fromList [ RECDAT, RECIND, SORDAT, SORIND ]
  , definition     = "An area of uniform horizontal datum."
  , referencesInt1 = [  ]
  , referencesM4   = [  ]
  , remarks        = Nothing
  , distinction    = Just "horizontal datum shift parameters;"
  }

m_vdat :: ObjectClass M_VDAT
m_vdat = ObjectClass
  { name           = "Vertical datum of data"
  , acronym        = M_VDAT
  , code           = 312
  , attributeA     = Set.fromList [ VERDAT ]
  , attributeB     = Set.fromList [ INFORM, NINFOM, NTXTDS, TXTDSC ]
  , attributeC     = Set.fromList [ RECDAT, RECIND, SORDAT, SORIND ]
  , definition     = "An area of uniform vertical datum."
  , referencesInt1 = [  ]
  , referencesM4   = [  ]
  , remarks        = Nothing
  , distinction    = Just "sounding datum;"
  }
