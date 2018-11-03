{-# language
    GeneralizedNewtypeDeriving
  , UndecidableInstances
  #-}
module Types where

import Clash.Prelude hiding (Word)

newtype Word = W { unWord :: BitVector 32 }
  deriving BitPack
newtype Halfword = HW { unHalfword :: BitVector 16 }
  deriving BitPack
newtype Byte = B { unByte :: BitVector 8 }
  deriving BitPack
newtype Nibble = NB { unNibble :: BitVector 4 }
  deriving BitPack

newtype Immediate = Imm { unImmediate :: BitVector 32 }
  deriving BitPack

type Parcel   = Halfword

-- To make types more specific
class (BitPack a) => ToNumber a where
  toSigned :: a -> Signed (BitSize a)
  toUnsigned :: a -> Unsigned (BitSize a)

instance ToNumber Word where
  toSigned = bitCoerce
  toUnsigned = bitCoerce

instance ToNumber Halfword where
  toSigned = bitCoerce
  toUnsigned = bitCoerce

instance ToNumber Byte where
  toSigned = bitCoerce
  toUnsigned = bitCoerce

instance ToNumber Nibble where
  toSigned = bitCoerce
  toUnsigned = bitCoerce

instance ToNumber Immediate where
  toSigned = bitCoerce
  toUnsigned = bitCoerce
