module Opcodes where

import CLaSH.Prelude
import Types
import ISA

xlen :: Int
xlen = 32

data Operation = Load
               | LoadFp
               | Custom0
               | MiscMem
               | OpImm
               | Auipc
               | OpImm32
               | NoUse0
               | Store
               | StoreFp
               | Custom1
               | Amo
               | Op
               | Lui
               | Op32
               | NoUse1
               | MAdd
               | MSub
               | NMSub
               | NMAdd
               | OpFp
               | Rederved0
               | Custom2
               | NoUse2
               | Branch
               | JalR
               | Reserved1
               | Jal
               | System
               | Reserved2
               | Custom3
               | NoUse3
               deriving (Show, Eq, Ord, Enum, Bounded)

-- | Class BitEnum defines is similiar to Enum but operates on clash BitVector.
class Opcode a where
  toOpcode :: BitVector 7 -> a
  fromOpcode :: a -> BitVector 7

instance Opcode Operation where
  fromOpcode = fromInteger . toInteger . (+ 3) . (4 *) . fromEnum
  toOpcode   n = toEnum $ fromInteger $ div (toInteger n) 4

data RType = R { rFunct7 :: BitVector 7
               , rRs2    :: BitVector 5
               , rRs1    :: BitVector 5
               , rFunct3 :: BitVector 3
               , rRd     :: BitVector 5
               , rOpcode :: Operation
               }

data UType = U { uImm3   :: Bit
               , uImm2   :: BitVector 11
               , uImm1   :: BitVector 5
               , uimm0   :: BitVector 3
               , uRd     :: BitVector 5
               , uOpcode :: Operation
               }

data UJType = UJ { ujImm3   :: Bit
                 , ujImm0   :: BitVector 10
                 , ujImm1   :: Bit
                 , ujImm2   :: BitVector 8
                 , ujRd     :: BitVector 5
                 , ujOpcode :: Operation
                 }

decodeUJ :: UJType -> ISA
decodeUJ i =
  case ujOpcode i of
    Jal -> JAL (fromInteger $ toInteger $ ujRd i) offset
      where offset = fromInteger $ toInteger $ ujImm3 i
                   ++# ujImm2 i
                   ++# ujImm1 i
                   ++# ujImm0 i

-- load      = 0b0000011
-- loadfp    = 0b0000111
-- custom0   = 0b0001011
-- miscmem   = 0b0001111
-- opimm     = 0b0010011
-- auipc     = 0b0010111
-- opimm32   = 0b0011011
-- store     = 0b0100011
-- storefp   = 0b0100111
-- custom1   = 0b0101011
-- amo       = 0b0101111
-- op        = 0b0110011
-- lui       = 0b0110111
-- op32      = 0b0111011
-- madd      = 0b1000011
-- msub      = 0b1000111
-- nmsub     = 0b1001011
-- nmadd     = 0b1001111
-- opfp      = 0b1010011
-- reserved0 = 0b1010111
-- custom2   = 0b1011011
-- branch    = 0b1100011
-- jalr      = 0b1100100
-- reserved1 = 0b1101011
-- jal       = 0b1101111
-- system    = 0b1110011
-- reserved2 = 0b1110111
-- custom3   = 0b1111011

