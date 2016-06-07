module Opcodes where

import Data.Maybe (fromJust)

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
               | VLIW48a
               | Store
               | StoreFp
               | Custom1
               | Amo
               | Op
               | Lui
               | Op32
               | VLIW64
               | MAdd
               | MSub
               | NMSub
               | NMAdd
               | OpFp
               | Rederved0
               | Custom2
               | VLIW48b
               | Branch
               | JalR
               | Reserved1
               | Jal
               | System
               | Reserved2
               | Custom3
               | VLIW80
               deriving (Show, Eq, Enum, Ord, Bounded)

opcodes = [ (Load,        3)
          , (LoadFp,      7)
          , (Custom0,    11)
          , (MiscMem,    15)
          , (OpImm,      19)
          , (Auipc,      23)
          , (OpImm32,    27)
          , (NoUse0,     31)
          , (Store,      35)
          , (StoreFp,    39)
          , (Custom1,    43)
          , (Amo,        47)
          , (Op,         51)
          , (Lui,        55)
          , (Op32,       59)
          , (NoUse1,     63)
          , (MAdd,       67)
          , (MSub,       71)
          , (NMSub,      75)
          , (NMAdd,      79)
          , (OpFp,       83)
          , (Rederved0,  87)
          , (Custom2,    91)
          , (NoUse2,     95)
          , (Branch,     99)
          , (JalR,      103)
          , (Reserved1, 107)
          , (Jal,       111)
          , (System,    115)
          , (Reserved2, 119)
          , (Custom3,   123)
          , (NoUse3,    127)
          ]

-- instance Enum Operation where
--   fromEnum x = fromJust $ flip lookup opcodes x
--   toEnum = fromJust . flip lookup (fmap swap opcodes)
--     where swap (x, y) = (y, x)

-- | Class BitEnum defines is similiar to Enum but operates on clash BitVector.
-- class Enum a => Opcode a where
--   toOpcode :: BitVector 7 -> a
--   fromOpcode :: a -> BitVector 7

-- This solutions gets 'compiled out' ;)
-- newtype Opcode = Opcode Operation

-- instance Enum Opcode where
--   toEnum n = Opcode (toEnum $ subtract 3 $ div n 4)
--   fromEnum (Opcode x) = (+ 3) . (4 *) $ fromEnum x

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
               , uImm0   :: BitVector 3
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

data Instruction = BitVector 32
                | UType
                | UJType
                deriving Show

decode :: Instruction -> ISA
decode 
decodeU  :: UType  -> ISA
decodeU i =
  case uOpcode i of
    Auipc -> AUIPC dest imm
    Lui   -> LUI   dest imm
    where dest = fromInteger $ toInteger $ uRd i
          imm  = fromInteger $ toInteger $ uImm3 i
               ++# uImm2 i
               ++# uImm1 i
               ++# uImm0 i

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

