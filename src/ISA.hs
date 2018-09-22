module ISA where

import Clash.Prelude
import Clash.Sized.Unsigned

type Register    = BitVector 5
type Funct3      = BitVector 3
type Funct7      = BitVector 7
type Iimmediate  = BitVector 12
type Simmediate  = BitVector 12
type SBimmediate = BitVector 12
type Uimmediate  = BitVector 20
type UJimmediate = BitVector 20

-- type Register    = Unsigned 5
-- type Funct3      = Unsigned 3
-- type Funct7      = Unsigned 7
-- type Iimmediate  = Unsigned 12
-- type Simmediate  = Unsigned 12
-- type SBimmediate = Unsigned 12
-- type Uimmediate  = Unsigned 20
-- type UJimmediate = Unsigned 20

data Instruction
  = Rtype  Funct3 Funct7 Register Register Register
  | Itype  Funct3        Register Register          Iimmediate
  | Stype  Funct3        Register Register          Simmediate
  | SBtype Funct3        Register Register          SBimmediate
  | Utype                Register                   Uimmediate
  | UJtype               Register                   UJimmediate
-- data Instruction
--   = AUIPC Register UImmediate            -- AUIPC dest U-immedaite
--   | JAL Register (Unsigned 20)           -- JAL dest offset
--   | JALR Register Register (Unsigned 11) -- JALR dest base offset
--   | LUI Register UImmediate              -- LUI dest U-immediate
--   deriving (Show)
