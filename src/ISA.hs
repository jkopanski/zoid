module ISA where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned

type Register = Unsigned 5
type UImmediate = Unsigned 20

data Instruction
  = AUIPC Register UImmediate            -- AUIPC dest U-immedaite
  | JAL Register (Unsigned 20)           -- JAL dest offset
  | JALR Register Register (Unsigned 11) -- JALR dest base offset
  | LUI Register UImmediate              -- LUI dest U-immediate
  deriving (Show)
