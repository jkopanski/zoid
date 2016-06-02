module InstructionSetArchitecture where

import CLaSH.Prelude

data ISA = JAL (BitVector 5) (BitVector 20)
