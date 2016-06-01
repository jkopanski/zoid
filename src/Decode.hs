module Decode where

import CLaSH.Prelude
import Types
import qualified Opcodes as OP

type DestinationReg = BitVector 5
type SourceReg = BitVector 5
type Immediate = BitVector

data Instruction = Instr (BitVector 32)
                    | Utype Opcode DestinationReg (Immediate 20)

data ISA = JAL (BitVector 5) (BitVector 20)

decodeU :: Instruction -> Command
decodeU (Utype opcode rd imm)
  | opcode == OP.jal = JAL rd imm

decode :: Instruction -> Command
decode instr@(Utype _ _ _) = decodeU instr

