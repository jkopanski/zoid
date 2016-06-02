module Decode where

import CLaSH.Prelude
import Types
import qualified Opcodes as OP
import InstructionSetArchitecture (ISA)

type DestinationReg = BitVector 5
type SourceReg = BitVector 5
type Immediate = BitVector

data Instruction = Instr (BitVector 32)
                 | Utype Opcode DestinationReg (Immediate 20)

decodeU :: Instruction -> ISA
decodeU (Utype opcode rd imm)
  | opcode == OP.jal = JAL rd imm

decode :: Instruction -> Command
decode instr@Utype{} = decodeU instr

