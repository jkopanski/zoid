{-# language DataKinds         #-}
{-# language FlexibleInstances #-}
{-# language GADTs             #-}
{-# language TypeInType        #-}

module Instructions where

import Clash.Prelude hiding (Word)
import Clash.Class.BitPack

import Opcodes
import Registers
import Types

type Immediate = Signed 32

data Instruction
  = Beq Register Register Immediate
  | Bne Register Register Immediate

data Instr a where
  -- Load opcode instructions
  --  :: rd       -> rs1      -> imm
  LB  :: Register -> Register -> Immediate -> Instr IOp
  LH  :: Register -> Register -> Immediate -> Instr IOp
  LW  :: Register -> Register -> Immediate -> Instr IOp
  LBU :: Register -> Register -> Immediate -> Instr IOp
  LHU :: Register -> Register -> Immediate -> Instr IOp
  -- LoadFp opcode
  -- Custom0
  -- Miscmem
  -- OpImm
  -- Auipc opcode instructions
  --    :: rd       -> imm
  AUIPC :: Register -> Immediate -> Instr UOp
  -- OpImm32 opcode instructions
  -- Vliw48a opcode instructions
  -- Store opcode instructions
  -- :: rs1      -> rs2      -> imm
  SB :: Register -> Register -> Immediate -> Instr SOp
  SH :: Register -> Register -> Immediate -> Instr SOp
  SW :: Register -> Register -> Immediate -> Instr SOp
  -- StoreFp opcode instructions
  -- Custom1 opcode instructions
  -- Amo opcode instructions
  -- Op opcode instructions
  --   :: rd       -> rs1      -> rs2
  ADD  :: Register -> Register -> Register -> Instr ROp
  SUB  :: Register -> Register -> Register -> Instr ROp
  SLL  :: Register -> Register -> Register -> Instr ROp
  SLT  :: Register -> Register -> Register -> Instr ROp
  SLTU :: Register -> Register -> Register -> Instr ROp
  XOR  :: Register -> Register -> Register -> Instr ROp
  SRL  :: Register -> Register -> Register -> Instr ROp
  SRA  :: Register -> Register -> Register -> Instr ROp
  OR   :: Register -> Register -> Register -> Instr ROp
  AND  :: Register -> Register -> Register -> Instr ROp

data Format
  = ROp
  | IOp
  | SOp
  | SBOp
  | UOp
  | UJOp
  deriving (Eq, Show)

type family InstrFormat (op :: (Opcode a)) :: Format where
  InstrFormat Load    = IOp
  InstrFormat LoadFp  = IOp
  -- to be extended elsewere
  -- InstrFormat Custom0 = 
  InstrFormat MiscMem = IOp
  InstrFormat OpImm   = IOp
  InstrFormat Auipc   = UOp
  InstrFormat OpImm32 = IOp
  -- InstrFormat Vliw48a
  InstrFormat Store   = SOp
  InstrFormat StoreFp = SOp
  -- InstrFormat Custom1 =
  -- InstrFormat Amo     =
  -- InstrFormat Op      =
  -- InstrFormat Lui =
  -- InstrFormat Op32 =
  -- -- InstrFormat Vliw60
  -- InstrFormat MAdd =
  -- InstrFormat MSub =
  -- InstrFormat NMSub =
  -- InstrFormat NMAdd =
  -- InstrFormat OpFp =
  -- InstrFormat Reserved0 =
  -- InstrFormat Custom2 =
  -- -- InstrFormat Vliw48b
  -- InstrFormat Branch =
  -- InstrFormat JalR =
  -- InstrFormat Reserved1 =
  -- InstrFormat Jal =
  -- InstrFormat System =
  -- InstrFormat Reserved2 =
  -- InstrFormat Custom3 =

-- data RInstr  = ROp  Register Register Register
-- data IInstr  = IOp  Register Register Immediate
-- data SInstr  = SOp  Register Register Immediate
-- data SBInstr = SBOp Register Register Immediate
-- data UInstr  = UOp  Register Immediate
-- data UJInstr = UJOp Register Immediate

instance BitPack (Instr IOp) where
  type BitSize (Instr IOp) = 32

  unpack w = undefined
    -- let op = opcode w :: Opcode IType
    --  in case op of
    --   _ ->  LB 1 1 1

  pack = undefined

opcode
  :: (BitSize (Opcode a) ~ 7, BitPack (Opcode a))
  => Word -> Opcode a
opcode = unpack . slice d6 d0
