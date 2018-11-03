{-# language
    DuplicateRecordFields
  #-}
module Instructions where

import Clash.Prelude
import Types
import Registers

data Format
  = R
  | I
  | S
  | B
  | U
  | J

data family Args (i :: Format)

data instance Args R = RArgs
  { rd  :: Register
  , rs1 :: Register
  , rs2 :: Register
  }

data instance Args I = IArgs
  { rd  :: Register
  , rs  :: Register
  , imm :: Immediate
  }

data instance Args U = UArgs
  { rd  :: Register
  , imm :: Immediate
  }

data instance Args S = SArgs
  { rd  :: Register
  , imm :: Immediate
  }

withArgsR
  :: (BitPack s, BitPack t, BitSize s ~ BitSize t)
  => Args R      -- ^ instruction arguments
  -> Registers t -- ^ register file
  -> (  t           -- ^ value from rs1
     -> t           -- ^ value from rs2
     -> s           -- ^ result of operation 
     )
  -> Registers t
withArgsR (RArgs rd rs1 rs2) regs f = 
  let (a, b) = getRegs regs rs1 rs2
      res = bitCoerce (f a b)
   in putReg rd res regs 

data ISA
  -- Load opcode instructions
  = LB  (Args I)
  | LH  (Args I)
  | LW  (Args I)
  | LBU (Args I)
  | LHU (Args I)
  -- LoadFp opcode
  -- Custom0
  -- Miscmem
  -- OpImm
  -- Auipc opcode instructions
  | AUIPC (Args U)
  | LUI   (Args U)
  -- OpImm32 opcode instructions
  -- Vliw48a opcode instructions
  -- Store opcode instructions
  | SB (Args S)
  | SH (Args S)
  | SW (Args S)
  -- StoreFp opcode instructions
  -- Custom1 opcode instructions
  -- Amo opcode instructions
  -- Op opcode instructions
  | ADD  (Args R)
  | SUB  (Args R)
  | SLL  (Args R)
  | SLT  (Args R)
  | SLTU (Args R)
  | XOR  (Args R)
  | SRL  (Args R)
  | SRA  (Args R)
  | OR   (Args R)
  | AND  (Args R)
