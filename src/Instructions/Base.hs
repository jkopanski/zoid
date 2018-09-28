{-# language DataKinds    #-}
{-# language TypeFamilies #-}
module Instructions.Base where

import Instructions.Common

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
