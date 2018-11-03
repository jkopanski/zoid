module Registers where

import Clash.Prelude
import Clash.Class.BitPack
import Clash.Sized.Index
import Types

data Register
  = Zero -- Hard-wired zero
  | RA   -- Return address
  | SP   -- Stack pointer
  | GP   -- Global pointer
  | TP   -- Thread pointer
  | T0   -- Temporaries
  | T1
  | T2
  | FP   -- Saved register/frame pointer
  | S1   -- Saved register
  | A0   -- Function arguments/return values
  | A1
  | A2   -- Function arguments
  | A3
  | A4
  | A5
  | A6
  | A7
  | S2   -- Saved register
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | T3   -- Temporaries
  | T4
  | T5
  | T6
  deriving (Eq, Enum, Show)

instance BitPack Register where
  type BitSize Register = 5

  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

type Registers t = Vec 32 t

getReg :: Registers t -> Register -> t
getReg regs reg = regs !! pack reg

getRegs :: Registers t -> Register -> Register -> (t, t)
getRegs regs i1 i2 = (getReg regs i1, getReg regs i2)

putReg :: Register -> t -> Registers t -> Registers t
putReg idx val regs = replace (pack idx) val regs 

modifyReg :: Registers t -> Register -> (t -> t) -> Registers t 
modifyReg regs idx f =
  let v = getReg regs idx
   in replace idx (f v) regs
