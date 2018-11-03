module Registers.General
  ( Register (..) ) where

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

getReg :: Registers t -> Index 32 -> t
getReg = (!!)

putReg :: Index 32 -> t -> Registers t -> Registers t
putReg i v r = replace

modifyReg :: Registers t -> Index 32 -> (t -> t) -> Registers t 
modifyReg regs index f =
  let v = getReg regs index
   in replace index (f v) regs
