module Registers.Floating
  ( FPRegister (..) ) where

import Prelude
import CLaSH.Class.BitPack

data FPRegister
  = FT0  -- FP temporaries
  | FT1
  | FT2
  | FT3
  | FT4
  | FT5
  | FT6
  | FT7
  | FS0  -- FP saved registers
  | FS1
  | FA0  -- FP arguments/return values
  | FA1
  | FA2  -- FP arguments
  | FA3
  | FA4
  | FA5
  | FA6
  | FA7
  | FS2  -- FP saved registers
  | FS3
  | FS4
  | FS5
  | FS6
  | FS7
  | FS8
  | FS9
  | FS10
  | FS11
  | FT8  -- FP temporaries
  | FT9
  | FT10
  | FT11
  deriving (Eq, Enum, Show)

instance BitPack FPRegister where
  type BitSize FPRegister = 5

  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum
