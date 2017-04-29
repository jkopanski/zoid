{-# LANGUAGE FlexibleInstances  #-}

module Opcodes
  ( Opcode (..) ) where

import Prelude
import CLaSH.Class.BitPack

data RType
data R4Type
data IType
data SType
data SBType
data UType
data UJType

data family Opcode f

data instance Opcode IType
  = Load
  | LoadFp
  | MiscMem
  | OpImm
  | OpImm32
  | JalR
  | System
  deriving (Eq, Show)

instance Enum (Opcode IType) where
  fromEnum Load          = 0x03
  fromEnum LoadFp        = 0x07
  fromEnum MiscMem       = 0x0F
  fromEnum OpImm         = 0x13
  fromEnum OpImm32       = 0x1B
  fromEnum JalR          = 0x67
  fromEnum System        = 0x73
  toEnum 0x03 = Load
  toEnum 0x07 = LoadFp
  toEnum 0x0F = MiscMem
  toEnum 0x13 = OpImm
  toEnum 0x1B = OpImm32
  toEnum 0x67 = JalR
  toEnum 0x73 = System

instance BitPack (Opcode IType) where
  type BitSize (Opcode IType) = 7
  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

data instance Opcode UType
  = Auipc
  | Lui
  deriving (Eq, Show)

instance Enum (Opcode UType) where
  fromEnum Auipc         = 0x17
  fromEnum Lui           = 0x37
  toEnum 0x17 = Auipc
  toEnum 0x37 = Lui

instance BitPack (Opcode UType) where
  type BitSize (Opcode UType) = 7
  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

data instance Opcode UJType
  = Jal
  deriving (Eq, Show)

instance Enum (Opcode UJType) where
  fromEnum Jal           = 0x6F
  toEnum 0x6F = Jal

instance BitPack (Opcode UJType) where
  type BitSize (Opcode UJType) = 7
  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

data instance Opcode SType
  = Store
  | StoreFp
  deriving (Eq, Show)

instance Enum (Opcode SType) where
  fromEnum Store         = 0x23
  fromEnum StoreFp       = 0x27
  toEnum 0x23 = Store
  toEnum 0x27 = StoreFp

instance BitPack (Opcode SType) where
  type BitSize (Opcode SType) = 7
  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

data instance Opcode SBType
  = Branch
  deriving (Eq, Show)

instance Enum (Opcode SBType) where
  fromEnum Branch        = 0x63
  toEnum 0x63 = Branch

instance BitPack (Opcode SBType) where
  type BitSize (Opcode SBType) = 7
  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

data instance Opcode RType
  = Amo
  | Op
  | Op32
  | OpFp
  deriving (Eq, Show)

instance Enum (Opcode RType) where
  fromEnum Amo           = 0x2F
  fromEnum Op            = 0x33
  fromEnum Op32          = 0x3B
  fromEnum OpFp          = 0x53
  toEnum 0x2F = Amo
  toEnum 0x33 = Op
  toEnum 0x3B = Op32
  toEnum 0x53 = OpFp

instance BitPack (Opcode RType) where
  type BitSize (Opcode RType) = 7
  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

data instance Opcode R4Type
  = MAdd
  | MSub
  | NMSub
  | NMAdd
  deriving (Eq, Show)

instance Enum (Opcode R4Type) where
  fromEnum MAdd          = 0x43
  fromEnum MSub          = 0x47
  fromEnum NMSub         = 0x4B
  fromEnum NMAdd         = 0x4F
  toEnum 0x43 = MAdd
  toEnum 0x47 = MSub
  toEnum 0x4B = NMSub
  toEnum 0x4F = NMAdd

instance BitPack (Opcode R4Type) where
  type BitSize (Opcode R4Type) = 7
  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

-- Custom and reserved opcodes
-- don't know yet how to provide elegant mechanism for extending
-- data Opcode (f :: k) where
--   Custom0   :: f -> Opcode f
--   Vliw48a   :: f -> Opcode f
--   Custom1   :: f -> Opcode f
--   Vliw64    :: f -> Opcode f
--   Reserved0 :: f -> Opcode f
--   Custom2   :: f -> Opcode f
--   Vliw48b   :: f -> Opcode f
--   Reserved1 :: f -> Opcode f
--   Reserved2 :: f -> Opcode f
--   Custom3   :: f -> Opcode f
--   Vliw80    :: f -> Opcode f

-- instance Enum (Opcode f) where
--   fromEnum (Custom0 _)   = 0x0B
--   fromEnum (Vliw48a _)   = 0x1F
--   fromEnum (Custom1 _)   = 0x2B
--   fromEnum (Vliw64 _)    = 0x3F
--   fromEnum (Reserved0 _) = 0x57
--   fromEnum (Custom2 _)   = 0x5B
--   fromEnum (Vliw48b _)   = 0x5F
--   fromEnum (Reserved1 _) = 0x6B
--   fromEnum (Reserved2 _) = 0x77
--   fromEnum (Custom3 _)   = 0x7B
--   fromEnum (Vliw80 _)    = 0x7F
--   toEnum 0x0B = Custom0
--   toEnum 0x1F = Vliw48a
--   toEnum 0x2B = Custom1
--   toEnum 0x3F = Vliw64
--   toEnum 0x57 = Reserved0
--   toEnum 0x5B = Custom2
--   toEnum 0x5F = Vliw48b
--   toEnum 0x6B = Reserved1
--   toEnum 0x77 = Reserved2
--   toEnum 0x7B = Custom3
--   toEnum 0x7F = Vliv80
