module Types where

import CLaSH.Prelude

data Word     = W  (BitVector 32)
data Halfword = HW (BitVector 16)
data Byte     = B  (BitVector  8)
data Nibble   = NB (BitVector  4)

type Parcel   = Halfword

data Operation = Load
               | LoadFp
               | Custom0
               | MiscMem
               | OpImm
               | Auipc
               | OpImm32
               | VLIW48a
               | Store
               | StoreFp
               | Custom1
               | Amo
               | Op
               | Lui
               | Op32
               | VLIW64
               | MAdd
               | MSub
               | NMSub
               | NMAdd
               | OpFp
               | Rederved0
               | Custom2
               | VLIW48b
               | Branch
               | JalR
               | Reserved1
               | Jal
               | System
               | Reserved2
               | Custom3
               | VLIW80
               deriving (Show, Eq, Enum, Ord, Bounded)
