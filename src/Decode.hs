{-# LANGUAGE RankNTypes #-}

module Decode where

import CLaSH.Prelude hiding (Word, bit)

-- Decoding is much like parsing
newtype Decoder a = Decoder
  { decode :: forall n. (KnownNat n, KnownNat (BitSize a))
           => BitVector (n + BitSize a) -> [(a, BitVector n)]
  }

data Opcode = Load | Store deriving (Eq, Show, Enum)

instance BitPack Opcode where
  type BitSize Opcode = 7

  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

runDecoder :: forall a n. (KnownNat n, KnownNat (BitSize a))
           => Decoder a -> BitVector (n + BitSize a) -> a
runDecoder d b =
  let [(res, bits)] = decode d b
   in if size# bits /= 0
         then error "did not consume all bits"
         else res

bit :: Decoder Bit
bit = Decoder $ \v ->
  if size# v == 0
     then []
     else [split v]

opcode :: Decoder Opcode
opcode = undefined

-- runDecoder :: Decoder 
-- instructionTypes = [ (Load,       rtype)
--                    , (LoadFp,     rtype)
--                    , (Custom0,    rtype)
--                    , (MiscMem,    rtype)
--                    , (OpImm,      rtype)
--                    , (Auipc,      rtype)
--                    , (OpImm32,    rtype)
--                    , (VLIW48a,    rtype)
--                    , (Store,      rtype)
--                    , (StoreFp,    rtype)
--                    , (Custom1,    rtype)
--                    , (Amo,        rtype)
--                    , (Op,         rtype)
--                    , (Lui,        rtype)
--                    , (Op32,       rtype)
--                    , (VLIW64,     rtype)
--                    , (MAdd,       rtype)
--                    , (MSub,       rtype)
--                    , (NMSub,      rtype)
--                    , (NMAdd,      rtype)
--                    , (OpFp,       rtype)
--                    , (Rederved0,  rtype)
--                    , (Custom2,    rtype)
--                    , (VLIW48b,    rtype)
--                    , (Branch,     rtype)
--                    , (JalR,       rtype)
--                    , (Reserved1,  rtype)
--                    , (Jal,       ujType)
--                    , (System,     rtype)
--                    , (Reserved2,  rtype)
--                    , (Custom3,    rtype)
--                    , (VLIW80,     rtype)
--                    ]

-- fstSourceReg :: Word -> Register
-- fstSourceReg (W w) = slice d19 d15 w

-- sndSourceReg :: Word -> Register
-- sndSourceReg (W w) = slice d24 d20 w

-- destReg :: Word -> Register
-- destReg (W w) = slice d11 d7 w

-- getFunct3 :: Word -> Funct3
-- getFunct3 (W w) = slice d14 d12 w

-- getFunct7 :: Word -> Funct7
-- getFunct7 (W w) = slice d31 d25 w

-- getIimm :: Word -> Iimmediate
-- getIimm (W w) = slice d31 d20 w

-- getSimm :: Word -> Simmediate
-- getSimm (W w) = a ++# b
--   where a = slice d31 d25 w
--         b = slice d11 d7 w

-- getSBimm :: Word -> SBimmediate
-- getSBimm (W w) = a ++# b ++# c ++# d
--   where a = msb w
--         b = w ! 7
--         c = slice d30 d25 w
--         d = slice d11 d8 w

-- getUimm :: Word -> Uimmediate
-- getUimm (W w) = slice d31 d12 w

-- getUJimm :: Word -> UJimmediate
-- getUJimm (W w) = a ++# b ++# c ++# d
--   where a = msb w
--         b = slice d19 d12 w
--         c = w ! 20
--         d = slice d30 d21 w

-- decode :: Word -> Instruction
-- decode bits@(W w) =
--   case aa of
--     0b11 -> decodeBase bits
--     otherwise -> error "Other instruction encoding lenghts are currently unsupported"
--   where aa = slice d1 d0 w

-- decodeBase :: Word -> Instruction
-- decodeBase = undefined
-- -- decodeBase :: BitVector 30 -> Instruction
-- -- decodeBase w =
-- --   let (rest, opcode) = split w :: (BitVector 25, BitVector 5)
-- --       op = toEnum $ fromIntegral $ toInteger opcode
-- --    in arguments op rest

-- -- let dest    = slice d4  d0
-- --     funct3  = slice d7  d5
-- --     source1 = slice d12 d8
-- --     source2 = slice d17 d13
-- --     funct7  = slice d18 d25 in
-- -- decodeR :: BitVector 25 -> Instruction
-- -- decodeR w = 

-- -- arguments :: Operation -> BitVector 25 -> Instruction
-- -- arguments Jal w = JAL (unpack dest) offset
-- --   where
-- --     (imm, dest) = split w :: (BitVector 20, BitVector 5)
-- --     imm3 = msb imm
-- --     imm2 = slice d7 d0 imm
-- --     imm1 = imm ! 8
-- --     imm0 = slice d18 d9 imm
-- --     offset = unpack $ imm3 ++# imm2 ++# imm1 ++# imm0
