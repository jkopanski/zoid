module Decode where

import CLaSH.Prelude hiding (Word)
import Types
import ISA

decode :: Word -> Instruction
decode (W w) =
  let (rest, aa) = split w :: (BitVector 30, BitVector 2) in
  case aa of
    0b11 -> decodeWord rest
    otherwise -> error "Other instruction encoding lenghts are currently unsupported"

decodeWord :: BitVector 30 -> Instruction
decodeWord w =
  let (rest, opcode) = split w :: (BitVector 25, BitVector 5)
      op = toEnum $ fromIntegral $ toInteger opcode
   in arguments op rest


arguments :: Operation -> BitVector 25 -> Instruction
arguments Jal w = JAL (unpack dest) offset
  where
    (imm, dest) = split w :: (BitVector 20, BitVector 5)
    imm3 = msb imm
    imm2 = slice d7 d0 imm
    imm1 = imm ! 8
    imm0 = slice d18 d9 imm
    offset = unpack $ imm3 ++# imm2 ++# imm1 ++# imm0
