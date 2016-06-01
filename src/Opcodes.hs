module Opcodes where

import CLaSH.Prelude
import Types

load      = 0b0000011
loadfp    = 0b0000111
custom0   = 0b0001011
miscmem   = 0b0001111
opimm     = 0b0010011
auipc     = 0b0010111
opimm32   = 0b0011011
store     = 0b0100011
storefp   = 0b0100111
custom1   = 0b0101011
amo       = 0b0101111
op        = 0b0110011
lui       = 0b0110111
op32      = 0b0111011
madd      = 0b1000011
msub      = 0b1000111
nmsub     = 0b1001011
nmadd     = 0b1001111
opfp      = 0b1010011
reserved0 = 0b1010111
custom2   = 0b1011011
branch    = 0b1100011
jalr      = 0b1100100
reserved1 = 0b1101011
jal       = 0b1101111
system    = 0b1110011
reserved2 = 0b1110111
custom3   = 0b1111011

