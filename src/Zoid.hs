module Zoid where

import Clash.Prelude
import Instructions

-- topEntity :: Signal (BitVector 32)
-- topEntity = decode

topEntity
  :: BitVector 32
  -> Instr IOp
topEntity = unpack
