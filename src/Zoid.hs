{-# language
    DuplicateRecordFields
  , GeneralizedNewtypeDeriving
  , TypeApplications
  #-}
module Zoid where

import Clash.Prelude hiding (read)
import Config
import Instructions
import Types

class Arithmetic n where
  add :: BitVector n -> BitVector n -> Processor (BitVector n)

-- class Logic a where
--   a == a :: Bool
--   a /= a :: Bool
--   not a :: a



-- class Control a where
--   jump :: a

-- class Load a where
--   load :: a

-- class Store a where
--   store :: a

decode :: BitVector 32 -> ISA
decode = undefined

-- instance Instruction IInstr where
--   type Args IInstr = IFormat

-- class Instruction i where
--   type Args i :: * -> Format  
--   execute :: i -> ProcM ()
   
topEntity
  :: ISA
  -> Processor ()
topEntity = execute
