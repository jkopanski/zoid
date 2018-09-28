{-# language DataKinds  #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeInType #-}
{-# language DuplicateRecordFields #-}
module Zoid where

import Clash.Prelude
import Control.Monad.State
import Data.Kind (Type)
import Registers
import Types

data Config = Config
  { registers :: Vec 32 (BitVector 32)
  }

newtype Processor a = Processor { process :: State Config a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState Config
           )

class Arithmetic n where
  add :: BitVector n -> BitVector n -> Processor (BitVector n)

-- class Logic a where
--   a == a :: Bool
--   a /= a :: Bool
--   not a :: a

class Control a where
  jump :: a

class Load a where
  load :: a

class Store a where
  store :: a

type Immediate = BitVector 32


decode :: BitVector 32 -> ISA
decode = undefined

-- instance Instruction IInstr where
--   type Args IInstr = IFormat

-- class Instruction i where
--   type Args i :: * -> Format  
--   execute :: i -> ProcM ()
   
topEntity
  :: BitVector 7
  -> BitVector 7
topEntity = id
