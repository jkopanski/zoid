{-# language
    DuplicateRecordFields
  , GeneralizedNewtypeDeriving
  #-}
module Config where

import Clash.Prelude hiding (Word)
import Control.Applicative
import Control.Monad.State
import Instructions
import Registers
import Types

data Config = MkConfig
  { registers :: Registers Word
  }
  
newtype Processor a = Processor { process :: State Config a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState Config
           )

-- class Monad m => RegFile m where
--   read :: Register -> m (BitVector 32)
--   read2 :: Register -> Register -> m (BitVector 32, BitVector 32)
--   read2 ra rb = do
--     a <- read ra
--     b <- read rb
--     pure (a, b)
--   store :: BitVector 32 -> Register -> m ()

-- instance RegFile Processor where
--   read reg = do
--     regs <- gets registers
--     pure (regs !! reg)
--   store val reg = do
--     regs <- gets registers
--     let new = replace reg val regs   
--     modify (\s -> s { registers = new })

execute :: ISA -> Processor ()
execute (ADD args) = do
  regs <- gets registers
  let newRegs = withArgsR args regs $ \a b ->
        bitCoerce (toUnsigned a + toUnsigned b)
  modify (\s -> s { registers = newRegs })

execute (SUB args) = do
  regs <- gets registers
  let newRegs = withArgsR args regs $ \a b ->
        bitCoerce (toUnsigned a - toUnsigned b)
  modify (\s -> s { registers = newRegs })

