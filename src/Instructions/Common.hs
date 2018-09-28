module Instructions.Common where

data Format
  = R
  | I
  | S
  | B
  | U
  | J

data family Args (i :: Format)

data instance Args R = RArgs
  { rd  :: Register
  , rs1 :: Register
  , rs2 :: Register
  }

data instance Args I = IArgs
  { rd  :: Register
  , rs  :: Register
  , imm :: Immediate
  }

data instance Args U = UArgs
  { rd  :: Register
  , imm :: Immediate
  }

data instance Args S = SArgs
  { rd  :: Register
  , imm :: Immediate
  }
