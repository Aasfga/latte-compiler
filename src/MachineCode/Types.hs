module MachineCode.Types where

import Types


-- 
-- Definitions
-- 

data Section
  = Text
  | Data
  deriving (Eq, Ord)

data ValueLocation
  = RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | RSP
  | RBP
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | FrameOffset Index
  | LabeledMemory Label
  | Number Int

-- 
-- Instances
-- 
instance Show Section where
  show Text = ".text"
  show Data = ".data"

instance Show ValueLocation where
  show RAX = "rax"
  show RBX = "rbx"
  show RCX = "rcx"
  show RDX = "rdx"
  show RSI = "rsi"
  show RDI = "rdi"
  show RSP = "rsp"
  show RBP = "rbp"
  show R8 = "r8"
  show R9 = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"
  show R15 = "r15"
  show (FrameOffset index)
    | index < 0 = "[rbp - " ++ show (abs index) ++ "]"
    | otherwise = "[rbp + " ++ show index ++ "]"
  show (LabeledMemory label) = label
  show (Number x) = show x
-- 
-- Functions
-- 
getArgumentLocation :: Index -> ValueLocation
getArgumentLocation index
  | index == 0 = RDI
  | index == 1 = RSI
  | index == 2 = RDX
  | index == 3 = RCX
  | index == 4 = R8
  | index == 5 = R9
  | index >= 6 = let
      offset = (index - 4) * 8
    in
      FrameOffset offset
  | otherwise = error "Index must be greater than 0" 



