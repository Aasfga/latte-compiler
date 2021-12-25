{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.Definitions.Quadruples where

import Lens.Micro.Platform hiding (element)
import qualified Data.Map as Map
import Types

data Quadruple
  = NOP

data QuadruplesCode 
  = Quadruples {
    _functions :: Map.Map String Function
  }

data Function
  = EmptyFunction
  | Function {
    _block :: [Block]
  }

type Block = [Quadruple]

$(makeLenses ''QuadruplesCode)
$(makeLenses ''Function)