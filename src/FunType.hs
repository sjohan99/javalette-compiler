module FunType where

import Javalette.Abs

data FunType = FunType { funRet :: Type, funPars :: [Type] }
  deriving (Show, Eq, Ord)
