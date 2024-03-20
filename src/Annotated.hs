module Annotated where

import qualified Prelude as C (Eq, Ord, Show, Read, Bool, Double, Integer, String)
import qualified Data.String
import qualified Javalette.Abs as Abs

newtype Prog = Program [TopDef]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TopDef = FnDef Abs.Type Abs.Ident [Abs.Arg] Blk
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Blk = Block [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- data AStm
--     = SExp Abs.Type Exp
--     | SDecls Abs.Type [Abs.Id]
--     | SInit Abs.Type Abs.Id Exp
--     | SReturn Abs.Type Exp
--     | SWhile Exp Stm
--     | SBlock [Stm]
--     | SIfElse Exp Stm Stm
--   deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
    = Empty
    | BStmt Blk
    | Decl Abs.Type [Item]
    | Ass Abs.Type Abs.Ident Expr
    | Incr Abs.Type Abs.Ident
    | Decr Abs.Type Abs.Ident
    | Ret Abs.Type Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Abs.Type Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- data Stmt = Stmt Abs.Type Abs.Stmt
--   deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = EVar Abs.Ident
    | ELitInt C.Integer
    | ELitDoub C.Double
    | ELitTrue
    | ELitFalse
    | EApp Abs.Ident [Expr]
    | EString C.String
    | Neg Expr
    | Not Expr
    | EMul Abs.Type Expr Abs.MulOp Expr
    | EAdd Abs.Type Expr Abs.AddOp Expr
    | ERel Abs.Type Expr Abs.RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- data Exp
--     = EBool C.Bool
--     | EInt C.Integer
--     | EDouble C.Double
--     | EId Abs.Id
--     | EApp Abs.Id [Exp]
--     | EPost Abs.Id Abs.IncDecOp
--     | EPre Abs.IncDecOp Abs.Id
--     | EArith Abs.Type Exp ArithOp Exp
--     | ECmp Abs.Type Exp Abs.CmpOp Exp
--     | EAnd Exp Exp
--     | EOr Exp Exp
--     | EAss Abs.Id Exp
--     | EI2D Exp
--   deriving (C.Eq, C.Ord, C.Show, C.Read)

-- data Expr = Expr Abs.Type Abs.Expr
--   deriving (C.Eq, C.Ord, C.Show, C.Read)

data Item = NoInit Abs.Ident | Init Abs.Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)