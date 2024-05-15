module Annotated where

import qualified Prelude as C (Eq, Ord, Show, Read, Bool, Double, Integer, String)
import qualified Data.String
import qualified Javalette.Abs as Abs

newtype Prog = Program [TopDef]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TopDef = FnDef Abs.Type Abs.Ident [Abs.Arg] [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
    = Empty
    | BStmt [Stmt]
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
    | AssArr Abs.Type Indexed Expr
    | AssValArr Abs.Type FnCall IndexOp Expr
    | For Abs.Type Abs.Ident Expr Stmt
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = EVar Abs.Type Abs.Ident
    | ELitInt Abs.Type C.Integer
    | ELitDoub Abs.Type C.Double
    | ELitTrue Abs.Type
    | ELitFalse Abs.Type
    | EApp Abs.Type FnCall
    | EString Abs.Type C.String
    | Neg Abs.Type Expr
    | Not Abs.Type Expr
    | EMul Abs.Type Expr Abs.MulOp Expr
    | EAdd Abs.Type Expr Abs.AddOp Expr
    | ERel Abs.Type Expr Abs.RelOp Expr
    | EAnd Abs.Type Expr Expr
    | EOr Abs.Type Expr Expr
    | ENewArr Abs.Type IndexOp
    | EIndexed Abs.Type Indexed
    | ELen Abs.Type Expr Abs.Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Item = NoInit Abs.Ident | Init Abs.Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IndexOp = IndexOp Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Indexed = Indexed Expr IndexOp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FnCall = FnCall Abs.Ident [Expr]
  deriving (C.Eq, C.Ord, C.Show, C.Read)