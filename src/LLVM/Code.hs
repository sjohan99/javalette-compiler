{-# LANGUAGE LambdaCase #-}
module LLVM.Code where

import Data.List (intercalate)
import Numeric (showHex)

import qualified Annotated as A
import Annotated (TopDef(..), Expr)
import Javalette.Abs (Ident(..), Type)
import qualified Javalette.Abs as Abs
import FunType
import Data.Char (toUpper, isAsciiLower)

newtype Label = L Int
newtype Reg = R Int
newtype StrConst = S (Int, Int)
type Addr  = Int

data Fun = Fun Ident FunType
  deriving (Show)

class ToLLVM a where
  toLLVM :: a -> String

instance ToLLVM Reg where
  toLLVM (R i) = "%t" ++ show i

instance ToLLVM Label where
  toLLVM (L i) = "%lab" ++ show i

instance ToLLVM StrConst where
  toLLVM (S (i, j)) = "@.str" ++ show i ++ "." ++ show j

instance ToLLVM Ident where
  toLLVM (Ident s) = s

instance ToLLVM Fun where
  toLLVM (Fun i (FunType rt ts)) = concat ["define ", toLLVM rt, " @", toLLVM i, "(", args, ")"]
    where args = intercalate ", " [toLLVM t ++ " " ++ toLLVM (R i) | (t, i) <- zip ts [0..]]

instance ToLLVM Abs.Arg where
  toLLVM (Abs.Argument t i) = toLLVM t ++ " " ++ toLLVM i

instance ToLLVM A.TopDef where
  toLLVM (FnDef t id args _) = concat ["define ", toLLVM t, " @", toLLVM id, "(", argsToLLVM, ")"]
    where argsToLLVM = intercalate ", " [argToLLVM a i | (a, i) <- zip args [0..]]
          argToLLVM (Abs.Argument t id) i = toLLVM t ++ " " ++ toLLVM (R i)


instance ToLLVM Type where
  toLLVM Abs.Int  = "i32"
  toLLVM Abs.Doub = "double"
  toLLVM Abs.Bool = "i1"
  toLLVM Abs.Void = "void"
  toLLVM Abs.Str  = "i8*"

instance ToLLVM Abs.AddOp where
  toLLVM Abs.Plus = "add"
  toLLVM Abs.Minus = "sub"

instance ToLLVM Abs.MulOp where
  toLLVM Abs.Times = "mul"
  toLLVM Abs.Div   = "div"

instance ToLLVM Abs.RelOp where
  toLLVM Abs.LTH = "slt"
  toLLVM Abs.LE  = "sle"
  toLLVM Abs.GTH = "sgt"
  toLLVM Abs.GE  = "sge"
  toLLVM Abs.EQU = "eq"
  toLLVM Abs.NE  = "ne"

instance ToLLVM Expr where
  toLLVM = \case
    A.EVar t _ -> toLLVM t
    A.ELitInt t _ -> toLLVM t
    A.ELitDoub t _ -> toLLVM t
    A.ELitTrue t -> toLLVM t
    A.ELitFalse t -> toLLVM t
    A.EApp t _ _ -> toLLVM t
    A.EString t _ -> toLLVM t
    A.Neg t _ -> toLLVM t
    A.Not t _ -> toLLVM t
    A.EMul t _ _ _ -> toLLVM t
    A.EAdd t _ _ _ -> toLLVM t
    A.ERel t _ _ _ -> toLLVM t
    A.EAnd t _ _ -> toLLVM t
    A.EOr t _ _ -> toLLVM t

data Code
  = Load Type Reg Reg -- ^ Load from first register to second register.
  | BrCond Expr Reg Label Label
  | Br Label
  | Label Label
  | Alloca Reg Type
  | StoreInt Integer Reg
  | StoreDouble Double Reg
  | StoreBool Bool Reg
  | Add Reg Type Reg Abs.AddOp Reg
  | Mul Reg Type Reg Abs.MulOp Reg
  | Rel Reg Abs.RelOp Type Reg Reg
  | Call Reg Fun [Reg]
  | Return Type Reg
  | Comment String
  | StringConst StrConst String
  | LoadStr Reg StrConst String
  | Blank

instance ToLLVM Code where
  toLLVM (Load t r1 r2) = toLLVM r1 ++ " = load " ++ toLLVM t ++ ", " ++ toLLVM t ++ "* " ++ toLLVM r2
  toLLVM (BrCond e r l1 l2) = "br i1 " ++ toLLVM r ++ ", label " ++ toLLVM l1 ++ ", label " ++ toLLVM l2
  toLLVM (Br l) = "br label " ++ toLLVM l
  toLLVM (Label (L i)) = "lab" ++ show i ++ ":"
  toLLVM (Alloca r t) = toLLVM r ++ " = alloca " ++ toLLVM t
  toLLVM (StoreInt i r) = "store i32 " ++ show i ++ ", i32* " ++ toLLVM r
  toLLVM (StoreDouble d r) = "store double " ++ show d ++ ", double* " ++ toLLVM r
  toLLVM (StoreBool b r) = "store i1 " ++ show (fromEnum b) ++ ", i1* " ++ toLLVM r
  toLLVM (Add r1 t r2 op r3) = toLLVM r1 ++ " = " ++ toLLVM op ++ " " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", " ++ toLLVM r3
  toLLVM (Mul r1 t r2 op r3) = toLLVM r1 ++ " = " ++ toLLVM op ++ " " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", " ++ toLLVM r3
  toLLVM (Rel r1 op t r2 r3) = toLLVM r1 ++ " = icmp " ++ toLLVM op ++ " " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", " ++ toLLVM r3
  toLLVM (Call r f rs) = prefix ++ "call " ++ toLLVM t ++ " @" ++ toLLVM id ++ "(" ++ args ++ ")"
      where (Fun id (FunType t ts)) = f
            args = intercalate ", " [toLLVM t ++ " " ++ toLLVM r | (t, r) <- zip ts rs]
            prefix = if t == Abs.Void then "" else toLLVM r ++ " = "
  toLLVM (Return t r) = "ret " ++ toLLVM t ++ " " ++ toLLVM r
  toLLVM (StringConst name s) = toLLVM name ++ " = internal constant [" ++ show (length s + 1)  ++ " x i8] c\"" ++ stringToHexString s ++ "\\00\""
  toLLVM (LoadStr r name s) = toLLVM r ++ " = getelementptr [" ++ show (length s + 1) ++ " x i8], [" ++ show (length s + 1) ++ " x i8]* " ++ toLLVM name ++ ", i32 0, i32 0"
  toLLVM (Comment s) = "; " ++ s
  toLLVM Blank = ""

addOpWithPrefix :: Abs.AddOp -> Type -> String
addOpWithPrefix op t = arithPrefix t ++ toLLVM op

mulOpWithPrefix :: Abs.MulOp -> Type -> String
mulOpWithPrefix Abs.Div Abs.Doub = "fdiv"
mulOpWithPrefix Abs.Div Abs.Int  = "sdiv"
mulOpWithPrefix Abs.Mod Abs.Int  = "srem"
mulOpWithPrefix op t = arithPrefix t ++ toLLVM op

arithPrefix :: Type -> String
arithPrefix Abs.Doub = "f"
arithPrefix Abs.Int  = ""

stringToHexString :: String -> String
stringToHexString = concatMap toHex
  where toHex :: Char -> String
        toHex c = "\\" ++ hex c
        hex c = case showHex (fromEnum c) "" of
          [a, b] -> [maybeUpper a, maybeUpper b]
          [a]    -> ['0', maybeUpper a]
        maybeUpper c = if isAsciiLower c then toUpper c else c

-- | Idealized JVM instructions.
-- Some idealized instructions (e.g., @Inc Type_double@)
-- decompose into several real JVM instructions.
-- data Code
--   = Store   Type  Addr        -- ^ Store stack content of type @Type@ in local variable @Addr@.
--   | Load    Type  Addr        -- ^ Push stack content of type @Type@ from local variable @Addr@.
--   | IConst  Integer           -- ^ Put integer constant on the stack.
--   | DConst  Double            -- ^ Put floating point constant on the stack.
--   | Dup     Type              -- ^ Duplicate top of stack.
--   | Pop     Type              -- ^ Pop something of type @Type@ from the stack.
--   | Return  Type              -- ^ Return from method of type @Type@.
--   | Call    Fun               -- ^ Call function.
--   | Label   Label             -- ^ Define label.
--   | Goto    Label             -- ^ Jump to label.
--   | If      CmpOp Label       -- ^ If top of stack is @'op' 0@, jump to label.
--   | IfCmp   Type  CmpOp Label -- ^ If prev "op' top, jump.
--   | DCmp                      -- ^ If prev > top, then 1, if prev == top, 0, if prev < top, -1.
--                               -- In case we do not care about NaN, this can be printed to either
--                               -- @dcmpg@ or @dcmpl@.
--   | Inc     Type  Addr Int    -- ^ In/decrease variable by small number
--   | Arith   Type  A.ArithOp     -- ^ Add/multiply 2 top values of stack.
--                               -- Subtract/divide top of stack from previous of stack.
--   | I2D                       -- ^ Convert top of the stack int to double.
--   | Comment String            -- ^A comment (not including ";;"). @null@ for blank line.
--   | Blank
--   deriving Show

-- toJVM :: Code -> String
-- toJVM = \case
--   Store t n
--     | n <= 3     -> prefixL t ++ "store_"  ++ show n
--     | otherwise -> prefixL t ++ "store "  ++ show n
--   Load  t n 
--     | n <= 3     -> prefixL t ++ "load_"  ++ show n
--     | otherwise -> prefixL t ++ "load "  ++ show n
--   Return t  -> prefixL t ++ "return"
--   Call f    -> "invokestatic " ++ idFromFun f ++ "(" ++ paramTypesFromFun f ++ ")" ++ retTypeFromFun f
--   DConst d  -> "ldc2_w " ++ show d
--   IConst i
--     | i == -1                 -> "iconst_m1"
--     | 0      <= i && i <= 5   -> "iconst_" ++ show i
--     | (-128) <= i && i <= 127 -> "bipush " ++ show i
--     | otherwise               -> "ldc " ++ show i
--   Dup Type_double -> "dup2"
--   Dup _           -> "dup"
--   Pop Type_double -> "pop2"
--   Pop _           -> "pop"
--   If op l
--     | op  == OLt   -> "iflt L" ++ show l
--     | op  == OGt   -> "ifgt L" ++ show l
--     | op  == OLtEq -> "ifle L" ++ show l
--     | op  == OGtEq -> "ifge L" ++ show l
--     | op  == ONEq  -> "ifne L" ++ show l
--     | op  == OEq   -> "ifeq L" ++ show l
--   IfCmp t op l
--     | op  == ONEq  -> "if_icmpne L" ++ show l
--     | op  == OEq   -> "if_icmpeq L" ++ show l
--     | op  == OLt   -> "if_icmplt L" ++ show l
--     | op  == OGt   -> "if_icmpgt L" ++ show l
--     | op  == OLtEq -> "if_icmple L" ++ show l
--     | op  == OGtEq -> "if_icmpge L" ++ show l
--   DCmp             -> "dcmpg"
--   Goto l           -> "goto L" ++ show l
--   Inc t a k        -> "iinc " ++ show a ++ " " ++ show k
--   Arith t op       -> case op of
--     A.OTimes -> prefixL t ++ "mul"
--     A.ODiv   -> prefixL t ++ "div"
--     A.OPlus  -> prefixL t ++ "add"
--     A.OMinus -> prefixL t ++ "sub"
--   Label l          -> "L" ++ show l ++ ":"
--   I2D             -> "i2d"
--   Comment s       -> ";; " ++ s
--   Blank           -> ""

-- prefix :: Type -> String
-- prefix Type_double = "D"
-- prefix Type_int    = "I"
-- prefix Type_bool   = "Z"
-- prefix Type_void   = ""

-- prefixL :: Type -> String
-- prefixL Type_double = "d"
-- prefixL Type_int    = "i"
-- prefixL Type_bool   = "i"
-- prefixL Type_void   = ""

-- class Size a where
--   size :: a -> Int

-- instance Size Type where
--   size Type_int = 1
--   size Type_double = 2
--   size Type_bool = 1
--   size Type_void = 0

-- instance Size Id where
--   size id = 0

-- instance Size a => Size [a] where
--   size = sum . map size

-- instance Size FunType where 
--   size (FunType t ts) = size ts - size t 

-- instance (Size a, Size b) => Size(a,b) where
--      size (x,y) = size x + size y

-- instance Size Fun where
--   size (Fun _ ft) = size ft