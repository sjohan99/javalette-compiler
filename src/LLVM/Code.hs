{-# LANGUAGE LambdaCase #-}
module LLVM.Code where

import           Data.List (intercalate)
import           Numeric (showHex)
import           Data.Char (toUpper, isAsciiLower)
import qualified Annotated as A
import           Annotated (TopDef(..), Expr)
import           Javalette.Abs (Ident(..), Type)
import qualified Javalette.Abs as Abs
import           FunType

newtype Label = L Int
  deriving (Eq, Ord)
newtype Reg = R Int
  deriving (Eq, Ord)
newtype StrConst = S Int
  deriving (Eq, Ord)

data Fun = Fun Ident FunType
  deriving (Show, Eq, Ord)

class ToLLVM a where
  toLLVM :: a -> String

instance ToLLVM Reg where
  toLLVM (R i) = "%t" ++ show i

instance ToLLVM Label where
  toLLVM (L i) = "%lab" ++ show i

instance ToLLVM StrConst where
  toLLVM (S i) = "@.str" ++ show i

instance ToLLVM Ident where
  toLLVM (Ident s) = s

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
  toLLVM t@(Abs.Arr t') = arrayPointer t

arrayName :: Type -> String
arrayName t = "%arr.struct." ++ toLLVM (arrayValueType t) ++ "." ++ show (arrayDepth t) 

arrayPointer :: Type -> String
arrayPointer t = "%arr.ptr." ++ toLLVM (arrayValueType t) ++ "." ++ show (arrayDepth t) 

arrayDepth :: Type -> Int
arrayDepth (Abs.Arr t) = 1 + arrayDepth t
arrayDepth _ = 0

arrayValueType :: Type -> Type
arrayValueType (Abs.Arr t) = arrayValueType t
arrayValueType t = t

arrayElementType :: Type -> Type
arrayElementType (Abs.Arr t) = t

arrayGetElemFun :: Type -> String
arrayGetElemFun t = "@getElem." ++ toLLVM (arrayValueType t) ++ "." ++ show (arrayDepth t)

instance ToLLVM Abs.AddOp where
  toLLVM Abs.Plus = "add"
  toLLVM Abs.Minus = "sub"

instance ToLLVM Abs.MulOp where
  toLLVM Abs.Times = "mul"
  toLLVM Abs.Div   = "div"

intRelOp :: Abs.RelOp -> String
intRelOp Abs.LTH = "slt"
intRelOp Abs.LE  = "sle"
intRelOp Abs.GTH = "sgt"
intRelOp Abs.GE  = "sge"
intRelOp Abs.EQU = "eq"
intRelOp Abs.NE  = "ne"

doubRelOp :: Abs.RelOp -> String
doubRelOp Abs.LTH = "olt"
doubRelOp Abs.LE  = "ole"
doubRelOp Abs.GTH = "ogt"
doubRelOp Abs.GE  = "oge"
doubRelOp Abs.EQU = "oeq"
doubRelOp Abs.NE  = "one"

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
  = Load Type Reg Reg -- ^ Load value from second register to first register.
  | BrCond Reg Label Label -- ^ Branch to 1st label if register is true, otherwise branch to 2nd label.
  | Br Label -- ^ Branch to label.
  | Label Label -- ^ Define label.
  | Alloca Reg Type -- ^ Allocate memory on stack, address stored in register.
  | StoreInt Integer Reg -- ^ Store integer at address given by register.
  | StoreDouble Double Reg -- ^ Store double at address given by register.
  | StoreBool Bool Reg -- ^ Store boolean at address given by register.
  | Store Type Reg Reg -- ^ Store value of 1st register in memory location of 2nd register.
  | Add Reg Type Reg Abs.AddOp Reg -- ^ Add values of 2nd and 3rd register with result in 1st register.
  | Mul Reg Type Reg Abs.MulOp Reg -- ^ Multiply values of 2nd and 3rd register with result in 1st register.
  | Rel Reg Type Reg Abs.RelOp Reg -- ^ Compare values of 2nd and 3rd register with result in 1st register.
  | Inc Type Reg Reg -- ^ Add 1 to value of the second register with result in the first register.
  | Dec Type Reg Reg -- ^ Subtract 1 from value of r2 with result in r1.
  | Negate Reg Type Reg -- ^ Negate value of r2 with result in r1.
  | Call Reg Fun [Reg] -- ^ Call function with result stored in 1st register.
  | Return Type Reg -- ^ Return value in the register.
  | ReturnVoid -- ^ Return void.
  | Initialize Reg Type -- ^ Initialize register with default value.
  | LogicalNot Type Reg Reg -- ^ Invert boolean value in 2nd register with result in 1st register.
  | NewArray Type -- ^ Create LLVM structs for array of given type
  | Calloc Reg Reg Reg -- ^ Allocate memory with pointer to it in r1, r2 is # of object, r3 is size of each object.
  | SizeOf Reg Reg Type
  | CreateGetElemFun Type
  | CallGetElem Type Reg Reg Reg -- ^ r1 = getElem(r2, r3)
  | GetElementPointer Type Reg Reg Reg -- ^ r1 = getelementptr r2, r3
  | Unreachable -- ^ Unreachable instruction.
  | FunHeader Ident Type [Abs.Arg] -- ^ Define function i.e "define rtype @id(args...) {"
  | FunFooter -- ^ End of function: "}"
  | Comment String -- ^ Make a comment in the code from the string.
  | StringConst StrConst String -- ^ Define a string constant.
  | LoadStr Reg StrConst String -- ^ Load string constant into register.
  | Blank -- ^ Blank line.
  deriving (Eq, Ord)

instance ToLLVM Code where
  toLLVM (Load t r1 r2) = toLLVM r1 ++ " = load " ++ toLLVM t ++ ", " ++ toLLVM t ++ "* " ++ toLLVM r2
  toLLVM (BrCond r l1 l2) = "br i1 " ++ toLLVM r ++ ", label " ++ toLLVM l1 ++ ", label " ++ toLLVM l2
  toLLVM (Br l) = "br label " ++ toLLVM l
  toLLVM (Label (L i)) = "lab" ++ show i ++ ":"
  toLLVM (Alloca r t) = toLLVM r ++ " = alloca " ++ toLLVM t
  toLLVM (StoreInt i r) = "store i32 " ++ show i ++ ", i32* " ++ toLLVM r
  toLLVM (StoreDouble d r) = "store double " ++ show d ++ ", double* " ++ toLLVM r
  toLLVM (StoreBool b r) = "store i1 " ++ show (fromEnum b) ++ ", i1* " ++ toLLVM r
  toLLVM (Store t r1 r2) = "store " ++ toLLVM t ++ " " ++ toLLVM r1 ++ ", " ++ toLLVM t ++ "* " ++ toLLVM r2
  toLLVM (Add r1 t r2 op r3) = toLLVM r1 ++ " = " ++ addOpWithPrefix op t ++ " " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", " ++ toLLVM r3
  toLLVM (Mul r1 t r2 op r3) = toLLVM r1 ++ " = " ++ mulOpWithPrefix op t ++ " " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", " ++ toLLVM r3
  toLLVM (Rel r1 t@Abs.Doub r2 op r3) = toLLVM r1 ++ " = fcmp " ++ doubRelOp op ++ " " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", " ++ toLLVM r3
  toLLVM (Rel r1 t r2 op r3) = toLLVM r1 ++ " = icmp " ++ intRelOp op ++ " " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", " ++ toLLVM r3
  toLLVM (Inc t@Abs.Doub r1 r2) = toLLVM r1 ++ " = fadd " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", 1.0"
  toLLVM (Inc t r1 r2) = toLLVM r1 ++ " = add " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", 1"
  toLLVM (Dec t@Abs.Doub r1 r2) = toLLVM r1 ++ " = fsub " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", 1.0"
  toLLVM (Dec t r1 r2) = toLLVM r1 ++ " = sub " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", 1"
  toLLVM (Negate r1 t@Abs.Doub r2) = toLLVM r1 ++ " = fneg " ++ toLLVM t ++ " " ++ toLLVM r2
  toLLVM (Negate r1 t r2) = toLLVM r1 ++ " = sub " ++ toLLVM t ++ " 0, " ++ toLLVM r2
  toLLVM (Call r f rs) = prefix ++ "call " ++ toLLVM t ++ " @" ++ toLLVM id ++ "(" ++ args ++ ")"
      where (Fun id (FunType t ts)) = f
            args = intercalate ", " [toLLVM t ++ " " ++ toLLVM r | (t, r) <- zip ts rs]
            prefix = if t == Abs.Void then "" else toLLVM r ++ " = "
  toLLVM (LogicalNot t r1 r2) = toLLVM r1 ++ " = xor " ++ toLLVM t ++ " " ++ toLLVM r2 ++ ", 1"

  toLLVM (NewArray t) = unlines [structName ++ " = type {i32, [0 x " ++ toLLVM (arrayElementType t) ++ "]}", structPtr ++ " = type " ++ structName ++ "*"]
    where structName = arrayName t
          structPtr  = arrayPointer t

  toLLVM (Calloc r n s) = toLLVM r ++ " = call i8* @calloc(i32 " ++ toLLVM n ++ ", i32 " ++ toLLVM s ++ ")"

  toLLVM (SizeOf p s t) = unlines [
    toLLVM p ++ " = getelementptr " ++ toLLVM t ++ ", " ++ toLLVM t ++ "* null, i32 1",
    toLLVM s ++ " = ptrtoint " ++ toLLVM t ++ "* " ++ toLLVM p ++ " to i32"
    ]

  toLLVM (CreateGetElemFun t) = unlines $ map concat [
      ["define ", toLLVM (arrayElementType t), " ", arrayGetElemFun t, "(", arrayPointer t, " %a, ", "i32 %i", ")", " {"]
    , ["%p1 = getelementptr ", arrayName t, ", ", arrayPointer t, " %a, i32 0, i32 1, i32 %i"]
    , ["%p2 = load ", toLLVM (arrayElementType t), ", ", toLLVM (arrayElementType t), "* %p1"]
    , ["ret ", toLLVM (arrayElementType t), " %p2"]
    , ["}"]
    ]

  toLLVM (GetElementPointer t r1 r2 r3) = toLLVM r1 ++ " = getelementptr " ++ arrayName t ++ ", " ++ arrayPointer t ++ " " ++ toLLVM r2 ++ ", i32 0, i32 1, i32 " ++ toLLVM r3

  toLLVM (CallGetElem t r1 r2 r3) = toLLVM r1 ++ " = call " ++ toLLVM (arrayElementType t) ++ " " ++ arrayGetElemFun t ++ "(" ++ arrayPointer t ++ " " ++ toLLVM r2 ++ ", " ++ "i32 " ++ toLLVM r3 ++  ")"

  toLLVM (Return t r) = "ret " ++ toLLVM t ++ " " ++ toLLVM r
  toLLVM ReturnVoid = "ret void"
  toLLVM Unreachable = "unreachable"
  toLLVM (Initialize r t) = "store " ++ toLLVM t ++ " " ++ initialValue t ++ ", " ++ toLLVM t ++ "* " ++ toLLVM r
  toLLVM (FunHeader id rt as) = concat ["define ", toLLVM rt, " @", toLLVM id, "(", args, ")", " {"]
    where args = intercalate ", " [toLLVM t ++ " " ++ toLLVM (R i) | (Abs.Argument t _, i) <- zip as [0..]]
  toLLVM (StringConst name s) = toLLVM name ++ " = internal constant [" ++ show (length s + 1)  ++ " x i8] c\"" ++ stringToHexString s ++ "\\00\""
  toLLVM (LoadStr r name s) = toLLVM r ++ " = getelementptr [" ++ show (length s + 1) ++ " x i8], [" ++ show (length s + 1) ++ " x i8]* " ++ toLLVM name ++ ", i32 0, i32 0"
  toLLVM FunFooter = "}"
  toLLVM (Comment s) = "; " ++ s
  toLLVM Blank = ""

-- sizeOf :: Type -> Int
-- sizeOf Abs.Int  = 4
-- sizeOf Abs.Doub = 8
-- sizeOf Abs.Bool = 1
-- sizeOf (Abs.Arr t) = sizeOf $ arrayValueType t

arrayStructName :: Type -> String
arrayStructName t = "%arr.struct." ++ toLLVM t

initialValue :: Type -> String
initialValue Abs.Int  = "0"
initialValue Abs.Doub = "0.0"
initialValue Abs.Bool = "0"

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