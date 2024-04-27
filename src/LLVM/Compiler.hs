{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Compiler for C--, producing symbolic JVM assembler.

module LLVM.Compiler (
  compile
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Map (Map)
import Data.List (isPrefixOf)
import qualified Data.Map as Map

import Annotated
import FunType
import Control.Monad (mapM_)
import qualified Javalette.Abs as Abs
import Javalette.Abs (Ident(..), Type, Arg(..))
import LLVM.Code
import LLVM.Translator
import Javalette.Print (printTree)

type Sig = Map Ident Fun

-- Local variables

type Cxt      = [CxtBlock]
type CxtBlock = [(Ident, (Reg, Type))]

-- State

data St = St
  { sig :: Sig -- ^ Function signatures
  , cxt :: Cxt -- ^ Context
  , nextRegister :: Reg -- ^ Next register index.
  , nextLabel :: Label -- ^ Next jump label
  , nextString :: StrConst -- ^ Next string index
  , output :: Output -- ^ Reversed code list (last instruction at front)
  , stringConsts :: [Code]
  }

initSt :: Sig -> Int -> St
initSt s strIdx = St
  { sig = s
  , cxt = [[]]
  , nextRegister = R 0
  , nextLabel = L 0
  , nextString = S (strIdx, 0)
  , output = []
  , stringConsts = []
  }

type Output = [Code]

type Compile = State St

builtin :: [(Ident, Fun)]
builtin =
  [ (Ident "printInt",    Fun (Ident "printInt")    (FunType Abs.Void   [Abs.Int]))
  , (Ident "printDouble", Fun (Ident "printDouble") (FunType Abs.Void   [Abs.Doub]))
  , (Ident "readInt",     Fun (Ident "readInt")     (FunType Abs.Int    []))
  , (Ident "readDouble",  Fun (Ident "readDouble")  (FunType Abs.Doub   []))
  , (Ident "printString", Fun (Ident "printString") (FunType Abs.Void   [Abs.Str]))
  ]

-- | Entry point.
compile
  :: Prog -- ^ Type-annotated program.
  -> String  -- ^ Generated llvm source file content.
compile (Program defs) =
  unlines header ++ unlines (combine (map (compileDef sig0) (zip defs [0..])))
  where
  sig0 = Map.fromList $ builtin ++ map sigEntry defs
  sigEntry def@(FnDef _ id _ _) = (id, Fun id (funType def))
  header = [
      "declare void @printInt(i32)"
    , "declare void @printDouble(double)"
    , "declare void @printString(i8*)"
    , "declare i32 @readInt()"
    , "declare double @readDouble()"
    ]

combine :: [([String], [Code])] -> [String]
combine sc = map toLLVM cs ++ ss
  where (ss, cs) = combine' sc

combine' :: [([String], [Code])] -> ([String], [Code])
combine' = foldr f ([], [])
  where
  f (ss, cs) (ss', cs') = (ss ++ ss', cs ++ cs')

-- | Indent non-empty lines.
indent :: String -> String
indent s = if null s || ("lab" `isPrefixOf` s && last s == ':') then s else "\t" ++ s

compileDef :: Sig -> (TopDef, Int) -> ([String], [Code])
compileDef sig0 (def@(FnDef t id args ss), i) = (concat
  -- function header
  [ [ ""
    , toLLVM fun ++ " {"
    ]
  -- output code
  , map (indent . toLLVM) $ clean $ output st
  -- function footer
  , [ ""
    , "}"
    ]
  ], stringConsts st)
  where
   st = execState (compileFun id t args ss) $ initSt sig0 i
   fun = Fun id $ funType def

cleanEmptyLabels :: [Code] -> [Code]
cleanEmptyLabels [] = []
cleanEmptyLabels (Label _ : Label l : cs) = cleanEmptyLabels (Label l:cs)
cleanEmptyLabels [Label _] = []
cleanEmptyLabels (c:cs) = c : cleanEmptyLabels cs

cleanDeadCode :: [Code] -> [Code]
cleanDeadCode [] = []
cleanDeadCode (Br _ : r@(Return _ _) : cs) = cleanDeadCode (r:cs)
cleanDeadCode (c:cs) = c : cleanDeadCode cs

clean :: [Code] -> [Code]
clean = cleanEmptyLabels . reverse . cleanDeadCode

compileFun :: Ident -> Type -> [Arg] -> [Stmt] -> Compile ()
compileFun id _ args [] = do
  l <- newLabel
  emit $ Label l
  mapDeclsToNewVars args
  mapM_ compileStm [VRet] -- Add return void statement if not present

compileFun id t args ss = do
  l <- newLabel
  emit $ Label l
  mapDeclsToNewVars args
  case last ss of
    Ret t e -> mapM_ compileStm ss
    VRet    -> mapM_ compileStm ss
    _       -> mapM_ compileStm (ss) -- ++ [VRet]) -- Add return void statement if not present

mapDeclsToNewVars :: [Arg] -> Compile ()
mapDeclsToNewVars args = do
  mapM_ (\(Argument t x) -> newVar x t) args

compileStm :: Stmt -> Compile ()
compileStm s0 = do

  -- Output a comment with the statement to compile
  let top = printTree $ stmToAbs s0
  unless (null top) $ do
    blank
    mapM_ comment $ lines top

  -- Compile the statement
  case s0 of
    Ret t e -> do
      r <- compileExpr e
      emit $ Return t r

    CondElse e s1 s2 -> do
      r <- compileExpr e
      trueLabel  <- newLabel
      falseLabel <- newLabel
      doneLabel <- newLabel
      emit $ BrCond e r trueLabel falseLabel
      emit $ Label trueLabel
      inNewBlock $ compileStm s1
      emit $ Br doneLabel
      emit $ Label falseLabel
      inNewBlock $ compileStm s2
      emit $ Br doneLabel
      emit $ Label doneLabel

    BStmt ss -> do
      inNewBlock $ mapM_ compileStm ss

    SExp t e -> do
      compileExpr e
      return ()

    s -> error $ "unimplemented: " ++ show s

compileExpr :: Expr -> Compile Reg
compileExpr = \case

  e@(ELitInt t i) -> compileLiteral e t

  e@(ELitDoub t d) -> compileLiteral e t

  e@(ELitTrue t) -> compileLiteral e t

  e@(ELitFalse t) -> compileLiteral e t

  EString t s -> do
    sid <- newString
    r <- newRegister
    emit $ StringConst sid s
    emit $ LoadStr r sid s
    return r

  EAdd t e1 op e2 -> do
    r1 <- newRegister
    r2 <- compileExpr e1
    r3 <- compileExpr e2
    emit $ Add r1 t r2 op r3
    return r1

  EMul t e1 op e2 -> do
    r1 <- newRegister
    r2 <- compileExpr e1
    r3 <- compileExpr e2
    emit $ Mul r1 t r2 op r3
    return r1

  EApp t id es -> do
    rs <- mapM compileExpr es
    f <- lookupFun id
    r <- newRegister
    emit $ Call r f rs
    return r

  EVar t x -> do
    (r, _) <- lookupRegister x
    return r

  ERel t e1 op e2 -> do
    r1 <- newRegister
    r2 <- compileExpr e1
    r3 <- compileExpr e2
    emit $ Rel r1 op Abs.Int r2 r3
    return r1

  e -> error $ "unimplemented: " ++ show e

compileLiteral :: Expr -> Type -> Compile Reg
compileLiteral e t = do
  r1 <- newRegister
  r2 <- newRegister
  emit $ Alloca r1 t
  case e of
    ELitInt _ i -> emit $ StoreInt i r1
    ELitDoub _ d -> emit $ StoreDouble d r1
    ELitTrue _ ->  emit $ StoreBool True r1
    ELitFalse _ -> emit $ StoreBool False r1
    _ -> error "compileLiteral: not a literal"
  emit $ Load t r2 r1
  return r2

newRegister :: Compile Reg
newRegister = do
  (R n) <- gets nextRegister
  modify $ \st -> st { nextRegister = R (succ n) }
  return $ R n

newLabel :: Compile Label
newLabel = do
  l@(L i) <- gets nextLabel
  modify $ \st -> st { nextLabel = L (succ i) }
  return l

newString :: Compile StrConst
newString = do
  s@(S (i, j)) <- gets nextString
  modify $ \st -> st { nextString = S (i, succ j) }
  return s

inNewBlock :: Compile a -> Compile a
inNewBlock cont = do
  modify $ \st -> st { cxt = [] : cxt st }
  a <- cont
  modify $ \ st -> st { cxt = tail $ cxt st }
  return a

newVar :: Ident -> Type -> Compile Reg
newVar x t = do
  r <- newRegister
  modify $ \st@St{ cxt = (b:bs) } -> st { cxt = ((x,(r, t)) : b) : bs }
  return r

lookupRegister :: Ident -> Compile (Reg, Type)
lookupRegister x = loop . concat <$> gets cxt
  where
  loop [] = error $ "unbound variable: " ++ printTree x
  loop ((y,(r,t)) : bs)
    | x == y    = (r, t)
    | otherwise = loop bs

lookupFun :: Ident -> Compile Fun
lookupFun x = do
  m <- gets sig
  return $ Map.findWithDefault (error $ "unknown function " ++ printTree x) x m

emit :: Code -> Compile ()
emit (StringConst sid s) = do
  modify $ \st@St{ stringConsts = scs } -> st{ stringConsts = StringConst sid s : scs }

emit c = do
  modify $ \st@St{ output = cs } -> st{ output = c:cs }

comment :: String -> Compile ()
comment = emit . Comment

blank :: Compile ()
blank = emit Blank

funType :: TopDef -> FunType
funType (FnDef rt _ args _) = FunType rt $ map (\(Argument t _) -> t) args