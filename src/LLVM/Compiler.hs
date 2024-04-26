{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Compiler for C--, producing symbolic JVM assembler.

module LLVM.Compiler where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Annotated
import FunType
import Control.Monad (mapM_)
import qualified Javalette.Abs as Abs
import Javalette.Abs (Ident(..), Type, Arg(..))
import LLVM.Code
import LLVM.Translator

type Sig = Map Ident Fun

-- Local variables

type Cxt      = [CxtBlock]
type CxtBlock = [(Ident, (Reg, Type))]

-- State

data St = St
  { sig :: Sig -- ^ Function signatures
  , cxt :: Cxt -- ^ Context
  , nextRegister :: Int -- ^ Next register index.
  , nextLabel :: Label -- ^ Next jump label
  , output :: Output -- ^ Reversed code list (last instruction at front)
  }

initSt :: Sig -> Int -> St
initSt s i = St
  { sig = s
  , cxt = [[]]
  , nextRegister = i -- Begin at i (0..i is reserved for function arguments)
  , nextLabel = 0
  , output = []
  }

type Output = [Code]

type Compile = State St

builtin :: [(Ident, FunType)]
builtin =
  [ (Ident "printInt",    Fun (Ident "printInt")    (FunType Type_void   [Type_int]))
  , (Ident "printDouble", Fun (Ident "printDouble") (FunType Type_void   [Type_double]))
  , (Ident "readInt",     Fun (Ident "readInt")     (FunType Type_int    []))
  , (Ident "readDouble",  Fun (Ident "readDouble")  (FunType Type_double []))
  ]

-- | Entry point.
compile
  :: String  -- ^ Class name.
  -> Prog -- ^ Type-annotated program.
  -> String  -- ^ Generated llvm source file content.
compile name (Program defs) =
  unlines (concatMap (compileDef sig0) defs)
  where
  sig0 = Map.fromList $ builtin ++ map sigEntry defs
  sigEntry def@(FnDef _ id _ _) = (id, Fun id (funType def))

-- | Indent non-empty lines.
indent :: String -> String
indent s = if null s || (head s == 'L' && last s == ':') then s else "\t" ++ s

compileDef :: Sig -> TopDef -> [String]
compileDef sig0 def@(FnDef t id args ss) = concat
  -- function header
  [ [ ""
    , toLLVM fun ++ " {"
    ]
  -- output code
  , map (indent . toJVM) $ reverse $ output st
  -- function footer
  , [ ""
    , "}"
    ]
  ]
  where
   st = execState (compileFun f t args ss) $ initSt sig0 (length args)
   fun = Fun f $ funType def

compileFun :: Ident -> Type -> [Arg] -> [Stmt] -> Compile ()
compileFun id _ args [] = do
  mapDeclsToNewVars args
  mapM_ compileStm [VRet] -- Add return void statement if not present

compileFun id t args ss = do
  mapDeclsToNewVars args
  case last ss of
    Ret t e -> mapM_ compileStm ss
    VRet    -> mapM_ compileStm ss
    _       -> mapM_ compileStm (ss ++ [VRet]) -- Add return void statement if not present

mapDeclsToNewVars :: [Arg] -> Compile ()
mapDeclsToNewVars args = do
  rs <- mapM (\(Argument t x) -> newVar x t) args
  mapM_ (\(Argument t _, r) -> emit $ Alloca r t) $ zip args rs

compileStm :: Stmt -> Compile ()
compileStm s0 = do

  -- Output a comment with the statement to compile
  let top = printTree $ T.stmToAbs s0
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
      emit $ Br e r trueLabel falseLabel
      emit $ Label trueLabel
      inNewBlock $ compileStm s1
      emit $ Label falseLabel
      inNewBlock $ compileStm s2

    -- SInit t x e -> do
    --   newVar x t
    --   compileExp e
    --   (a, _) <- lookupVar x
    --   emit $ Store t a

    -- SExp t e -> do
    --   compileExp e
    --   emit $ Pop t

    -- SReturn t e -> do
    --   compileExp e
    --   emit $ Return t

    -- SDecls t ids -> do
    --   mapM_ (`newVar` t) ids

    -- SBlock ss -> do
    --   inNewBlock $ mapM_ compileStm ss

    -- SIfElse e s1 s2 -> do
    --     ifLabel   <- newLabel
    --     elseLabel <- newLabel
    --     compileExp e
    --     emit $ If OEq elseLabel
    --     inNewBlock $ compileStm s1
    --     emit (Goto ifLabel)
    --     emit (Label elseLabel)
    --     inNewBlock $ compileStm s2
    --     emit (Label ifLabel)

    -- SWhile e s -> do
    --   startLabel <- newLabel
    --   trueLabel  <- newLabel
    --   falseLabel <- newLabel
    --   emit $ Label startLabel
    --   compileExp e
    --   emit $ If OEq falseLabel
    --   emit $ Label trueLabel
    --   inNewBlock $ compileStm s
    --   emit $ Goto startLabel
    --   emit $ Label falseLabel

compileExpr :: Expr -> Compile Reg
compileExpr = \case

  ELitInt i -> do
    r1 <- newRegister
    r2 <- newRegister
    emit $ Alloca r1 Abs.Int
    emit $ StoreInt i r1
    emit $ Load Abs.Int r1 r2
    return r2

  EAdd t e1 op e2 -> do
    r1 <- compileExpr e1
    r2 <- compileExpr e2
    emit $ Add t r1 op r2

  EApp id es -> do
    rs <- mapM compileExpr es
    f <- lookupFun id
    r <- newRegister
    emit $ Call r f rs

  EDouble d -> emit $ DConst d

  EBool b -> emit $ if b then IConst 1 else IConst 0

  EId x -> do
    (a, t) <- lookupVar x
    emit $ Load t a

  EApp x es -> do
    mapM_ compileExp es
    f <- lookupFun x
    emit $ Call f

  EPost id op -> do
    (a, t) <- lookupVar id
    emit $ Load t a
    case op of
      OInc -> do emit $ Inc t a 1
      ODec -> do emit $ Inc t a (-1)

  EPre op id -> do
    (a, t) <- lookupVar id
    case op of
      OInc -> do emit $ Inc t a 1
      ODec -> do emit $ Inc t a (-1)
    emit $ Load t a

  EArith t e1 op e2 -> do
    compileExp e1
    compileExp e2
    emit $ Arith t op

  ECmp t e1 cOp e2 -> do
    trueLabel <- newLabel
    emit $ IConst 1
    compileExp e1
    compileExp e2
    emit $ IfCmp t cOp trueLabel
    emit $ Pop Type_int
    emit $ IConst 0
    emit $ Label trueLabel

  EAnd e1 e2 -> do
    trueLabel  <- newLabel
    falseLabel <- newLabel
    compileExp e1
    emit $ If OEq falseLabel
    compileExp e2
    emit $ If OEq falseLabel
    emit $ IConst 1
    emit $ Goto trueLabel
    emit $ Label falseLabel
    emit $ IConst 0
    emit $ Label trueLabel

  EOr e1 e2 -> do
    trueLabel <- newLabel
    endLabel  <- newLabel
    compileExp e1
    emit $ If ONEq trueLabel
    compileExp e2
    emit $ If ONEq trueLabel
    emit $ IConst 0
    emit $ Goto endLabel
    emit $ Label trueLabel
    emit $ IConst 1
    emit $ Label endLabel

  EAss x e -> do
    compileExp e
    (a, t) <- lookupVar x
    emit $ Store t a
    emit $ Load  t a

  EI2D e -> do
    compileExp e
    emit I2D

newRegister :: Compile Reg
newRegister = do
  n <- gets nextRegister
  modify $ \st -> st { nextRegister = succ n }
  return $ R n

newLabel :: Compile Label
newLabel = do
  l <- gets nextLabel
  modify $ \st -> st { nextLabel = succ l }
  return l

inNewBlock :: Compile a -> Compile a
inNewBlock cont = do
  modify $ \st -> st { cxt = [] : cxt st }
  a <- cont
  modify $ \ st -> st { cxt = tail $ cxt st }
  return a

newVar :: Ident -> Type -> Compile Reg
newVar x t = do
  r <- newRegister
  modify $ \st@St{ cxt = (b:bs) } -> st { cxt = ((x,(R r, t)) : b) : bs }
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
  return $ Map.findWithDefault (error $ "unknown function" ++ printTree x) x m

emit :: Code -> Compile ()

emit (Store Type_void _) = return ()
emit (Load Type_void _)  = return ()
emit (Dup Type_void)   = return ()
emit (Pop Type_void)   = return ()

emit (Inc t@Type_double a k) = do
  emit $ Load t a
  emit $ DConst $ fromIntegral k
  emit $ Arith t OPlus
  emit $ Store t a

emit (IfCmp Type_double o l) = do
  emit DCmp
  emit $ If o l

emit c = do
  modify $ \st@St{ output = cs } -> st{ output = c:cs }
  adjustStack c

comment :: String -> Compile ()
comment = emit . Comment

blank :: Compile ()
blank = emit Blank

funType :: TopDef -> FunType
funType (FnDef rt _ args _) = FunType rt $ map (\(Argument t _) -> t) args