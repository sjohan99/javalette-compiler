{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module LLVM.Compiler (
  compile
) where

import           Control.Monad.State
import           Data.Maybe
import           Data.Map (Map)
import           Data.List (isPrefixOf, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set (Set)
import           Annotated
import           FunType
import qualified Javalette.Abs as Abs
import           Javalette.Abs (Ident(..), Type, Arg(..))
import           LLVM.Code
import           Javalette.Print (printTree)

type Sig      = Map Ident Fun
type Cxt      = [CxtBlock]
type CxtBlock = [(Ident, (Reg, Type))]

data St = St
  { cxt :: Cxt -- ^ Context
  , nextRegister :: Reg -- ^ Next register index.
  , nextLabel :: Label -- ^ Next jump label
  , output :: [Code] -- ^ Reversed code list (last instruction at front)
  , sig :: Sig
  , stringConsts :: [Code]
  , nextString :: StrConst
  , arrayStructs :: Set Code
  }

initSt :: Sig -> St
initSt s = St {
    cxt = [[]]
  , nextRegister = R 0
  , nextLabel = L 0
  , output = []
  , sig = s
  , stringConsts = []
  , nextString = S 0
  , arrayStructs = Set.empty
  }

type Compile = State St

builtin :: [(Ident, Fun)]
builtin =
  [ (Ident "printInt",    Fun (Ident "printInt")    (FunType Abs.Void   [Abs.Int]))
  , (Ident "printDouble", Fun (Ident "printDouble") (FunType Abs.Void   [Abs.Doub]))
  , (Ident "readInt",     Fun (Ident "readInt")     (FunType Abs.Int    []))
  , (Ident "readDouble",  Fun (Ident "readDouble")  (FunType Abs.Doub   []))
  , (Ident "printString", Fun (Ident "printString") (FunType Abs.Void   [Abs.Str]))
  ]

externals :: [String]
externals = [
    "declare void @printInt(i32)"
  , "declare void @printDouble(double)"
  , "declare void @printString(i8*)"
  , "declare i32 @readInt()"
  , "declare double @readDouble()"
  , "declare i32* @calloc(i32, i32)"
  ]

-- | Entry point.
compile
  :: Prog -- ^ Type-annotated program.
  -> String  -- ^ Generated llvm source file content.
compile (Program defs) =
  unlines externals ++ unlines (strings ++ arrayTypes ++ code)
  where
  sig0 = Map.fromList $ builtin ++ map sigEntry defs
  sigEntry def@(FnDef _ id _ _) = (id, Fun id (funType def))
  st = execState (mapM_ compileFun defs) $ initSt sig0
  strings = map toLLVM (stringConsts st)
  arrayTypes = map toLLVM (Set.toList $ arrayStructs st)
  code = map (indent . toLLVM) $ clean $ output st

-- | Indent non-empty lines.
indent :: String -> String
indent s
  | null s = s
  | "lab" `isPrefixOf` s && last s == ':' = "\t" ++ s
  | "}" `isPrefixOf` s = s
  | "define" `isPrefixOf` s = s
  | otherwise = intercalate "\n" $ map ("\t\t" ++) $ lines s

cleanEmptyLabels :: [Code] -> [Code]
cleanEmptyLabels [] = []
cleanEmptyLabels (Label _ : Label l : cs) = cleanEmptyLabels (Label l:cs)
cleanEmptyLabels [Label _] = []
cleanEmptyLabels (Label l : FunFooter : cs) = cleanEmptyLabels (FunFooter:cs)
cleanEmptyLabels (Label l : Br _ : cs) = cleanEmptyLabels (Label l : Unreachable :cs)
cleanEmptyLabels (c:cs) = c : cleanEmptyLabels cs

cleanDeadCode :: [Code] -> [Code]
cleanDeadCode [] = []
cleanDeadCode (Br _ : r@(Return _ _) : cs) = cleanDeadCode (Unreachable:r:cs)
cleanDeadCode (Br _ : r@ReturnVoid : cs) = cleanDeadCode (Unreachable:r:cs)
cleanDeadCode (c:cs) = c : cleanDeadCode cs

clean :: [Code] -> [Code]
clean = cleanEmptyLabels . reverse . cleanDeadCode

compileFun :: TopDef -> Compile ()
compileFun (FnDef t id args ss) = do
  resetLocalState
  emit $ FunHeader id t args
  l <- newLabel
  emit $ Label l
  mapArgsToNewRegisters args
  case (t, lastStmt ss) of
    (_, [])        -> mapM_ compileStmt [VRet] -- Add return void statement for empty (void) functions
    (_, [Ret _ _]) -> mapM_ compileStmt ss
    (_, [VRet])    -> mapM_ compileStmt ss
    (Abs.Void, _)  -> mapM_ compileStmt (ss ++ [VRet]) -- Add return void statement if not present
    _              -> mapM_ compileStmt ss
  emit FunFooter
  where lastStmt [] = []
        lastStmt ss = [last ss]

mapArgsToNewRegisters :: [Arg] -> Compile ()
mapArgsToNewRegisters args = do
  regs <- replicateM (length args) newRegister
  mapM_ (\(Argument t x, r) -> do
    r' <- newVar x t
    emit $ Store t r r') (zip args regs)

compileItem :: Type -> Item -> Compile ()
compileItem t = \case
  NoInit id -> do
    r <- newVar id t
    return ()
  Init id e -> do
    r2 <- compileExpr e
    r1 <- newVar id t
    -- error $ show (toLLVM t)
    emit $ Store t r2 r1

compileIncDec :: Stmt -> Compile ()
compileIncDec s = do
  varPtr <- lookupRegister id
  oldVal <- newRegister
  emit $ Load t oldVal varPtr
  newVal <- newRegister
  emit $ op t newVal oldVal
  emit $ Store t newVal varPtr
  where
    (t, id, op) = case s of
      Incr t id -> (t, id, Inc)
      Decr t id -> (t, id, Dec)
      _ -> error "compileIncDec: not an increment or decrement"

compileStmt :: Stmt -> Compile ()
compileStmt = \case
    Empty -> return ()

    Ret t e -> do
      r <- compileExpr e
      emit $ Return t r

    VRet -> emit ReturnVoid

    Decl t items -> mapM_ (compileItem t) items

    Ass t id e -> do
      r1 <- compileExpr e
      r2 <- lookupRegister id
      emit $ Store t r1 r2

    AssArr t@(Abs.Arr t') (Indexed eArr (IndexOp eIdx)) eVal -> do
      rArr <- compileExpr eArr
      rIdx <- compileExpr eIdx
      rVal <- compileExpr eVal
      rPtr <- newRegister
      emit $ GetArrElementPointer t rPtr rArr rIdx
      emit $ Store t' rVal rPtr

    s@(Incr _ _) -> compileIncDec s

    s@(Decr _ _) -> compileIncDec s

    CondElse e s1 s2 -> do
      bool <- compileExpr e
      trueLabel  <- newLabel
      falseLabel <- newLabel
      doneLabel <- newLabel
      emit $ BrCond bool trueLabel falseLabel
      emit $ Label trueLabel
      inNewBlock $ compileStmt s1
      emit $ Br doneLabel
      emit $ Label falseLabel
      inNewBlock $ compileStmt s2
      emit $ Br doneLabel
      emit $ Label doneLabel

    Cond e s -> do
      bool <- compileExpr e
      trueLabel <- newLabel
      doneLabel <- newLabel
      emit $ BrCond bool trueLabel doneLabel
      emit $ Label trueLabel
      inNewBlock $ compileStmt s
      emit $ Br doneLabel
      emit $ Label doneLabel

    While e s -> do
      checkCondLabel <- newLabel
      loopLabel <- newLabel
      exitLabel <- newLabel
      emit $ Br checkCondLabel
      emit $ Label checkCondLabel
      bool <- compileExpr e
      emit $ BrCond bool loopLabel exitLabel
      emit $ Label loopLabel
      inNewBlock $ compileStmt s
      emit $ Br checkCondLabel
      emit $ Label exitLabel

    For elemType id e arrType s -> do
      checkLoop <- newLabel
      loop <- newLabel
      doneLabel <- newLabel
      rTmp <- newRegister

      -- get array
      rArr <- compileExpr e

      -- get array size
      rArrSize <- newRegister
      emit $ Len arrType rArr rTmp rArrSize

      -- set idx to 0
      rIdxStore <- newRegister
      emit $ Alloca rIdxStore Abs.Int
      emit $ StoreInt 0 rIdxStore

      -- check if loop is done 
      emit $ Br checkLoop
      emit $ Label checkLoop
      rIdxVal <- newRegister
      emit $ Load Abs.Int rIdxVal rIdxStore
      rIdxEQUSize <- newRegister
      emit $ Rel rIdxEQUSize Abs.Int rIdxVal Abs.GE rArrSize
      emit $ BrCond rIdxEQUSize doneLabel loop
      
      -- execute loop statement
      emit $ Label loop
      rElem <- newRegister
      emit $ CallGetElem arrType rElem rArr rIdxVal
      inNewBlock $ compileForStmt s id elemType rElem
      
      -- increment idx
      rNewIdxVal <- newRegister
      emit $ Inc Abs.Int rNewIdxVal rIdxVal
      emit $ Store Abs.Int rNewIdxVal rIdxStore
      emit $ Br checkLoop

      emit $ Label doneLabel

    BStmt ss -> do
      inNewBlock $ mapM_ compileStmt ss

    SExp t e -> do
      compileExpr e
      return ()

compileForStmt :: Stmt -> Ident -> Type -> Reg -> Compile ()
compileForStmt s id t rElem = do
  r <- newVar id t
  emit $ Store t rElem r
  compileStmt s

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
    r1 <- lookupRegister x
    r2 <- newRegister
    emit $ Load t r2 r1
    return r2

  ERel t e1 op e2 -> do
    r1 <- newRegister
    r2 <- compileExpr e1
    r3 <- compileExpr e2
    emit $ Rel r1 t r2 op r3
    return r1

  Neg t e -> do
    r1 <- newRegister
    r2 <- compileExpr e
    emit $ Negate r1 t r2
    return r1

  Not t e -> do
    r1 <- newRegister
    r2 <- compileExpr e
    emit $ LogicalNot t r1 r2
    return r1

  EOr t e1 e2 -> do
    e1TrueLabel <- newLabel
    e1FalseLabel <- newLabel
    doneLabel <- newLabel
    r1 <- newRegister
    emit $ Alloca r1 t

    r2 <- compileExpr e1
    emit $ BrCond r2 e1TrueLabel e1FalseLabel
    emit $ Label e1TrueLabel
    emit $ StoreBool True r1
    emit $ Br doneLabel

    emit $ Label e1FalseLabel
    r3 <- compileExpr e2
    emit $ Store t r3 r1
    emit $ Br doneLabel

    emit $ Label doneLabel
    r4 <- newRegister
    emit $ Load t r4 r1
    return r4

  EAnd t e1 e2 -> do
    e1TrueLabel <- newLabel
    e1FalseLabel <- newLabel
    doneLabel <- newLabel
    r1 <- newRegister
    emit $ Alloca r1 t
    r2 <- compileExpr e1
    emit $ BrCond r2 e1TrueLabel e1FalseLabel

    emit $ Label e1TrueLabel
    r3 <- compileExpr e2
    emit $ Store t r3 r1
    emit $ Br doneLabel

    emit $ Label e1FalseLabel
    emit $ StoreBool False r1
    emit $ Br doneLabel

    emit $ Label doneLabel
    r4 <- newRegister
    emit $ Load t r4 r1
    return r4

  ENewArr t idxOps -> do
    compileNewArrayTypes t idxOps
    regs <- mapM allocateNewArray (p t idxOps)
    return $ head regs

  EIndexed t (Indexed eArr (IndexOp eIdx)) -> do
    rArr <- compileExpr eArr
    rIdx <- compileExpr eIdx
    rResult <- newRegister
    emit $ CallGetElem t rResult rArr rIdx
    return rResult

  ELen _et arrType e _ -> do
    rArr <- compileExpr e
    rTmp <- newRegister
    rLen <- newRegister
    emit $ Len arrType rArr rTmp rLen
    return rLen

p :: Type -> [IndexOp] -> [(Type, IndexOp)]
p t@(Abs.Arr t') (i:is) = (t, i) : p t' is
p _ _ = []

compileNewArrayTypes :: Type -> [IndexOp] -> Compile ()
compileNewArrayTypes t@(Abs.Arr t') (i:is) = do
  emit $ NewArray t
  emit $ CreateGetElemFun t
  compileNewArrayTypes t' is

compileNewArrayTypes _ [] = return ()

sizeOf :: Type -> Compile Reg
sizeOf t = do
  p <- newRegister
  s <- newRegister
  emit $ SizeOf p s t
  return s

allocateNewArray :: (Type, IndexOp) -> Compile Reg
allocateNewArray (t, IndexOp e) = do
  -- allocate memory
  n <- compileExpr e
  s <- sizeOf t
  p <- newRegister
  emit $ Calloc p n s

  -- set array size value
  sizePtr <- newRegister
  emit $ GetArrSizePointer t sizePtr p
  emit $ Store Abs.Int n sizePtr

  return p

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
  s@(S i) <- gets nextString
  modify $ \st -> st { nextString = S (succ i) }
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
  emit $ Alloca r t
  case t of
    Abs.Arr _ -> return ()
    _ -> emit $ Initialize r t
  modify $ \st@St{ cxt = (b:bs) } -> st { cxt = ((x,(r, t)) : b) : bs }
  return r

lookupRegister :: Ident -> Compile Reg
lookupRegister x = loop . concat <$> gets cxt
  where
  loop [] = error $ "unbound variable: " ++ printTree x
  loop ((y,(r,t)) : bs)
    | x == y    = r
    | otherwise = loop bs

lookupFun :: Ident -> Compile Fun
lookupFun x = do
  m <- gets sig
  return $ Map.findWithDefault (error $ "unknown function " ++ printTree x) x m

resetLocalState :: Compile ()
resetLocalState = do
  modify $ \st -> st { nextRegister = R 0, nextLabel = L 0, cxt = [[]]}

emit :: Code -> Compile ()
emit (StringConst sid s) = do
  modify $ \st@St{ stringConsts = scs } -> st{ stringConsts = StringConst sid s : scs }

emit na@(NewArray t) = do
  modify $ \st@St{ arrayStructs = as } -> st{ arrayStructs = Set.insert na as }

emit fun@(CreateGetElemFun t) = do
  modify $ \st@St{ arrayStructs = as } -> st{ arrayStructs = Set.insert fun as }

emit c = do
  modify $ \st@St{ output = cs } -> st{ output = c:cs }

comment :: String -> Compile ()
comment = emit . Comment

blank :: Compile ()
blank = emit Blank

funType :: TopDef -> FunType
funType (FnDef rt _ args _) = FunType rt $ map (\(Argument t _) -> t) args