{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeChecker where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Foldable (toList)
import           Data.List.NonEmpty (pattern (:|))
import qualified Data.List.NonEmpty as List1
import           Data.Maybe
import           Data.List (groupBy, sort)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Javalette.Abs
import           Javalette.Print (printTree, Print)
import qualified Annotated as A
import           FunType

type AnnotatedProgram = A.Prog
type TypeError = String
type Check = ReaderT Sig (StateT St (Except TypeError))
type Sig = Map Ident FunType
type Cxt   = List1.NonEmpty Block
type Block = Map Ident Type

data St = St
    { stCxt :: Cxt
    , stRet :: Type
    }

pt :: Print a => a -> String
pt = printTree

fmt :: [String] -> String
fmt = concatMap (++ " ")

initSt :: Type -> St
initSt = St $ Map.empty :| []

builtin :: [(Ident, FunType)]
builtin =
    [ (Ident "readInt"    , FunType Int  [])
    , (Ident "readDouble" , FunType Doub [])
    , (Ident "printInt"   , FunType Void [Int])
    , (Ident "printDouble", FunType Doub [Doub])
    , (Ident "printString", FunType Void [Str])
    ]

typecheck :: Prog -> Either TypeError A.Prog
typecheck prg@(Program defs) = do
    let sig = builtin ++ sourceFuncs defs
    assertHasMain sig
    assertNoDuplicateFuns sig
    assertNoStringReturns defs
    let st = error "no state yet"
    runExcept (evalStateT (runReaderT (checkProgram prg) (Map.fromList sig)) st)
    where sourceFuncs = map (\(FnDef t fId args _) -> (fId, FunType t (map (\(Argument t _) -> t) args)))
          assertHasMain s = unless ((Ident "main", FunType Int []) `elem` s) $ throwError "no main function"
          assertNoStringReturns defs = do
            let strRetFuncs = [id | FnDef t id _ _ <- defs, t == Str]
            unless (null strRetFuncs) $ throwError $ fmt ["function may not return type string:", pt (head strRetFuncs)]
          assertNoDuplicateFuns s = do
            let duplicates = filter (\x -> length x > 1) $ groupBy (\t1 t2 -> fst t1 == fst t2) $ sort s
            unless (null duplicates) $ throwError $ fmt ["duplicate function definition:", pt (fst (head $ head duplicates))]

checkProgram :: Prog -> Check A.Prog
checkProgram (Program defs) = do
    mapM_ checkReturn defs
    defs' <- mapM checkDef defs
    return (A.Program defs')

checkDef :: TopDef -> Check A.TopDef
checkDef (FnDef t fId args ss) = do
    put $ initSt t
    mapM_ (\(Argument t x) -> newVar x t) args
    ss' <- checkStmts ss
    return $ A.FnDef t fId args ss'

checkReturn :: TopDef -> Check ()
checkReturn (FnDef Void fId _ ss) = return ()
checkReturn (FnDef t fId _ ss) = if any hasReturn ss then
        return ()
    else
        throwError $ fmt ["Not every execution path returns in function:", pt fId]

hasReturn :: Stmt -> Bool
hasReturn = \case
    Ret _ -> True
    CondElse _ s1 s2 -> hasReturn s1 && hasReturn s2
    (BStmt ss) -> any hasReturn ss
    _ -> False

checkItems :: Type -> [Item] -> Check [A.Item]
checkItems t [] = return []
checkItems t (i:is) = do
    i' <- checkItem t i
    is' <- checkItems t is
    return $ i' : is'

checkItem :: Type -> Item -> Check A.Item
checkItem t = \case
    NoInit id -> do
        newVar id t
        return $ A.NoInit id
    Init id e -> do
        newVar id t
        e' <- checkExpr e t
        return $ A.Init id e'

checkStmt :: Stmt -> Check A.Stmt
checkStmt = \case
    Empty -> return A.Empty

    SExp e -> do
        case e of
            EApp _ _ -> do
                (t, e') <- inferExp e
                return $ A.SExp t e'
            _ -> throwError $ fmt ["Non-void expressions except for function calls are not allowed:", pt e]

    Ass id e -> do
        (t, e') <- inferExp e
        t' <- lookupVar id
        if t == t' then
                return $ A.Ass t id e'
            else
                throwError $ fmt ["Type mismatch:", pt e, ". Expected", pt t', ", got", pt t]

    Incr id -> do
        t <- lookupVar id
        unless (t == Int) $ throwError $ fmt ["Type mismatch. Cannot apply '++' to", pt t]
        return $ A.Incr t id

    Decr id -> do
        t <- lookupVar id
        unless (t == Int) $ throwError $ fmt ["Type mismatch. Cannot apply '--' to", pt t]
        return $ A.Decr t id

    Decl t items -> do
        items' <- checkItems t items
        return $ A.Decl t items'

    Ret e -> do
        (t, e') <- inferExp e
        t' <- gets stRet
        if t == t' then
                return $ A.Ret t e'
            else
                throwError $ fmt ["Type mismatch:", pt e, ". Expected", pt t', ", but function returns", pt t]

    VRet -> do
        t <- gets stRet
        unless (t == Void) $ throwError "Type mismatch: void return in non-void function"
        return A.VRet

    Cond e s -> do
        (t, e') <- inferExp e
        unless (t == Bool) $ throwError $ fmt ["Type mismatch:", pt e, ". Expected boolean, got", pt t]
        s' <- checkStmt $ blockify s
        return $ A.Cond e' s'

    CondElse e s1 s2 -> do
        (t, e') <- inferExp e
        unless (t == Bool) $ throwError $ fmt ["Type mismatch:", pt e, ". Expected boolean, got ", pt t]
        s1' <- checkStmt $ blockify s1
        s2' <- checkStmt $ blockify s2
        return $ A.CondElse e' s1' s2'

    While e s -> do
        (t, e') <- inferExp e
        unless (t == Bool) $ throwError $ fmt ["Type mismatch:", pt e, ". Expected boolean, got", pt t]
        s' <- checkStmt $ blockify s
        return $ A.While e' s'

    BStmt ss -> do
        sret <- gets stRet
        bs <- gets stCxt
        put $ St (Map.empty :| toList bs) sret
        ss' <- checkStmts ss
        put $ St bs sret
        return $ A.BStmt ss'

    where blockify (BStmt ss) = BStmt ss
          blockify ss = BStmt [ss]

checkStmts :: [Stmt] -> Check [A.Stmt]
checkStmts ss = mapM checkStmt ss

inferExp :: Expr -> Check (Type, A.Expr)
inferExp = \case
    EVar id -> do
        t <- lookupVar id
        return (t, A.EVar t id)

    ELitInt i    -> return (Int, A.ELitInt Int i)

    ELitDoub d   -> return (Doub, A.ELitDoub Doub d)

    ELitTrue   -> return (Bool, A.ELitTrue Bool)

    ELitFalse  -> return (Bool, A.ELitFalse Bool)

    EApp fid es -> do
        sig <- ask
        case Map.lookup fid sig of
            Nothing -> throwError $ fmt ["undefined function:", pt fid]
            Just (FunType t ts) -> do
                unless (length ts == length es) $ throwError $ fmt ["function:", pt fid, "expected", show (length ts), "arguments, got", show (length es)]
                es' <- zipWithM checkExpr es ts
                return (t, A.EApp t fid es')

    EString s -> return (Str, A.EString Str s)

    Neg e -> do
        (t, e') <- inferExp e
        case t of
            Int  -> return (Int, A.Neg Int e')
            Doub -> return (Doub, A.Neg Doub e')
            _        -> throwError $ fmt ["Type mismatch:", pt e, ". Cannot apply negate '-' to type", pt t]

    Not e -> do
        (t, e') <- inferExp e
        unless (t == Bool) $ throwError $ fmt ["Type mismatch:", pt e, ". Cannot apply '!' to type", pt t]
        return (Bool, A.Not Bool e')

    EMul e1 op e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Int,  Int)  -> return (Int, A.EMul Int e1' op e2')
            (Doub, Doub) -> do
                unless (op /= Mod) $ throwError $ fmt ["Type mismatch:", pt e1, pt op, pt e2, ". Cannot apply '%' to", pt t1, "and", pt t2]
                return (Doub, A.EMul Doub e1' op e2')
            _                    -> throwError $ fmt ["Type mismatch:", pt e1, pt op, pt e2, ". Cannot apply", pt op, "to", pt t1, "and", pt t2]

    EAdd e1 op e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Int,  Int)  -> return (Int, A.EAdd Int e1' op e2')
            (Doub, Doub) -> return (Doub, A.EAdd Doub e1' op e2')
            _                    -> throwError $ fmt ["Type mismatch:", pt e1, pt op, pt e2, ". Cannot apply", pt op, "to", pt t1, "and", pt t2]

    ERel e1 op e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Bool, Bool) -> return (Bool, A.ERel Bool e1' op e2')
            (Int, Int)   -> return (Bool, A.ERel Int e1' op e2')
            (Doub, Doub) -> return (Bool, A.ERel Doub e1' op e2')
            _                    -> throwError $ fmt ["Type mismatch:", pt e1, pt op, pt e2, ". Cannot apply", pt op, "to", pt t1, "and", pt t2]

    EAnd e1 e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Bool, Bool) -> return (Bool, A.EAnd Bool e1' e2')
            _                    -> throwError $ fmt ["Type mismatch:", pt e1, "&&", pt e2, ". Cannot apply && to", pt t1, "and", pt t2]

    EOr e1 e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Bool, Bool) -> return (Bool, A.EOr Bool e1' e2')
            _                    -> throwError $ fmt ["Type mismatch:", pt e1, "||", pt e2, ". Cannot apply || to", pt t1, "and", pt t2]


checkExpr :: Expr -> Type -> Check A.Expr
checkExpr e t = do
    (t', e') <- inferExp e
    unless (t == t') $ throwError $ fmt ["Type mismatch:", pt e, ". Expected", pt t, ", got", pt t']
    return e'

lookupVar :: Ident -> Check Type
lookupVar x = gets stCxt >>= (\case
    [] -> throwError $ "unbound variable: " ++ pt x
    (t:_) -> return t) . mapMaybe (Map.lookup x) . toList

newVar :: Ident -> Type -> Check ()
newVar x t = case t of
    Void -> throwError $ "variable of type void not allowed: " ++ pt x
    Str -> throwError $ "variable of type string not allowed: " ++ pt x
    _ -> do
        sret <- gets stRet
        b :| bs <- gets stCxt
        let (found, b') = Map.insertLookupWithKey (\ _ t _ -> t) x t b
        unless (isNothing found) $ throwError $ "variable already defined: " ++ pt x
        put $ St (b' :| bs) sret
