{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeChecker where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable (toList)
import Data.Functor
import Data.List.NonEmpty (pattern (:|), (<|))
import qualified Data.List.NonEmpty as List1
import Data.Maybe
import Data.List (groupBy, sort)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Javalette.Abs
import qualified Javalette.Abs as Abs
import Javalette.Print (printTree)

import FunType
import qualified Annotated as A
import Control.Monad.Trans.Except (throwE)

type AnnotatedProgram = A.Prog
type TypeError = String

type List1 = List1.NonEmpty

type Sig = Map Ident FunType

data St = St
    { stCxt :: Cxt
    , stRet :: Type
    }

type Cxt   = List1 Block
type Block = Map Ident Type

initSt :: Type -> St
initSt = St $ Map.empty :| []

type Check = ReaderT Sig (StateT St (Except TypeError))

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
            unless (null strRetFuncs) $ throwError $ "function may not return type string: " ++ printTree (head strRetFuncs)
          assertNoDuplicateFuns s = do
            let duplicates = filter (\x -> length x > 1) $ groupBy (\t1 t2 -> fst t1 == fst t2) $ sort s
            unless (null duplicates) $ throwError $ "duplicate function definition: " ++ printTree (fst (head $ head duplicates))

checkProgram :: Prog -> Check A.Prog
checkProgram (Program defs) = do
    mapM_ checkReturn defs
    defs' <- mapM checkDef defs
    return (A.Program defs')

checkDef :: TopDef -> Check A.TopDef
checkDef (FnDef t fId args ss) = do
    put $ initSt t
    mapM_ (\(Argument t x) -> newVar x t) args
    ss' <- checkStms ss
    return $ A.FnDef t fId args (A.Block ss')

checkReturn :: TopDef -> Check ()
checkReturn (FnDef Void fId _ ss) = return ()
checkReturn (FnDef t fId _ (Block ss)) = if any checkPath ss then
        return ()
    else
        throwError $ "Not every execution path returns in function: " ++ printTree fId

checkPath :: Stmt -> Bool
checkPath = \case
    Ret _ -> True
    CondElse _ s1 s2 -> checkPath s1 && checkPath s2
    (BStmt (Block ss)) -> any checkPath ss
    _ -> False

checkInit :: Type -> [Item] -> Check [A.Item]
checkInit t = go
    where go :: [Item] -> Check [A.Item]
          go [] = return []
          go (i:is) = do
              i' <- checkItem i
              is' <- go is
              return $ i' : is'
          checkItem :: Item -> Check A.Item
          checkItem = \case
              NoInit id -> do
                  newVar id t
                  return $ A.NoInit id
              Init id e -> do
                  newVar id t
                  e' <- checkExp e t
                  return $ A.Init id e'

checkStm :: Stmt -> Check A.Stmt
checkStm = \case
    Empty -> return A.Empty

    SExp e -> do
        case e of
            EApp _ _ -> do
                (t, e') <- inferExp e
                return $ A.SExp t e'
            _ -> throwError $ "Void expressions are not allowed: " ++ printTree e

    Ass id e -> do
        (t, e') <- inferExp e
        t' <- lookupVar id
        if t == t' then
                return $ A.Ass t id e'
            else
                throwError $ "Type mismatch: " ++ printTree e ++ ". Expected " ++ printTree t' ++ ", got " ++ printTree t

    Incr id -> do
        t <- lookupVar id
        unless (t == Int) $ throwError $ "Type mismatch. Cannot apply '++' to " ++ printTree t
        return $ A.Incr t id

    Decr id -> do
        t <- lookupVar id
        unless (t == Int) $ throwError $ "Type mismatch. Cannot apply '--' to " ++ printTree t
        return $ A.Decr t id

    Decl t items -> do
        items' <- checkInit t items
        return $ A.Decl t items'
        
    Ret e -> do
        (t, e') <- inferExp e
        t' <- gets stRet
        if t == t' then
                return $ A.Ret t e'
            else
                throwError $ "Type mismatch: " ++ printTree e ++ ". Expected " ++ printTree t' ++ ", but function returns " ++ printTree t

    VRet -> do
        t <- gets stRet
        unless (t == Void) $ throwError "Type mismatch: void return in non-void function"
        return A.VRet

    Cond e s -> do
        (t, e') <- inferExp e
        unless (t == Bool) $ throwError $ "Type mismatch: " ++ printTree e ++ ". Expected boolean, got " ++ printTree t
        s' <- checkStm $ blockify s
        return $ A.Cond e' s'

    CondElse e s1 s2 -> do
        (t, e') <- inferExp e
        unless (t == Bool) $ throwError $ "Type mismatch: " ++ printTree e ++ ". Expected boolean, got " ++ printTree t
        s1' <- checkStm $ blockify s1
        s2' <- checkStm $ blockify s2
        return $ A.CondElse e' s1' s2'

    While e s -> do
        (t, e') <- inferExp e
        unless (t == Bool) $ throwError $ "Type mismatch: " ++ printTree e ++ ". Expected boolean, got " ++ printTree t
        s' <- checkStm $ blockify s
        return $ A.While e' s'

    BStmt ss -> do
        sret <- gets stRet
        bs <- gets stCxt
        put $ St (Map.empty :| toList bs) sret
        ss' <- checkStms ss
        put $ St bs sret
        return $ A.BStmt (A.Block ss')

    where blockify (BStmt ss) = BStmt ss
          blockify ss = BStmt (Block [ss])

checkStms :: Blk -> Check [A.Stmt]
checkStms (Block ss) = mapM checkStm ss

inferExp :: Expr -> Check (Type, A.Expr)
inferExp = \case
    EVar id -> do
        t <- lookupVar id
        return (t, A.EVar id)

    ELitInt i    -> return (Int, A.ELitInt i)

    ELitDoub d   -> return (Doub, A.ELitDoub d)

    ELitTrue   -> return (Bool, A.ELitTrue)

    ELitFalse  -> return (Bool, A.ELitFalse)

    EApp fid es -> do
        sig <- ask
        case Map.lookup fid sig of
            Nothing -> throwError $ "undefined function: " ++ printTree fid
            Just (FunType t ts) -> do
                unless (length ts == length es) $ throwError $ "function: " ++ printTree fid ++ " expected " ++ show (length ts) ++ " arguments, got " ++ show (length es)
                es' <- zipWithM checkExp es ts
                return (t, A.EApp fid es')

    -- Set type to void, as the BNF does not specify a type for the string literal
    EString s -> return (Str, A.EString s)

    Neg e -> do
        (t, e') <- inferExp e
        case t of
            Int  -> return (Int, A.Neg e')
            Doub -> return (Doub, A.Neg e')
            _        -> throwError $ "Type mismatch: " ++ printTree e ++ ". Cannot negate (-) to " ++ printTree t

    Not e -> do
        (t, e') <- inferExp e
        unless (t == Bool) $ throwError $ "Type mismatch: " ++ printTree e ++ ". Cannot apply '!' to " ++ printTree t
        return (Bool, A.Not e')

    EMul e1 op e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Int,  Int)  -> return (Int, A.EMul Int e1' op e2')
            (Doub, Doub) -> do
                unless (op /= Mod) $ throwError $ "Type mismatch: " ++ printTree e1 ++ " " ++ printTree op ++ " " ++ printTree e2 ++ ". Cannot apply '%' to " ++ printTree t1 ++ " and " ++ printTree t2
                return (Doub, A.EMul Doub e1' op e2')
            _                    -> throwError $ "Type mismatch: " ++ printTree e1 ++ " " ++ printTree op ++ " " ++ printTree e2 ++ ". Cannot apply '" ++ printTree op ++ "' to " ++ printTree t1 ++ " and " ++ printTree t2

    EAdd e1 op e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Int,  Int)  -> return (Int, A.EAdd Int e1' op e2')
            (Doub, Doub) -> return (Doub, A.EAdd Doub e1' op e2')
            _                    -> throwError $ "Type mismatch: " ++ printTree e1 ++ " " ++ printTree op ++ " " ++ printTree e2 ++ ". Cannot apply '" ++ printTree op ++ "' to " ++ printTree t1 ++ " and " ++ printTree t2

    ERel e1 op e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Bool, Bool) -> return (Bool, A.ERel Bool e1' op e2')
            (Int, Int)   -> return (Bool, A.ERel Int e1' op e2')
            (Doub, Doub) -> return (Bool, A.ERel Doub e1' op e2')
            _                    -> throwError $ "Type mismatch: " ++ printTree e1 ++ " " ++ printTree op ++ " " ++ printTree e2 ++ ". Cannot apply '" ++ printTree op ++ "' to " ++ printTree t1 ++ " and " ++ printTree t2

    EAnd e1 e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Bool, Bool) -> return (Bool, A.EAnd e1' e2')
            _                    -> throwError $ "Type mismatch: " ++ printTree e1 ++ " " ++ printTree e2 ++ ". Cannot apply '&&' to " ++ printTree t1 ++ " and " ++ printTree t2

    EOr e1 e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        case (t1, t2) of
            (Bool, Bool) -> return (Bool, A.EOr e1' e2')
            _                    -> throwError $ "Type mismatch: " ++ printTree e1 ++ " " ++ printTree e2 ++ ". Cannot apply '||' to " ++ printTree t1 ++ " and " ++ printTree t2


checkExp :: Expr -> Type -> Check A.Expr
checkExp e t = do
    (t', e') <- inferExp e
    unless (t == t') $ throwError $ "Type mismatch: " ++ printTree e ++ ". Expected " ++ printTree t ++ ", got " ++ printTree t'
    return e'

lookupVar :: Ident -> Check Type
lookupVar x = gets stCxt >>= (\case
    [] -> throwError $ "unbound variable: " ++ printTree x
    (t:_) -> return t) . mapMaybe (Map.lookup x) . toList

newVar :: Ident -> Type -> Check ()
newVar x t = case t of
    Void -> throwError $ "variable void type not allowed: " ++ printTree x
    Str -> throwError $ "variable string type not allowed: " ++ printTree x
    _ -> do
        sret <- gets stRet
        b :| bs <- gets stCxt
        let (found, b') = Map.insertLookupWithKey (\ _ t _ -> t) x t b
        unless (isNothing found) $ throwError $ "variable already defined: " ++ printTree x
        put $ St (b' :| bs) sret
