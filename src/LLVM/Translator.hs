module LLVM.Translator where
-- Translates statements / expressions from Annotated types to Abs types
-- to allow for printing via the generated printer.
import qualified Javalette.Abs as Abs
import Annotated

stmToAbs :: Stmt -> Abs.Stmt
stmToAbs Empty = Abs.Empty
stmToAbs (BStmt ss) = Abs.BStmt (map stmToAbs ss)
stmToAbs (Decl t ids) = Abs.Decl t (map itemToAbs ids)
stmToAbs (Incr t id) = Abs.Incr id
stmToAbs (Decr t id) = Abs.Decr id
stmToAbs (Ret t e) = Abs.Ret (expToAbs e)
stmToAbs VRet = Abs.VRet
stmToAbs (Cond e s) = Abs.Cond (expToAbs e) (stmToAbs s)
stmToAbs (CondElse e s1 s2) = Abs.CondElse (expToAbs e) (stmToAbs s1) (stmToAbs s2)
stmToAbs (While e s) = Abs.While (expToAbs e) (stmToAbs s)
stmToAbs (SExp t e) = Abs.SExp (expToAbs e)

itemToAbs :: Item -> Abs.Item
itemToAbs (NoInit id) = Abs.NoInit id
itemToAbs (Init id e) = Abs.Init id (expToAbs e)

expToAbs :: Expr -> Abs.Expr
expToAbs (EVar _ id) = Abs.EVar id
expToAbs (ELitInt _ i) = Abs.ELitInt i
expToAbs (ELitDoub _ d) = Abs.ELitDoub d
expToAbs (ELitTrue _) = Abs.ELitTrue
expToAbs (ELitFalse _) = Abs.ELitFalse
expToAbs (EApp _ id es) = Abs.EApp id (map expToAbs es)
expToAbs (EString _ s) = Abs.EString s
expToAbs (Neg _ e) = Abs.Neg (expToAbs e)
expToAbs (Not _ e) = Abs.Not (expToAbs e)
expToAbs (EMul t e1 op e2) = Abs.EMul (expToAbs e1) op (expToAbs e2)
expToAbs (EAdd t e1 op e2) = Abs.EAdd (expToAbs e1) op (expToAbs e2)
expToAbs (ERel t e1 op e2) = Abs.ERel (expToAbs e1) op (expToAbs e2)
expToAbs (EAnd _ e1 e2) = Abs.EAnd (expToAbs e1) (expToAbs e2)
expToAbs (EOr _ e1 e2) = Abs.EOr (expToAbs e1) (expToAbs e2)
