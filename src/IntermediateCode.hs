module IntermediateCode where

import AST
import IR
import Control.Monad.State (State)
import qualified Control.Monad.State as State

type Supply = Int

initialSupply :: Supply
initialSupply = 0

-- Geradores de temporários e etiquetas
newTemp :: State Supply Temp
newTemp = do
  s <- State.get
  State.put (s + 1)
  return $ "t" ++ show s

newLabel :: State Supply Label
newLabel = do
  s <- State.get
  State.put (s + 1)
  return $ "L" ++ show s

-- Transformação de expressões
transExpr :: Exp -> Temp -> State Supply [Instr]
transExpr (Num n) dest = return [MOVEI dest n]
transExpr (DoubleLit d) dest = return [MOVEF dest d]
transExpr (StringLit s) dest = return [MOVES dest s]  -- Tratando literais de string
transExpr (BoolLit True) dest = return [MOVEI dest 1]
transExpr (BoolLit False) dest = return [MOVEI dest 0]
transExpr (Not e) dest = do
  t <- newTemp
  code <- transExpr e t
  return $ code ++ [NOT dest t]
transExpr (Add e1 e2) dest = transBinaryOp ADD e1 e2 dest
transExpr (Sub e1 e2) dest = transBinaryOp SUB e1 e2 dest
transExpr (Mul e1 e2) dest = transBinaryOp MUL e1 e2 dest
transExpr (Div e1 e2) dest = transBinaryOp DIV e1 e2 dest
transExpr (Mod e1 e2) dest = transBinaryOp MOD e1 e2 dest
transExpr (Var x) dest = return [MOVE dest x]
transExpr ReadLine dest = return [READ dest]
transExpr (And e1 e2) dest = do
  lTrue <- newLabel
  lEnd <- newLabel
  t1 <- newTemp
  t2 <- newTemp
  code1 <- transExpr e1 t1
  code2 <- transExpr e2 t2
  tTrue <- newTemp  -- Criação de temporário para "1"
  return $ code1 ++ [MOVEI tTrue 1] ++ [COND t1 EQUAL tTrue lTrue lEnd] ++ 
         [LABEL lTrue] ++ code2 ++ [COND t2 EQUAL tTrue dest lEnd, LABEL lEnd]
transExpr (Equal e1 e2) dest = transRelOp EQUAL e1 e2 dest
transExpr (NotEqual e1 e2) dest = transRelOp NOTEQUAL e1 e2 dest
transExpr (LessThan e1 e2) dest = transRelOp LESSTHAN e1 e2 dest
transExpr (LessEqual e1 e2) dest = transRelOp LESSEQUAL e1 e2 dest
transExpr (GreaterThan e1 e2) dest = transRelOp GREATERTHAN e1 e2 dest
transExpr (GreaterEqual e1 e2) dest = transRelOp GREATEREQUAL e1 e2 dest
transExpr (Or e1 e2) dest = do
  lFalse <- newLabel
  lEnd <- newLabel
  t1 <- newTemp
  t2 <- newTemp
  code1 <- transExpr e1 t1
  code2 <- transExpr e2 t2
  tFalse <- newTemp  -- Criação de temporário para "0"
  return $ code1 ++ [MOVEI tFalse 0] ++ [COND t1 EQUAL tFalse lFalse lEnd] ++ 
         [LABEL lFalse] ++ code2 ++ [COND t2 EQUAL tFalse dest lEnd, LABEL lEnd]

transRelOp :: RelOp -> Exp -> Exp -> Temp -> State Supply [Instr]
transRelOp op e1 e2 dest = do
  t1 <- newTemp
  t2 <- newTemp
  code1 <- transExpr e1 t1
  code2 <- transExpr e2 t2
  return $ code1 ++ code2 ++ [COND t1 op t2 dest "0"]

-- Função auxiliar para operações binárias
transBinaryOp :: BinOp -> Exp -> Exp -> Temp -> State Supply [Instr]
transBinaryOp op e1 e2 dest = do
  t1 <- newTemp
  t2 <- newTemp
  code1 <- transExpr e1 t1
  code2 <- transExpr e2 t2
  return $ code1 ++ code2 ++ [OP op dest t1 t2]

-- Transformação de Statements
transStmt :: Stmt -> State Supply [Instr]
transStmt (Assign var expr) = do
  t <- newTemp
  code <- transExpr expr t
  return $ code ++ [MOVE var t]
transStmt (ValDeclCT var _ expr) = do
  t <- newTemp
  code <- transExpr expr t
  return $ code ++ [MOVE var t]

transStmt (VarDeclCT var _ expr) = do
  t <- newTemp
  code <- transExpr expr t
  return $ code ++ [MOVE var t]
transStmt (StmtLi stmts) = concat <$> mapM transStmt stmts
transStmt (PlusAssign var expr) = transCompoundAssign ADD var expr
transStmt (MinusAssign var expr) = transCompoundAssign SUB var expr
transStmt (TimesAssign var expr) = transCompoundAssign MUL var expr
transStmt (DivAssign var expr) = transCompoundAssign DIV var expr
transStmt (ValDeclSI var _) = return [MOVEI var 0]
transStmt (VarDeclSI var _) = return [MOVEI var 0]
transStmt (PreAdd var) = return [OP ADD var var "1"]
transStmt (PostAdd var) = transPostIncrement var ADD
transStmt (PreMinus var) = return [OP SUB var var "1"]
transStmt (PostMinus var) = transPostIncrement var SUB
transStmt (Print expr) = do
  t <- newTemp
  code <- transExpr expr t
  return $ code ++ [PRINT t]
transStmt (If cond thenStmt) = do
  lTrue <- newLabel
  lEnd <- newLabel
  condCode <- transCond cond lTrue lEnd
  thenCode <- transStmt thenStmt
  return $ condCode ++ [LABEL lTrue] ++ thenCode ++ [LABEL lEnd]
transStmt (IfElse cond thenStmt elseStmt) = do
  lTrue <- newLabel
  lFalse <- newLabel
  lEnd <- newLabel
  condCode <- transCond cond lTrue lFalse
  thenCode <- transStmt thenStmt
  elseCode <- transStmt elseStmt
  return $ condCode ++ [LABEL lTrue] ++ thenCode ++ [JUMP lEnd, LABEL lFalse] ++ elseCode ++ [LABEL lEnd]
transStmt (While cond body) = do
  lStart <- newLabel
  lBody <- newLabel
  lEnd <- newLabel
  condCode <- transCond cond lBody lEnd
  bodyCode <- transStmt body
  return $ [LABEL lStart] ++ condCode ++ [LABEL lBody] ++ bodyCode ++ [JUMP lStart, LABEL lEnd]
transStmt (ValDeclST var expr) = do
  t <- newTemp
  code <- transExpr expr t
  return $ code ++ [MOVE var t]

transStmt (VarDeclST var expr) = do
  t <- newTemp
  code <- transExpr expr t
  return $ code ++ [MOVE var t]

transStmt (ExpStmt expr) = do
  t <- newTemp
  transExpr expr t

-- Função auxiliar para atribuições compostas
transCompoundAssign :: BinOp -> String -> Exp -> State Supply [Instr]
transCompoundAssign op var expr = do
  t <- newTemp
  code <- transExpr expr t
  return $ code ++ [OP op var var t]

-- Função auxiliar para incremento/decremento pós-fixado
transPostIncrement :: String -> BinOp -> State Supply [Instr]
transPostIncrement var op = do
  t <- newTemp
  return [MOVE t var, OP op var var "1"]

-- Funções auxiliares para condicionais
transCond :: Exp -> Label -> Label -> State Supply [Instr]
transCond cond lTrue lFalse = do
  t <- newTemp
  code <- transExpr cond t
  return $ code ++ [COND t EQUAL "1" lTrue lFalse]

-- Transformação do programa completo
transProgram :: Program -> State Supply [Instr]
transProgram (Program decls) = concat <$> mapM transDecl decls

transDecl :: Decl -> State Supply [Instr]
transDecl (FunDecl stmts) = concat <$> mapM transStmt stmts
