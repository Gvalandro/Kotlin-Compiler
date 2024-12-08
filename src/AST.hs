module AST where

data Program 
    = Program [Decl]
    deriving (Eq, Show)

data Decl
    = FunDecl [Stmt]
    deriving (Eq, Show)

data Stmt
    = IfElse Exp Stmt Stmt
    | If Exp Stmt
    | While Exp Stmt
    | StmtLi [Stmt]
    | ValDeclCT String Tp Exp
    | ValDeclST String Exp
    | ValDeclSI String Tp
    | VarDeclCT String Tp Exp
    | VarDeclST String Exp
    | VarDeclSI String Tp
    | Print Exp
    | Return
    | Assign String Exp         -- não deveria ser ID exp?   -- Atribuição simples, e.g., `x = 5`
    | PlusAssign String Exp
    | MinusAssign String Exp
    | TimesAssign String Exp
    | DivAssign String Exp
    | PostAdd String
    | PostMinus String
    | PreAdd String
    | PreMinus String
    | ExpStmt Exp              -- Standalone expression as a statement, e.g., `x + 1`
    deriving (Eq, Show)

data Tp 
    = TpInt
    | TpBool
    | TpDouble
    deriving (Eq, Show)


data Exp
    = Num Int                  -- Literal inteiro, e.g., `42`
    | DoubleLit Double         -- Literal double, e.g., `3.14`
    | StringLit String         -- Literal de string, e.g., `"hello"`
    | BoolLit Bool             -- Literal booleano, e.g., `true` ou `false`   --devemos fazer um separado para true/false
    | Not Exp                  -- Operador NOT lógico, e.g., `!x`
    | Add Exp Exp              -- Operador binário, e.g., `x + y`
    | Sub Exp Exp              -- Operador binário, e.g., `x + y`
    | Mul Exp Exp              -- Operador binário, e.g., `x + y`
    | Div Exp Exp              -- Operador binário, e.g., `x + y`
    | Mod Exp Exp              -- Operador binário, e.g., `x + y`
    | Equal Exp Exp            -- Operador relacional, e.g., `x < y`
    | NotEqual Exp Exp         -- Operador relacional, e.g., `x < y`
    | LessThan Exp Exp         -- Operador relacional, e.g., `x < y`
    | LessEqual Exp Exp        -- Operador relacional, e.g., `x < y`
    | GreaterThan Exp Exp      -- Operador relacional, e.g., `x < y`
    | GreaterEqual Exp Exp     -- Operador relacional, e.g., `x < y`
    | And Exp Exp              -- Operador relacional, e.g., `x < y`
    | Or Exp Exp               -- Operador relacional, e.g., `x < y`
    | Var String               -- Chamada de variável, e.g., x
    | ReadLine 		           -- Funcao Read
    deriving (Eq, Show)

