{

module Parser where

import Lexer
import AST

}

%name parser
%tokentype { Token }
%error { parseError }

%token
id              { ID $$ }
num             { INTEGER $$ }
double          { DOUBLE_LIT $$ }
string          { STRING_LIT $$ }
'true'          { BOOLEAN_LIT True }
'false'         { BOOLEAN_LIT False }
'('             { LPAREN }
')'             { RPAREN }
'{'             { LBRACE }
'}'             { RBRACE }
':'             { COLON }
'+='            { PLUS_ASSIGN }
'-='            { MINUS_ASSIGN }
'*='            { TIMES_ASSIGN }
'/='            { DIV_ASSIGN }
'='             { ASSIGN }
'++'            { INCREMENT } 
'--'            { DECREMENT } 
'+'             { PLUS }
'-'             { MINUS }
'*'             { TIMES }
'/'             { DIVIDE }
'%'             { MOD_TOK }
'=='            { EQUAL }
'!='            { NEQ }
'<'             { LT_TOK }
'<='            { LE_TOK }
'>'             { GT_TOK }
'>='            { GE_TOK }
'&&'            { AND }
'||'            { OR }
'!'             { NOT_TOK }
'fun'           { FUN }
'main'          { MAIN }
'val'           { VAL }
'var'           { VAR }
'print'         { PRINT }  
'readln'        { READLINE } 
'if'            { IF }
'else'          { ELSE }
'while'         { WHILE }

-- Types
'Int'           { INT }
'Double'        { DOUBLE }
'Boolean'       { BOOL }


-- precedência dos operadores 
%right '=' '+=' '-=' '*=' '/='          -- operadores de atribuição (MENOR PRECEDENCIA)
%left '||'                              -- operador lógico OR
%left '&&'                              -- operador lógico AND
%nonassoc '==' '!='                     -- operadores de igualdade
%nonassoc '<' '<=' '>' '>='             -- operadores de comparação
%left '+' '-'                           -- operadores de adição e subtração
%left '*' '/' '%'                       -- operadores de multiplicação, divisão e resto
%right '!' '++' '--'                    -- operadores unários (MAIOR PRECEDENCIA)

%%

-- regra principal do programa
Program         : DeclList                                                  { Program $1 }

DeclList        : Decl DeclList                                             { $1 : $2 }    
                | {- empty -}                                               { [] }

Decl            : 'fun' 'main' '(' ')' '{' StmtList '}'                     { FunDecl $6 }

StmtList        : Stmt StmtList                                             { $1 : $2 }
                | {- empty -}                                               { [] }

-- statements
Stmt            : 'if' '(' Exp ')' Stmt 'else' Stmt 			    { IfElse $3 $5 $7 }
                | 'if' '(' Exp ')' Stmt                                     { If $3 $5 }
                | 'while' '(' Exp ')' Stmt                                  { While $3 $5 }
                | '{' StmtList '}'                                          { StmtLi $2 }
                | 'print' '(' Exp ')'                                       { Print $3 }
                | 'val' id ':' Tp '=' Exp                                   { ValDeclCT $2 $4 $6 } -- Declaração de variável imutável
                | 'val' id '=' Exp           	                            { ValDeclST $2 $4 }    -- Para `val`, imutável, tipo não é necessário
                | 'val' id ':' Tp                                           { ValDeclSI $2 $4 }    -- Declara a variável sem inicializa-a, tipo é necessário 
                | 'var' id ':' Tp '=' Exp                                   { VarDeclCT $2 $4 $6 } -- Declaração de variável mutável
                | 'var' id '=' Exp           	                            { VarDeclST $2 $4 }    -- Para `var`, mutável, tipo não é necessário
                | 'var' id ':' Tp                                           { VarDeclSI $2 $4 }    -- Declara a variável sem inicializa-a, tipo é necessário 
                | id '=' Exp                                                { Assign $1 $3 }
                | id '+=' Exp           	                            { PlusAssign $1 $3 }
                | id '-=' Exp          	                                    { MinusAssign $1 $3 }
                | id '*=' Exp          	                                    { TimesAssign $1 $3 }
                | id '/=' Exp            	                            { DivAssign $1 $3 }
                | id '++'                                                   { PostAdd $1}
                | id '--'                                                   { PostMinus $1}
                | '++' id                                                   { PreAdd $2}
                | '--' id                                                   { PreMinus $2 }
                | Exp                                                       { ExpStmt $1 }
	       
-- definição de tipo
Tp              : 'Int'                                                     { TpInt }
                | 'Boolean'                                                 { TpBool }
                | 'Double'                                                  { TpDouble }

-- expressões aritméticas e booleanas
Exp             : num                                                       { Num $1 }
                | double                                                    { DoubleLit $1 }
                | string                                                    { StringLit $1 }
                | 'true'                                                    { BoolLit True }    
                | 'false'                                                   { BoolLit False }
                | '!' Exp                                                   { Not $2 }
                | Exp '+' Exp                                               { Add $1 $3 }
                | Exp '-' Exp                                               { Sub $1 $3 }
                | Exp '*' Exp                                               { Mul $1 $3 }
                | Exp '/' Exp                                               { Div $1 $3 }
                | Exp '%' Exp                                               { Mod $1 $3 }
                | Exp '==' Exp                                              { Equal $1 $3 }
                | Exp '!=' Exp                                              { NotEqual $1 $3 }
                | Exp '<' Exp                                               { LessThan $1 $3 }
                | Exp '<=' Exp                                              { LessEqual $1 $3 }
                | Exp '>' Exp                                               { GreaterThan $1 $3 }
                | Exp '>=' Exp                                              { GreaterEqual $1 $3 }
                | Exp '&&' Exp                                              { And $1 $3 }
                | Exp '||' Exp                                              { Or $1 $3 }
                | 'readln' '(' ')'                                          { ReadLine }
                | id                                                        { Var $1 }
                | '(' Exp ')'                                               { $2 }

{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error at tokens: " ++ show tokens

}
