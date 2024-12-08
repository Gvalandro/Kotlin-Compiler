module IR where

type Temp = String
type Label = String

data Instr
  = MOVE Temp Temp           -- move valor de um temporário para outro
  | MOVEI Temp Int           -- move um valor imediato para um temporário
  | MOVEF Temp Double     -- Para valores de ponto flutuante
  | MOVES Temp String         -- move uma string literal para um temporário
  | OP BinOp Temp Temp Temp  -- operação binária
  | NOT Temp Temp
  | COND Temp RelOp Temp Label Label -- condição
  | LABEL Label              -- etiqueta de destino
  | JUMP Label               -- salto incondicional
  | CALL Temp String [Temp]  -- chamada de função
  | RETURN                   -- retorno vazio
  | PRINT Temp               -- imprime o valor de um temporário
  | READ Temp                -- lê um valor e armazena no temporário
  deriving (Show, Eq)

-- Operadores binários
data BinOp = ADD | SUB | MUL | DIV | MOD 
  deriving (Show, Eq)

-- Operadores relacionais
data RelOp = 
    LESSTHAN   -- Menor que
  | LESSEQUAL -- Menor ou igual
  | EQUAL   -- Igual
  | NOTEQUAL  -- Diferente
  | GREATERTHAN   -- Maior que
  | GREATEREQUAL -- Maior ou igual
  | AND
  | OR
  deriving (Show, Eq)
