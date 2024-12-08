import System.IO (getContents)
import Control.Monad.Trans.State.Lazy (evalState)
import Data.Functor.Identity (runIdentity)
import Lexer (alexScanTokens)
import Parser (parser)
import IntermediateCode (transProgram, Supply, initialSupply) -- Adicione o Supply inicial

main :: IO ()
main = do
  -- Lê o código fonte da entrada padrão
  txt <- getContents
  
  -- Exibe o código fonte
  putStrLn "Código fonte:\n"
  putStrLn txt
  putStrLn "\n"
  
  -- Realiza a análise léxica e sintática
  putStrLn "Realizando análise sintática..."
  let parseResult = parser $ alexScanTokens txt
  
  -- Mostra o AST gerado
  putStrLn "\nAST gerado:"
  print parseResult
  putStrLn "\n"
  
  -- Gera o código intermediário executando o estado inicial
  putStrLn "Gerando código intermediário..."
  let intermediateCode = evalState (transProgram parseResult) initialSupply
  
  -- Mostra o código intermediário
  putStrLn "\nCódigo intermediário:"
  mapM_ print intermediateCode
  
