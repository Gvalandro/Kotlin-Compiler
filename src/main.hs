import Lexer (alexScanTokens)
import Parser (parser)   
import AST               


main :: IO ()
main = do
  txt <- getContents
  let tokens = alexScanTokens txt
  print (tokens)
  print()
  print()
  print()
  print()
  print (parser $ alexScanTokens txt)

 
