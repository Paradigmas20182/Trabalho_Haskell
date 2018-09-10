module Path_forca where

import Control.Applicative

palavrasObjetos :: FilePath
palavrasObjetos = "objetos.txt"

palavrasAnimais :: FilePath
palavrasAnimais = "animais.txt"

palavrasPaises :: FilePath
palavrasPaises = "paises.txt"

selecionaCategoria :: IO String
selecionaCategoria = do
    putStrLn "\n ### Menu Para selecao de categorias ### \n";
    putStrLn "Digite 1 para objetos";
    putStrLn "Digite 2 para animais";
	putStrLn "Digite 3 para paises";
	putStrLn "Opção: ";
    opcao <- getLine;
    case opcao of "1" -> do
                    readFile palavrasObjetos
                  "2" -> do
                    readFile palavrasAnimais
                  "3" -> do
                    readFile palavrasPaises

    