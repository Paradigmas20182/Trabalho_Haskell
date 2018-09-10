module Jogador where

import System.IO
import System.Directory


data Jogador = Jogador { nome :: String
                       , pontuacao :: Int
                       } deriving (Show, Read, Eq)


converte :: [String] -> [Jogador] -> [Jogador]
converte [] [] = []
converte [] ys = ys
converte [x] [] = [read x :: Jogador]
converte (x:xs) [y] = converte xs (( read x :: Jogador ) : [y])
converte (x:xs) ys = converte xs (( read x :: Jogador ) : ys)

geraString :: [Jogador] -> String -> String
geraString [] y = y
geraString [x] y = (y ++ show x ++ "\n")
geraString (x:xs) y = geraString xs (y ++ show x ++ "\n")

carregaJogadores :: IO[String]
carregaJogadores = do
  arq <- readFile "dados.txt"
  let jogadores = lines arq
  return $ jogadores

salvaJogadores :: [Jogador] -> IO ()
salvaJogadores jogadores = do
  writeFile "temp.txt" (geraString jogadores "")
  removeFile "dados.txt"
  renameFile "temp.txt" "dados.txt"
  putStrLn "Jogador salvo com sucesso!"

existeJogador :: [Jogador] -> String-> Bool
existeJogador [] _ = False
existeJogador ((Jogador n p):xs) nome
      | (n == nome) = True
      | otherwise = existeJogador xs nome
