-- module Jogador where

import System.IO

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
  writeFile "dados.txt" (geraString jogadores "")
  putStrLn "Jogadores salvos com sucesso!"

main :: IO ()
main = do
  -- let player = "Jogador {nome = \"Lucas\", pontuacao = 10}"
  -- let testa = player:[]
  -- let player2 = Jogador { nome="Cenoura" , pontuacao=20 }
  -- let teste = player2:resultado

  let player2 = Jogador { nome="Bruno" , pontuacao=20 }

  jogadores <- carregaJogadores
  let resultado = converte jogadores []

  let teste = player2:resultado

  putStrLn $ show $ head resultado
  putStrLn $ show $ last resultado
  putStrLn $ show $ head teste
  putStrLn $ show $ last teste

  salvaJogadores teste
  putStrLn "Funciona!!!"
