module Ranking where

import Jogador
import Data.List
import Data.Function

exibirRanking :: [Jogador] -> IO ()
exibirRanking [] = return ()
exibirRanking (x:xs) = do
			putStrLn ((obterNome x) ++ " possui " ++ (show (obterPontuacao x)) ++ " pontos")
			exibirRanking xs

obterNome :: Jogador -> String
obterNome (Jogador nome _) = nome

obterPontuacao :: Jogador -> Int
obterPontuacao (Jogador _ pontuacao) = pontuacao

ordenar :: [Jogador] -> [Jogador]
ordenar dados = sortBy (compare `on` obterPontuacao) dados

mainRanking :: IO ()
mainRanking = do

  jogadores <- carregaJogadores
  let resultado = converte jogadores []
  exibirRanking (reverse (ordenar resultado))
