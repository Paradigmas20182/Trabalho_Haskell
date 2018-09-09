import Jogador
import Data.List
import System.IO
import Data.Char
import Data.List (transpose)
import System.Random (randomIO)
import Control.Applicative
import Path_forca
import Control.Exception
import System.IO.Error
import System.Process
import Data.Function
import Data.String

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
