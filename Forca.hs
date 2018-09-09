module Forca (imagensBonecoForca, imagemForca, numeroMaxErros, mostrarPalavra, tentarLetra,
				sorteiaPalavra, jogo, inicio) where

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
import Jogador

-- type Jogadores = [String]
type LetrasTentadas = [Char]
type Nome = String
-- type Pontuacao = Int
-- data Jogador = Jogador Nome Pontuacao
-- 					deriving (Show, Read)

--Matriz com imagens dos bonecos da forca em uma matriz transposta
imagensBonecoForca :: [[String]]
imagensBonecoForca =
	transpose
	[["   ", " O ", " O ", " O ", " O " , " O " , " O " ]
	,["   ", "   ", " | ", " | ", " | " , "/| " , " |\\" ]
	,["   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\"]]

--Função para renderizar boneco na forca, de acordo com indice, que receberá numero de erros
imagemForca :: Int -> [String]
imagemForca indice =
	"-----------" :
	"|    |" :
	map ("|   " ++) imagem
	where imagem = imagensBonecoForca !! indice

--Declaração do número máximo de erros
numeroMaxErros :: Int
numeroMaxErros = length imagensBonecoForca - 1

mostrarPalavra :: String -> String
mostrarPalavra palavra = intersperse ' ' [if a `elem` ['a'..'z'] then '_' else a | a <- palavra]

tentarLetra :: String -> Char -> Int -> LetrasTentadas -> IO ()
tentarLetra palavra letra tentativas letrasTentadas
	| letra `elem` palavra 	= do
		let letras = toUpper letra : letrasTentadas
		jogo [if letra == a then toUpper letra else a | a <- palavra] tentativas letras
	| toUpper letra `elem` letrasTentadas = do
		putStrLn "Voce já tentou essa letra! "
		putStrLn $ letrasTentadas
		jogo palavra tentativas letrasTentadas
	| otherwise = do
		let letras = toUpper letra : letrasTentadas
		jogo palavra (tentativas - 1) letras

-- Esta funcao faz um sorteio de uma palvra dentro de um arquivo .txt de forma aleatoria
sorteiaPalavra :: IO[Char]
sorteiaPalavra = do
	discionario <- readFile discionarioPalavras
	let palavras = filter palavraValida $ lines discionario
	let numeroPalavras = length palavras
	numeroAleatorio <- randomIO
	let palavraAleatoria = palavras !! (numeroAleatorio `mod` numeroPalavras)
	return $ palavraAleatoria
	where
		palavraValida palavra =
			'\'' `notElem` palavra &&
			map toLower palavra == palavra

-- Funcao que realiza o jogo e o loop do jogo, verificando se ainda restam numero de tentativas e apresentando imagem da forca
jogo :: String -> Int -> LetrasTentadas -> IO ()
jogo palavra tentativas letrasTentadas
	| palavra == map toUpper palavra = do
		putStrLn $ mostrarPalavra palavra
		putStrLn "Voce Ganhou!"
	| tentativas == 0 = do
		putStrLn $ mostrarPalavra palavra
		putStrLn "Voce Perdeu..."
	| otherwise = do
		putStrLn $ unlines $ imagemForca(numeroMaxErros - tentativas)
		putStrLn $ "Voce tem " ++ show tentativas ++ " tentativas restantes."
		putStrLn $ mostrarPalavra palavra
		putStr "Digite uma letra: "
		tentativaDeLetra <- getLine

		tentarLetra palavra (head tentativaDeLetra) tentativas letrasTentadas

-- função que recebe uma String e retorna uma IO String
getString :: String -> IO String
getString str = do
			putStr str
			res <- getLine
			return res

-- Inicia o jogo
inicio :: IO()
inicio = do
	hSetBuffering stdout NoBuffering
	players <- carregaJogadores
	putStrLn "Bem vindo ao Jogo da Forca"
	palavra <- sorteiaPalavra
	jogador1 <- getString"\nDigite o nome do jogador: "
	putStrLn jogador1
	-- if jogador1 `elem` players then do
	--  	putStrLn "\nJogador encontrado"
	-- else do
	-- 	putStrLn "\nNovo jogador! Seja bem vindo!!"
	-- 	appendFile "dados.txt" (jogador1 ++ "\n")

	mainJogador jogador1

	let letras = []
	jogo (map toLower palavra) numeroMaxErros letras
	putStrLn "Obrigado por jogar! :)"
