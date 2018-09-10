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

import System.Exit
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

tentarLetra :: String -> Char -> Int -> LetrasTentadas -> [Jogador] -> String -> IO ()
tentarLetra palavra letra tentativas letrasTentadas lista_jogadores jogador1
	| letra `elem` palavra 	= do
		let letras = toUpper letra : letrasTentadas
		jogo [if letra == a then toUpper letra else a | a <- palavra] tentativas letras lista_jogadores jogador1
	| toUpper letra `elem` letrasTentadas = do
		putStrLn "Voce já tentou essa letra! "
		putStrLn $ letrasTentadas
		jogo palavra tentativas letrasTentadas lista_jogadores jogador1
	| otherwise = do
		let letras = toUpper letra : letrasTentadas
		jogo palavra (tentativas - 1) letras lista_jogadores jogador1

-- Esta funcao faz um sorteio de uma palvra dentro de um arquivo .txt de forma aleatoria
sorteiaPalavra :: IO[Char]
sorteiaPalavra = do
	dicionario <- selecionaCategoria
	let palavras = filter palavraValida $ lines dicionario
	let numeroPalavras = length palavras
	numeroAleatorio <- randomIO
	let palavraAleatoria = palavras !! (numeroAleatorio `mod` numeroPalavras)
	return $ palavraAleatoria
	where
		palavraValida palavra =
			'\'' `notElem` palavra &&
			map toLower palavra == palavra

-- Funcao que realiza o jogo e o loop do jogo, verificando se ainda restam numero de tentativas e apresentando imagem da forca
jogo :: String -> Int -> LetrasTentadas -> [Jogador] -> String -> IO ()
jogo palavra tentativas letrasTentadas lista_jogadores jogador1
	| palavra == map toUpper palavra = do
		putStrLn $ mostrarPalavra palavra
		putStrLn "Voce Ganhou!"
		let nova_pontuacao = incrementaPontuacao lista_jogadores jogador1
		salvaJogadores nova_pontuacao
	| tentativas == 0 = do
		putStrLn $ mostrarPalavra palavra
		putStrLn "Voce Perdeu..."
	| otherwise = do
		putStrLn $ unlines $ imagemForca(numeroMaxErros - tentativas)
		putStrLn $ "Voce tem " ++ show tentativas ++ " tentativas restantes."
		putStrLn $ mostrarPalavra palavra
		putStr "Digite uma letra: "
		tentativaDeLetra <- getLine

		tentarLetra palavra (head tentativaDeLetra) tentativas letrasTentadas lista_jogadores jogador1

incrementaPontuacao :: [Jogador] -> String -> [Jogador]
incrementaPontuacao ((Jogador nome pontuacao):xs) vencedor
				| (nome == vencedor) = [(Jogador nome (pontuacao + 5))] ++ xs
				| otherwise = (Jogador nome pontuacao):(incrementaPontuacao xs vencedor)

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
	putStrLn "\n -- Bem vindo ao Jogo da Forca --"
	palavra <- sorteiaPalavra
	jogador1 <- getString"\nDigite o nome do jogador: "

	let novo_jogador = Jogador { nome = jogador1 , pontuacao = 0 }

	jogadores <- carregaJogadores
	let lista_jogadores = converte jogadores []

	if (existeJogador lista_jogadores jogador1) then do
		putStr"Bem-Vindo novamente "
		putStrLn jogador1

		let letras = []
		jogo (map toLower palavra) numeroMaxErros letras lista_jogadores jogador1
		putStrLn "Obrigado por jogar! :)"

	else do
		let nova_lista = novo_jogador:lista_jogadores
		salvaJogadores nova_lista
		let letras = []
		jogo (map toLower palavra) numeroMaxErros letras nova_lista jogador1
		putStrLn "Obrigado por jogar! :)"
