import Forca (imagensBonecoForca, imagemForca, numeroMaxErros, mostrarPalavra, tentarLetra, 
				sorteiaPalavra, jogo)
import Data.List
import System.IO
import Control.Monad

menu :: IO()
menu = do { putStrLn "Menu Principal do Jogo: ";
        putStrLn "1 - Quero escolher meu personagem";
        putStrLn "2 - Quero comecar a jogar";
        putStrLn "3 - Quero sair do jogo";
	putStrLn "4 - Nenhuma das opcoes";
        opcao <- getLine;
        case opcao of "1" -> putStrLn "Aguarde, voceh serah direcionado para escolha de seu personagem...";
		              "2" -> putStrLn "MIGUÉ";                 	
		              "3" -> putStrLn "O jogo serah finalizado...";
		              "4" -> menu;
	}