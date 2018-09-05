import Forca (imagensBonecoForca, imagemForca, numeroMaxErros, mostrarPalavra, tentarLetra, 
				sorteiaPalavra, jogo, inicio)
import Data.List
import System.IO
import Control.Monad

menu :: IO()
menu = do { putStrLn "Menu Principal do Jogo: ";
        putStrLn "Digite 1 para cadastrar jogador";
        putStrLn "Digite 2 para jogar";
        putStrLn "Digite 3 para visualizar o ranking";
	putStrLn "Digite 0 para sair";
        opcao <- getLine;
        case opcao of "1" -> do
        			putStrLn "\nAguarde, voceh serah direcionado para escolha de seu personagem...\n";
        			menu;

	              "2" -> do
	              	inicio;
	              	menu;

	              "3" -> do
	                putStrLn "Ranking"; 
	                menu;

	              "0" -> do
					putStrLn ("\nBye! Obrigado por jogar ;-)\n")
					return ()
	}