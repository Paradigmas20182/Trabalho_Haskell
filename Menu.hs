import Forca (imagensBonecoForca, imagemForca, numeroMaxErros, mostrarPalavra, tentarLetra, 
				sorteiaPalavra, jogo, inicio)
import Ranking
import Data.List
import System.IO
import Control.Monad

menu :: IO()
menu = do { putStrLn "\n ### Menu Principal do Jogo ### \n";
        putStrLn "Digite 1 para jogar";
        putStrLn "Digite 2 para visualizar o ranking";
	putStrLn "Digite 0 para sair";
	putStrLn "Opção: ";
        opcao <- getLine;
        case opcao of "1" -> do
        			inicio;
        			menu;

	              "2" -> do
	              	putStrLn "\n ### Ranking ### \n";
	              	mainRanking;
	              	menu;

	              "0" -> do
					putStrLn ("\nBye! Obrigado por jogar ;-)\n")
					return ()
	}