import System.Random
import Control.Monad
import Data.List (nub)
import System.IO()

type Battlefield = [[Int]]
type Ship = [(Int, Int)]
type Position = (Int, Int)

data Difficulty = Easy | Medium | Hard deriving (Show, Eq)

main :: IO ()
main = do
    showMainMenu
    _ <- getChar
    municao <- dificuldade
    let battlefield = inicializaCampoDeBatalha
    corvetas <- posicionaCorvetas 2 []
    fragatas <- posicionaFragatas 2 corvetas []
    let hits = []
    let sunken = []
    gameLoop battlefield corvetas fragatas hits sunken municao 0 0

showMainMenu :: IO ()
showMainMenu = do
    let logo = [
                "    ____       _______ _______ _      ______  _____ _    ___     _____",
                "    |  _ \\   /\\|__   __|__   __| |    |  ____|/ ____| |  | \\ \\   |  __ \\",
                "     | |_) | /  \\  | |     | |  | |    | |__  | (___ | |__| |\\ \\  | |__) |",
                "    |  _ < / /\\ \\ | |     | |  | |    |  __|  \\___ \\|  __  | > \\ |  ___/",
                "| |_) / ____ \\| |     | |  | |____| |____ ____) | |  | |/ ^ \\| |",
                "|____/_/    \\_\\_|     |_|  |______|______|_____/|_|  |_/_/ \\_\\_|",


                "                                                                       "]

    let prompt = "Argh, marujo! Seja bem-vindo! Pressione ENTER para iniciar a batalha, argh!"
    mapM_ putStrLn (center logo)
    putStrLn ""
    putStrLn (centerLine prompt)
    putStrLn ""
    putStrLn ""

center :: [String] -> [String]
center xs = map centerLine xs

centerLine :: String -> String
centerLine s = replicate padding ' ' ++ s
  where
    screenWidth = 80
    padding = (screenWidth - length s) `div` 2

dificuldade :: IO Int
dificuldade = do
    putStrLn "Bem-vindo, marujo! \\o/\nVocê foi selecionado para defender o país de uma invasão pirata!"
    putStrLn "Duas CORVETAS e duas FRAGATAS compõem a frota inimiga.\n"
    putStrLn "Uma CORVETA ocupa um espaço na tabela, para afundá-la basta acertar um tiro."
    putStrLn "Uma FRAGATA ocupa dois espaços, para afundá-las você deve acertar dois tiros em duas ou mais tentativas.\n"
    putStrLn "Escolha a dificuldade na qual deseja jogar:\n"
    putStrLn "- Para 'Fácil' digite 1 e tecle ENTER:\n\tNesta dificuldade você terá um total de 28 tiros\n"
    putStrLn "- Para 'Médio' digite 2 e tecle ENTER:\n\tNesta dificuldade você terá um total de 20 tiros\n"
    putStrLn "- Para 'Díficil' digite 3 e tecle ENTER:\n\tNesta dificuldade você terá um total de 12 tiros\n"
    diff <- readLn
    putStrLn "\n"
    case diff of
        1 -> return 28
        2 -> return 20
        3 -> return 12
        _ -> do
            putStrLn "Por favor, digite apenas numeros de 1 a 3!"
            dificuldade

inicializaCampoDeBatalha :: Battlefield
inicializaCampoDeBatalha = replicate 6 (replicate 6 0)

visualizaCampoDeBatalha :: Battlefield -> IO ()
visualizaCampoDeBatalha battlefield = do
    putStrLn "\t1 \t2 \t3 \t4 \t5 \t6"
    forM_ (zip [1 :: Int ..] battlefield) $ \(y, row) -> do
        putStr (show y)
        forM_ row $ \cell -> putStr (cellChar cell)
        putStrLn "\n"
  where
    cellChar 0 = "\t~"
    cellChar (-1) = "\t*"
    cellChar 1 = "\tX"
    cellChar _ = "\t?"

tiroCerto :: Position -> Ship -> [(Int, Int, Int, Int)] -> Bool
tiroCerto disparo corvetas fragatas = disparo `elem` corvetas || any (posicaoNaFragata disparo) fragatas
  where
    posicaoNaFragata (x, y) (fx1, fy1, fx2, fy2) = (x == fx1 && y == fy1) || (x == fx2 && y == fy2)

atirar :: IO Position
atirar = do
    putStrLn "Escolha a linha e a coluna para dar o tiro!"
    putStrLn "Linha: "
    linha <- readLn
    putStrLn "Coluna: "
    coluna <- readLn
    return (linha - 1, coluna - 1)

alteraCampoDeBatalha :: Position -> Battlefield -> Int -> Battlefield
alteraCampoDeBatalha (x, y) battlefield marker
    | x < 0 || x >= length battlefield || y < 0 || y >= length (head battlefield) = battlefield
    | otherwise =
        let (before, currentRow:after) = splitAt x battlefield
            newRow = alterRow y currentRow marker
        in before ++ [newRow] ++ after

alterRow :: Int -> [Int] -> Int -> [Int]
alterRow y row marker
    | y < 0 || y >= length row = row
    | otherwise =
        let (rowBefore, _:rowAfter) = splitAt y row
        in rowBefore ++ [marker] ++ rowAfter

posicionaCorvetas :: Int -> Ship -> IO Ship
posicionaCorvetas 0 corvetas = return corvetas
posicionaCorvetas n corvetas = do
    g <- newStdGen
    let positions = take n $ nub $ randomRs ((0, 0), (5, 5)) g
    let newCorvetas = positions ++ corvetas
    if length (nub newCorvetas) == length newCorvetas
        then posicionaCorvetas (n - 1) newCorvetas
        else posicionaCorvetas n corvetas

posicionaFragatas :: Int -> Ship -> [(Int, Int, Int, Int)] -> IO [(Int, Int, Int, Int)]
posicionaFragatas 0 _ fragatas = return fragatas
posicionaFragatas n corvetas fragatas = do
    g <- newStdGen
    let positions = take (n * 2) $ nub $ randomRs ((0, 0), (5, 5)) g
    let newFragata = ((positions !! 0), (positions !! 1))
    let flattenedFragata = (\((x1, y1), (x2, y2)) -> [(x1, y1), (x2, y2)]) newFragata
    if all (`notElem` corvetas) flattenedFragata && length flattenedFragata == length (nub flattenedFragata)
        then posicionaFragatas (n - 1) corvetas ((toQuadruple newFragata) : fragatas)
        else posicionaFragatas n corvetas fragatas
  where
    toQuadruple ((x1, y1), (x2, y2)) = (x1, y1, x2, y2)

dicaErro :: Position -> Int -> Ship -> [(Int, Int, Int, Int)] -> Ship -> IO ()
dicaErro (x, y) iteracoes corvetas fragatas sunken = do
    let remainingCorvetas = filter (`notElem` sunken) corvetas
    let remainingFragatas = filter (\(fx1, fy1, fx2, fy2) -> not ((fx1, fy1) `elem` sunken || (fx2, fy2) `elem` sunken)) fragatas

    let dicaCorvetaX = length $ filter (\(cx, _) -> cx == x) remainingCorvetas
    let dicaCorvetaY = length $ filter (\(_, cy) -> cy == y) remainingCorvetas
    putStrLn $ "\nDica " ++ show iteracoes ++ ": Disparo na água! \nLinha " ++ show (x + 1) ++ " -> Há " ++ show dicaCorvetaX ++ " corvetas nesta linha\nColuna " ++ show (y + 1) ++ " -> Há " ++ show dicaCorvetaY ++ " corvetas nesta coluna\n"

    let dicaFragataX = length $ filter (\(fx1, _, fx2, _) -> fx1 == x || fx2 == x) remainingFragatas
    let dicaFragataY = length $ filter (\(_, fy1, _, fy2) -> fy1 == y || fy2 == y) remainingFragatas
    putStrLn $ "\nLinha " ++ show (x + 1) ++ " -> Há " ++ show dicaFragataX ++ " fragatas nesta linha\nColuna " ++ show (y + 1) ++ " -> Há " ++ show dicaFragataY ++ " fragatas nesta coluna\n"

gameLoop :: Battlefield -> Ship -> [(Int, Int, Int, Int)] -> [Position] -> Ship -> Int -> Int -> Int -> IO ()
gameLoop battlefield corvetas fragatas hits sunken municao iteracoes naviosAfundados
    | naviosAfundados == 6 = endGame True battlefield
    | iteracoes >= municao = endGame False battlefield
    | otherwise = do
        visualizaCampoDeBatalha battlefield
        disparo <- atirar
        let acerto = tiroCerto disparo corvetas fragatas
        let hits' = if acerto then disparo : hits else hits
        let battlefield' = if acerto
                           then alteraCampoDeBatalha disparo battlefield 1
                           else alteraCampoDeBatalha disparo battlefield (-1)
        let newSunken = if acerto then disparo : sunken else sunken
        if acerto
            then do
                putStrLn "Acertou um navio!"
                gameLoop battlefield' corvetas fragatas hits' newSunken municao (iteracoes + 1) (naviosAfundados + 1)
            else do
                dicaErro disparo iteracoes corvetas fragatas newSunken
                gameLoop battlefield' corvetas fragatas hits' newSunken municao (iteracoes + 1) naviosAfundados

endGame :: Bool -> Battlefield -> IO ()
endGame True battlefield = do
    visualizaCampoDeBatalha battlefield
    putStrLn "\n\nFim de jogo! Você ganhou!\n\nVocê afundou todos os navios piratas e conseguiu conter a invasão inimiga.\n\nParabéns, marujo, argh!\n\n"
endGame False battlefield = do
    visualizaCampoDeBatalha battlefield
    putStrLn "\n\nFim de jogo! Você perdeu...\n\nAlguns navios piratas conseguiram passar pelas suas defesas e seu país foi invadido...\nBoa sorte na próxima!\n\n"
