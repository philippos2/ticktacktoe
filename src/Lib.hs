module Lib
    ( playTicktacktoe
    ) where

import qualified System.Console.Readline  as Readline
import qualified Data.Map                 as Map
import qualified Data.List                as List
import qualified Data.List.Split          as Split
import qualified Data.Maybe               as Maybe

import Control.Monad.State
import System.Exit

-------------------------------------------------------------------------------
type Board = Map.Map Int Mark

data Mark = Nought | Cross | Blank | Wall deriving Eq
instance Show Mark where
  show Nought = "O"
  show Cross  = "X"
  show Blank  = " "
  show Wall   = "W"

{------------------------------------------------
 メインルーチン
 ------------------------------------------------}
playTicktacktoe :: IO ()
playTicktacktoe = do 
  let board = initBoard (4, 4)
  printBoard board
  evalStateT (play False) board >>= putStrLn

{------------------------------------------------
 盤面初期化
 ------------------------------------------------}
initBoard :: (Int, Int) -> Board
initBoard (x, y) = Map.fromList $ map genCell [0..(x * y - 1)]
  where
    genCell n
            | mod (n + 1) x > 0 && n < (x * y - 1) - x = (n, Blank)
            | otherwise = (n, Wall)

cellLines :: Board -> [[Int]]
cellLines board = rows board ++ columns board ++ diag1 board ++ diag2 board
  where
    rows    board = filter (/= []) . Split.splitWhen (\n -> (elem n . Map.keys $ Map.filter (== Wall) board)) $ Map.keys board
    columns board = map (\n -> map (\x -> x !! n) $ rows board) [0..(length (rows board) - 1)]
    diag1   board = [map (\n -> (rows board !! n) !! n) [0..(length (rows board) - 1)]]
    diag2   board = [reverse $ map (\n -> (rows board !! (length (rows board) - 1 - n)) !! n) [0..(length (rows board) - 1)]]

{------------------------------------------------
 盤面描画
 ------------------------------------------------}
printBoard :: Board -> IO ()
printBoard board = do
  let header = "  " ++ (List.intersperse ' ' $ take (lengthof board) ['a'..]) :: String
  putStrLn header
  putStrLn . unlines . zipWith (++) (numbers board) . List.intersperse (partition board) . map (\x -> List.intercalate "|" x) . rows $ Map.elems board
  where
    rows      board = filter (/= []) . map lines . Split.splitOn "W\n" . unlines $ map show board
    partition board = concat . List.intersperse "+" . replicate (lengthof board) $ "-"
    lengthof  board = head . map length . map concat . rows $ Map.elems board
    numbers   board = map (++ " ") . List.intersperse " " $ map show [0..(lengthof board)]

-------------------------------------------------------------------------------

{------------------------------------------------
 ゲームルーチン
 ------------------------------------------------}
play :: Bool -> StateT Board IO String
play isCpusTurn = do
  if isCpusTurn
    then cpusTurn
    else usersTurn

  board <- get
  liftIO $ printBoard board
  --liftIO $ print board
  case judge board of
    Nothing     -> (play $ not isCpusTurn)
    Just result -> return result

{------------------------------------------------
 ユーザ側ターン
 ------------------------------------------------}
usersTurn :: StateT Board IO Int
usersTurn = do
  liftIO $ putStrLn "your turn (quit: q): "
  board    <- get
  position <- liftIO $ getPosition board
  updateBoard Nought position >> return position
  where
    getPosition board = do
      input <- getLine
      if elem input . map show . Map.keys $ Map.filter (== Blank) board
        then return (read input :: Int)
        else
          if input == "q"
            then exitSuccess
            else putStrLn "wrong input" >> getPosition board

{------------------------------------------------
 CPU側ターン
 ------------------------------------------------}
cpusTurn :: StateT Board IO Int
cpusTurn = do
  liftIO $ putStrLn "CPU selected: "
  board <- get
  updateBoard Cross $ position board
  return $ position board
  where
    position board = Maybe.fromJust $ strategize board
    strategize =
      block       <||>
      checkmate   <||>
      assignBlank
    f1 <||> f2 = mplus <$> f1 <*> f2

checkmate :: Board -> Maybe Int
checkmate = fillupLine Cross

block :: Board -> Maybe Int
block = fillupLine Nought

fillupLine :: Mark -> Board -> Maybe Int
fillupLine mark board =
  if null $ choises mark board
    then Nothing
    else return . head $ choises mark board
  where
    choises mark board              = concat . map (\line -> searchCheckmate mark board line) $ cellLines board
    searchCheckmate mark board line = filter (\n -> isCheckmate mark board line n && Blank == lookupMark board n) line
    isCheckmate mark board line n   = all (== mark) . map (\n -> lookupMark board n) $ filter (/= n) line
    lookupMark board n              = Maybe.fromMaybe Blank $ Map.lookup n board

assignBlank :: Board -> Maybe Int
assignBlank board =
  List.elemIndex Blank $ Map.elems board

{------------------------------------------------
 盤面更新
 ------------------------------------------------}
updateBoard :: Mark -> Int -> StateT Board IO ()
updateBoard mark position = get >>= put . (update mark position)

update :: Mark -> Int -> Board -> Board
update mark position board = Map.insert position mark board

{------------------------------------------------
 判定処理
 ------------------------------------------------}
judge :: Board -> Maybe String
judge board
  | wonBy Nought  board = return "won by user."
  | wonBy Cross   board = return "won by cpu."
  | isThereBlank  board = return "drawn."
  | otherwise           = Nothing
  where
    isThereBlank board = Nothing == (List.elemIndex Blank $ Map.elems board)

wonBy :: Mark -> Board -> Bool
wonBy mark board =
  any (== True) . map (\e -> all (== mark) $ e) $ elemLines board
  where
    elemLines     board   = map (\n -> extractMarks board n) [0..((length $ cellLines board) - 1)]
    extractMarks  board n = map (\x -> Maybe.fromMaybe Blank $ Map.lookup x board) $ cellLines board !! n

