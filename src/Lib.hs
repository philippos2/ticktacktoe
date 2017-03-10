module Lib
    ( playTicktacktoe
    ) where

import qualified System.Console.Readline  as Readline
import qualified Data.Map                 as Map
import qualified Data.List                as List
import qualified Data.List.Split          as Split

import Control.Monad.State

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

{------------------------------------------------
 盤面描画
 ------------------------------------------------}
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn . unlines . List.intersperse (partition board)
    . map (\x -> List.intercalate "|" x) . rows $ Map.elems board
  where
    rows      board = filter (/= []) . map lines . Split.splitOn "W\n" . unlines $ map show board
    partition board = concat . List.intersperse "+" . replicate (lengthof board) $ "-"
    lengthof  board = head . map length . map concat . rows $ Map.elems board

-------------------------------------------------------------------------------

{------------------------------------------------
 ゲームルーチン
 ------------------------------------------------}
play :: Bool -> StateT Board IO String
play isMachinesTurn = do
  if isMachinesTurn
    then machinesTurn
    else usersTurn

  board <- get
  liftIO $ printBoard board
  liftIO $ print board
  case judge board of
    Nothing     -> (play $ not isMachinesTurn)
    Just result -> return result

{------------------------------------------------
 ユーザ側ターン
 ------------------------------------------------}
usersTurn :: StateT Board IO Int
usersTurn = do
  liftIO $ putStrLn "your turn: "
  board    <- get
  position <- liftIO $ getPosition board
  updateBoard Nought position >> return position
  where
    getPosition board = do
      input <- getLine
      if elem input . map show . Map.keys $ Map.filter (== Blank) board
        then return (read input :: Int)
        else putStrLn "wrong input" >> getPosition board

{------------------------------------------------
 CPU側ターン
 ------------------------------------------------}
machinesTurn :: StateT Board IO Int
machinesTurn = do
  liftIO $ putStrLn "CPU selected: "
  board <- get
  return 0

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
  | wonBy Cross   board = return "won by machine."
  | isThereBlank  board = return "drawn."
  | otherwise           = Nothing
  where
    isThereBlank board = Nothing == (List.elemIndex Blank $ Map.elems board)

wonBy :: Mark -> Board -> Bool
wonBy mark board = False

