module Lib
    ( playTicktacktoe
    ) where

import qualified System.Console.Readline as Readline
import qualified Data.Map as Map
import qualified Data.List as List

import Control.Monad.State

-------------------------------------------------------------------------------
type Board = Map.Map Integer Mark

data Mark = Nought | Cross | Blank | Wall deriving Eq
instance Show Mark where
  show Nought = "N"
  show Cross  = "C"
  show Blank  = " "
  show Wall   = "W"

initBoard :: (Integer, Integer) -> Board
initBoard (x, y) = Map.fromList $ map genCell [0..(x * y - 1)]
  where genCell n
                | mod (n + 1) x > 0 && n < (x * y - 1) - x = (n, Blank)
                | otherwise = (n, Wall)

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn . unlines . List.intersperse (partition board)
    . map (\x -> List.intercalate "|" x) . rows $ Map.elems board
  where
    rows      board = map (map show) . filter (\xs -> notElem Wall $ [head xs]) $ List.group board
    partition board = concat . List.intersperse "+" . replicate (lengthof board) $ "-"
    lengthof  board = head . map length . map concat . rows $ Map.elems board

-------------------------------------------------------------------------------
play :: Bool -> StateT Board IO String
play isMachinesTurn = do
  if isMachinesTurn
    then machinesTurn
    else usersTurn

  board <- get
  liftIO $ printBoard board
  case judge board of
    Nothing     -> (play $ not isMachinesTurn)
    Just result -> return result

usersTurn :: StateT Board IO Integer
usersTurn = do
  liftIO $ putStrLn "your turn: "
  position <- liftIO getLine
  board    <- get
  return 0

machinesTurn :: StateT Board IO Integer
machinesTurn = do
  liftIO $ putStrLn "CPU select: "
  board <- get
  return 0

judge :: Board -> Maybe String
judge board
  | otherwise = Nothing

-------------------------------------------------------------------------------
playTicktacktoe :: IO ()
playTicktacktoe = do 
  let board = initBoard (9, 9)
  -- print board
  printBoard board
  evalStateT (play False) board >>= putStrLn

--someFunc :: IO ()
--someFunc = putStrLn "someFunc"
