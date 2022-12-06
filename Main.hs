module Main where
import Game
import Solver
-- I love importing things (part 2)
import Data.Char
import System.IO
import Control.Monad
import System.Exit
import Text.Read (readMaybe)
import System.Environment
import System.Console.GetOpt
import Data.Maybe
import Data.List.Split
import Debug.Trace
import Control.Exception

data Flag = Winner | Depth String | Help | Move String | Verbose deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg Winner) "Print out the best move, using an exhaustive search (no cut-off depth)." 
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Use <num> as a cutoff depth, instead of your default." 
          , Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['m'] ["move"] (ReqArg Move "<move>") "Make <move> and print out the resulting board, in the input format, to stdout. The move should be 1-indexed. If a move requires multiple values, the move will be a tuple of numbers separated by a comma with no space."
          , Option ['v'] ["verbose"] (NoArg Verbose) "Output both the move and a description of how good it is: win, lose, tie, or a rating."
          ]

getNum :: [Flag] -> Int
getNum [] = 1
getNum ((Num x):_) = 
  case readMaybe x of
    Nothing -> error "Invalid input to num flag"
    Just num -> num
getNum (_:flags) = getNum flags

getMove :: [Flag] -> Play
getMove [] = head $ validPlays game
getMove ((Move x):_) =
    case readMaybe x of
        Nothing -> error "Invalid input to move flag"
        Just move -> move
getMove (_:flags) = getMove flags

getStart :: [Flag] -> IO Int
getStart ((Start x):_) = 
  case readMaybe x of
    Nothing -> error "Invalid input to start flag"
    Just num -> return num
getStart (_:flags) = getStart flags
getStart [] = 
  --needs change
  do name <- prompt "What is your name"
     putStrLn $ "Hello " ++ name ++ "!"
     return $ indexOfName name 


main :: IO ()
main =
  do args <- getArgs
     let (flags, inputs, error) = getOpt Permute options args
     let fname = if null inputs then "emptyboard.csv" else head inputs
     fileString <- loadGame fname
     let game = readGame fileString
     if Help `elem` flags || (not $ null error)
     then putStrLn $ usageInfo "Usage: Supreme Tic Tac Toe [options] [file]" options
     else do
       index <- getStart flags
       (chooseAction flags) index game

chooseAction :: [Flag] -> Int -> Macrogame -> IO ()
chooseAction flags 
  | Winner `elem` flags = putStrLn $ show (bestPlay game)
  | Depth`elem` flags = specifiedDepth (getNum flags)
  | Move `elem` flags =  makeMoveAndPrintBoard (getMove flags)
  | verbose `elem` flags = verboseFunction
  | otherwise = giveGoodMove 5

  --exhaustiveDepth :: IO()
  --exhaustiveDepth = putStrLn $ show (bestPlay game)

  specifiedDepth :: Int -> IO()
  specifiedDepth depth = putStrLn $ show (predictMightWin game depth)

  makeMoveAndPrintBoard :: Play -> IO()
  makeMoveAndPrintBoard play = putStrLn $ showMacroboard (makePlay play game)

  verboseFunction :: Play -> IO()
  verboseFunction play =
      let scoreAfterPlay = scoreGame (makePlay play game)
      in 
        if (scoreAfterPlay < 0) then putStrLn $ "The play " ++ show play ++ " is good for O with a score of " ++ show (scoreAfterPlay * -1)
        else "The play " ++ show play ++ " is good for X with a score of " ++ show (scoreAfterPlay)


