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

getDepth :: [Flag] -> Maybe Int
getDepth [] = Nothing
getDepth ((Depth x):_) = 
  case readMaybe x of
    Nothing -> error "Invalid input to depth flag"
    Just num -> Just num
getDepth (_:flags) = getDepth flags

getMove :: [Flag] -> Macrogame -> Maybe Play
-- getMove [] game = head $ validPlays game
getMove [] game = Nothing
getMove ((Move x):_) game =
    case readMaybe x of
        Nothing -> error "Invalid input to move flag"
        Just move -> Just move
getMove (_:flags) game = getMove flags game

main :: IO ()
main =
  do args <- getArgs
     let (flags, inputs, error) = getOpt Permute options args
     let fname = if null inputs then "emptyboard.csv" else head inputs
     fileString <- loadGame fname
     let game = readGame $ showGame fileString
     if Help `elem` flags || (not $ null error)
     then putStrLn $ usageInfo "Usage: Supreme Tic Tac Toe [options] [file]" options
     --else if null inputs then giveGoodMove game 5
     else do
         case getDepth flags of
             Just depth -> specifiedDepth game depth
             Nothing -> 
                 case getMove flags game of
                     Just move -> makeMoveAndPrintBoard game move
                     Nothing -> chooseAction flags game
--     else chooseAction flags game

chooseAction :: [Flag] -> Macrogame -> IO ()
chooseAction flags game
    | (Winner `elem` flags) = putStrLn $ show (bestPlay game)
    | (Verbose `elem` flags) = verboseFunction game (fromJust $ getMove flags game)
    | otherwise = giveGoodMove game 5

  --exhaustiveDepth :: IO()
  --exhaustiveDepth = putStrLn $ show (bestPlay game)

specifiedDepth :: Macrogame -> Int -> IO()
specifiedDepth game depth = putStrLn $ show (predictMightWin game depth)

makeMoveAndPrintBoard :: Macrogame -> Play -> IO()
-- I hope you like the generous use of the dollar sign notation in this very line of code
makeMoveAndPrintBoard game play = putStrLn $ showMacroboard $ fst $ makePlay play game

verboseFunction :: Macrogame -> Play -> IO()
verboseFunction game play =
-- I love backwards syntax; don't you??????????????????????
        let scoreAfterPlay = scoreGame (makePlay play game)
        in 
                if (scoreAfterPlay < 0) then putStrLn $ "The play " ++ show play ++ " is good for O with a score of " ++ show (-1 * scoreAfterPlay) 
                else putStrLn $ "The play " ++ show play ++ " is good for X with a score of " ++ show (scoreAfterPlay)

giveGoodMove :: Macrogame -> Int -> IO()
giveGoodMove game depth = putStrLn $ show $ decentPlay game depth



