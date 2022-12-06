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
options = [ Option ['w'] ["winner"] (NoArg Winner) "Print out the best move using an exhaustive search." 
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Prints out the best move within a cutoff depth of potential future moves." 
          , Option ['h'] ["help"] (NoArg Help) "Prints usage information."
          , Option ['m'] ["move"] (ReqArg Move "<move>") "Makes a move and prints out the resulting board, in the input format, to stdout. The move should be formatted with quotes around a tuple."
          , Option ['v'] ["verbose"] (NoArg Verbose) "Outputs both the move and a description of how good it is."
          ]


getDepth :: [Flag] -> Int
getDepth [] = 5
getDepth ((Depth x):_) = 
  case readMaybe x of
    Nothing -> error "Invalid input to depth flag"
    Just num -> num
getDepth (_:flags) = getDepth flags

getMove :: [Flag] -> Macrogame -> Maybe Play
getMove [] game = Nothing
getMove ((Move x):_) game =
    case readMaybe x of
        Nothing -> error "Invalid input to move flag"
        Just move -> Just (fst move - 1, snd move - 1) --Our moves are 0-indexed internally, so this is to handle 1-indexing on inputs
getMove (_:flags) game = getMove flags game

main :: IO ()
main =
  do args <- getArgs
     let (flags, inputs, error) = getOpt Permute options args
     if Help `elem` flags || (not $ null error)
     then putStrLn $ usageInfo "Usage: Supreme Tic Tac Toe [options] [file]" options
     else do
             let fname = if null inputs then "ZZemptyboard.csv" else head inputs
             fileString <- loadGame fname
             let game = readGame $ showGame fileString
             chooseAction flags game
             --specifiedDepth game (getDepth flags)
          --   Nothing -> 
          --       case getMove flags game of
          --           Just move -> makeMoveAndPrintBoard game move
          --           Nothing -> chooseAction flags game

chooseAction :: [Flag] -> Macrogame -> IO ()
chooseAction flags game
    | (Winner `elem` flags) = putStrLn $ show (bestPlay game)
--    | (Verbose `elem` flags) = verboseFunction game (fromJust $ getMove flags game)
    | otherwise = do case getMove flags game of
                       Nothing -> giveGoodMove game (getDepth flags)
                       Just move ->
                           if (Verbose `elem` flags) then
                               let scoreAfterPlay = scoreGame (makePlay move game)
                               in
                                   if (scoreAfterPlay < 0) then putStrLn $ "The play " ++ show move ++ " is good for O with a score of " ++ show (-1 * scoreAfterPlay)
                                   else putStrLn $ "The play " ++ show move ++ " is good for X with a score of " ++ show (scoreAfterPlay)
                           else makeMoveAndPrintBoard game move

hasMove :: Maybe Play -> (Bool, Play)
hasMove mplay =
    case mplay of
        Nothing -> (False, (0,0))
        Just play -> (True, play) 

  --exhaustiveDepth :: IO()
  --exhaustiveDepth = putStrLn $ show (bestPlay game)

--specifiedDepth :: Macrogame -> Int -> IO()
--specifiedDepth game depth = putStrLn $ show (predictMightWin game depth)

makeMoveAndPrintBoard :: Macrogame -> Play -> IO()
-- I hope you like the generous use of the dollar sign notation in this very line of code
makeMoveAndPrintBoard game play = putStrLn $ showMacroboard $ fst $ makePlay play game

--verboseFunction :: Macrogame -> Play -> IO()
--verboseFunction game play =
-- I love backwards syntax; don't you??????????????????????
--        let scoreAfterPlay = scoreGame (makePlay play game)
--        in 
--                if (scoreAfterPlay < 0) then putStrLn $ "The play " ++ show play ++ " is good for O with a score of " ++ show (-1 * scoreAfterPlay) 
--                else putStrLn $ "The play " ++ show play ++ " is good for X with a score of " ++ show (scoreAfterPlay)

giveGoodMove :: Macrogame -> Int -> IO()
giveGoodMove game depth = putStrLn $ show $ decentPlay game depth



