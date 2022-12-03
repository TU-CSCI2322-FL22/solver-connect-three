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

main :: IO ()
main = do
        args <- getArgs
        let (flags, inputs, error) = getOpt Permute options args
        putStrLn $ show (flags, inputs, error)
        let num = getNumber flags

getNumber :: [Flag] -> Int
getNumber ((Number x):_) = 
  case readMaybe x of
    Nothing -> error "That's not a number. Try again." 
    Just n -> n
getNumber (_:flags) = getNumber flags
getNumber [] = 1
