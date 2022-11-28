-- evaluation stuff: difference between how many moves away X and O are from winning in a microgame
--                   average between microgame evaluations for macrogame

module Solver where
import Game
-- I love importing things
import Data.List.Split
import Control.Monad
import System.IO
import System.Environment
import Data.Char
import Control.Exception

predictWin :: Macrogame -> Victory
predictWin macgame =
    let play          = bestPlay macgame
        gameAfterPlay = makePlay play macgame
    in if (checkMacrowin (fst gameAfterPlay)) == Just (Won X) then Won X --fromJust wasn't working lol
       else if (checkMacrowin (fst gameAfterPlay)) == Just (Won O) then Won O
       else if (checkMacrowin (fst gameAfterPlay)) == Just Tie then Tie
       else predictWin gameAfterPlay

--    let valPlays = validPlays macgame
--    in if (winningMoves macgame) /= [] then Won (snd macgame)
--       else aux valPlays macgame
--                where aux [] game = if (snd game == X) then Won O else Won X
--                      aux (p:ps) game =
--                          let gameAfterPlay = makePlay p game
--                          in if (checkMacrowin gameAfterPlay == Just (Won (snd game))) then Won (snd game)
--                             else if (winningMoves gameAfterPlay) /= [] then aux ps
--                                  else undefined
                                          

--let winMoves = winningMoves macgame
--        --tieMoves = tyingMoves macgame
--    in if (winMoves /= []) then Won (snd macgame)
--       else aux (validPlays macgame) macgame
--                where aux [] game = 
--                      aux (m:ms) game =
--                          let gameAfterPlay = makePlay m macgame
--                          in 
--                              if (winningMoves gameAfterPlay) /= [] then aux ms game
--                              else aux (validPlays gameAfterPlay) gameAfterPlay

-- taking in Macrogame to determine winning moves for current Player in (Macroboard, Player) tuple
-- "chooses the move with the best outcome for the current player"
--
-- priority: winning moves for current player
-- secondary: moves that block other play from winning on their next turn
-- 
-- change type signature to exclude [Play] at some point? leave Macrogame -> ...
winningMoves :: Macrogame -> [Play] --Checking for microboard tiles that result in a macroboard win (and tyingMoves checks for microboard tiles that result in a macroboard tie)
winningMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just (Won (snd macgame))]

--winningOMoves :: Macrogame -> Play
--winningOMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Won O]

tyingMoves :: Macrogame -> [Play]
tyingMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just Tie]

-- tyingMoves
-- "                                                                                                              "-ish == Tie

-- These type signatures on Dr. Fogarty's website have "Game" instead of "Macrogame" - be cautious - we might have to write these for microgames too??

bestPlay :: Macrogame -> Play
bestPlay macgame =
    let winMoves = winningMoves macgame 
        valPlays = validPlays macgame
    in if (winMoves /= []) then head winMoves
       else aux valPlays (head valPlays)
                where aux [] best = best
                      aux (p:ps) best =
                          let gameAfterPlay = makePlay p macgame
                          in if (winningMoves gameAfterPlay) /= [] then aux ps best
                             else if (tyingMoves gameAfterPlay) /= [] then p
                          else aux ps p

-- Joey
readGame :: String -> Macrogame
readGame str = 
        let
                [currentPlayer, b0, b1, b2, b3, b4, b5, b6, b7, b8] = lines str
                curr = if currentPlayer == "X" then X else O
                board0 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b0]
                board1 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b1]
                board2 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b2]
                board3 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b3]
                board4 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b4]
                board5 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b5]
                board6 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b6]
                board7 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b7]
                board8 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b8]
        in
                ([(board0, checkWin board0), (board1, checkWin board1), (board2, checkWin board2), (board3, checkWin board3), (board4, checkWin board4), (board5, checkWin board5), (board6, checkWin board6), (board7, checkWin board7), (board8, checkWin board8)], curr)

-- Joey
showGame :: Macrogame -> String
showGame macgame = 
    let
        [b0, b1, b2, b3, b4, b5, b6, b7, b8] = fst macgame
        curr = snd macgame
     -- [b0, b1, b2, b3, b4, b5, b6, b7, b8] = fst macgame
        currentPlayer = if curr == X then "X" else "O"
        board0 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b0]
        board1 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b1]
        board2 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b2]
        board3 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b3]
        board4 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b4]
        board5 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b5]
        board6 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b6]
        board7 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b7]
        board8 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- fst b8]
    in
        unlines [currentPlayer, concat board0, concat board1, concat board2, concat board3, concat board4, concat board5, concat board6, concat board7, concat board8]

-- TO-DO
-- writes output of showGame to a file
writeGame :: Macrogame -> FilePath -> IO ()
writeGame macgame flpath = 
        let str = showGame macgame
        in writeFile flpath str

-- TO-DO
loadGame :: FilePath -> IO Macrogame
loadGame flpath = do
    str <- readFile flpath 
    evaluate $ readGame str --wtf

--     str <- readFile flpath
--     retgame <- readGame str
--     retgame

-- TO-DO
putWinner :: Macrogame -> IO ()
putWinner macgame = do
    let play = bestPlay macgame
    putStrLn $ "(" ++ show (fst play) ++ "," ++ show (snd play) ++ ")"

--comfort line
--comfort line 2