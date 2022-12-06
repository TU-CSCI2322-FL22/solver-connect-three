-- evaluation stuff: difference between how many moves away X and O are from winning in a microgame
--                   ~~average~~ (weighted?) SUM between microgame evaluations for macrogame
--                   # of outs, or possible wins - look for three non-O's in a row - open spaces are a possible win for X
--                   subtract possible wins from both sides
--                   should look "very very similar" to checkWinner code

module Solver where
import Game
-- I love importing things
import Data.List.Split
import Control.Monad
import Debug.Trace
import System.IO
import System.Environment
import Data.Char
import Control.Exception

-- ***maybe*** make a type alias where Score = Int

predictWin :: Macrogame -> Victory
predictWin macgame@(macroboard, player) =
    case checkMacrowin macroboard of 
        Just outcome -> outcome
        Nothing -> --if (winningMoves macgame /= []) then Won player
                  -- else 
                       let valPlays      = validPlays macgame
                           childStates   = [makePlay play macgame | play <- valPlays]
                           listOfFutures = [predictWin state | state <- childStates]
                       in bestVicFor player listOfFutures
    where bestVicFor player listOfFutures = 
              if (Won player) `elem` listOfFutures then Won (snd macgame)
                  else if (Tie `elem` listOfFutures) then Tie
                  else if null listOfFutures then traceShow macgame $ error "AAAH" 
                   else head listOfFutures

-- this is the version with depth for the final milestone
--
-- SHOULD RETURN A SCORE NOT A VICTORY
-- IF DEPTH 0 THEN CALL EVAL FUNCTION
predictMightWin :: Macrogame -> Int -> Int
predictMightWin macgame@(macroboard, player) depth =
    if (depth == 0) then scoreGame macgame
    else 
        case checkMacrowin macroboard of 
            Just outcome -> scoreGame macgame
            Nothing      -> let valPlays      = validPlays macgame
                                childStates   = [makePlay play macgame | play <- valPlays]
                                listOfFutures = [predictMightWin state (depth - 1) | state <- childStates]
                            in  bestScoreFor player listOfFutures
    -- can't use elem for bestScoreFor
    where bestScoreFor player listOfFutures =
              if (player == X) then maximum listOfFutures
              else minimum listOfFutures 
            --  if (Won player) `elem` listOfFutures then Won (snd macgame)
            --      else if (Tie `elem` listOfFutures) then Tie
             --     else if null listOfFutures then traceShow macgame $ error "AAAH" 
              --    else head listOfFutures


-- taking in Macrogame to determine winning moves for current Player in (Macroboard, Player) tuple
-- "chooses the move with the best outcome for the current player"
--
-- priority: winning moves for current player
-- secondary: moves that block other play from winning on their next turn
-- 
-- change type signature to exclude [Play] at some point? leave Macrogame -> ...
winningMoves :: Macrogame -> [Play] --Checking for microboard tiles that result in a macroboard win (and tyingMoves checks for microboard tiles that result in a macroboard tie)
winningMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just (Won (snd macgame))]

--winningMicroMoves :: Microgame -> Player -> [Int]
--winningMicroMoves micgame = [play | (_,play) ]

--winningOMoves :: Macrogame -> Play
--winningOMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Won O]

tyingMoves :: Macrogame -> [Play]
tyingMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just Tie]

-- These type signatures on Dr. Fogarty's website have "Game" instead of "Macrogame" - be cautious - we might have to write these for microgames too??

bestPlay :: Macrogame -> Play
bestPlay macgame@(macroboard, player) =
    let valPlays      = validPlays macgame
        childStates   = [(makePlay play macgame, play) | play <- valPlays]
        listOfFutures = [(predictWin (fst state), snd state) | state <- childStates]
    in bestPlayFor player listOfFutures
    where bestPlayFor :: Player -> [(Victory, Play)] -> Play
          bestPlayFor player [f] = snd f
          bestPlayFor player (f:fs) =
              let vicList = [vic | (vic, play) <- (f:fs)]
              in
                  if (fst f) == (Won player) then snd f
                  else if ((fst f) == Tie && not ((Won player) `elem` vicList)) then snd f
                  else bestPlayFor player fs

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

scoreGame :: Macrogame -> Int
scoreGame game =
    --use winsInNMoves to get a score for each board, then take the sum of all the scores where x is positive and o is negative
    let
        xScore = 20* length [microgame | microgame <- fst game, snd microgame == Just (Won X)] + 10*sum [winsInNMoves (fst game) i X 1 | (b, i) <-zip (fst game) [0..]] + 
            sum [winsInNMoves (fst game) i X 2 | (b, i) <- zip (fst game) [0..]]
        oScore = 20* length [microgame | microgame <- fst game, snd microgame == Just (Won O)] + 10*sum [winsInNMoves (fst game) i O 1 | (b, i) <- zip (fst game) [0..]] + 
            sum [winsInNMoves (fst game) i O 2 | (b, i) <- zip (fst game) [0..]]
    in
        if snd game == X then
            xScore + 5 - oScore 
        else if snd game == O then
            xScore - 5 - oScore
        else xScore - oScore

winsInNMoves :: Macroboard -> Int -> Player -> Int -> Int
winsInNMoves macboard mictile player n = 
    let
        --valPlays = [play | play <- validPlays (macboard, player), fst play == mictile]
        winMoves = [play | play <- winningMoves (macboard, player), fst play == mictile]
    in
        if (winMoves /= []) then (length winMoves)
        else if (n == 0) then 0
        else let valPlays = [play | play <- validPlays (macboard, player), fst play == mictile] 
             in sum [winsInNMoves (fst (makePlay p (macboard, player))) mictile player (n-1) | p <- valPlays] 


        
--comfort line
--comfort line 2
