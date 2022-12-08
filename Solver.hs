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
 


winningMoves :: Macrogame -> [Play] --Checking for microboard tiles that result in a macroboard win (and tyingMoves checks for microboard tiles that result in a macroboard tie)
winningMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just (Won (snd macgame))]

tyingMoves :: Macrogame -> [Play]
tyingMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just Tie]

winningMicroMoves :: Microgame -> Player -> [Int]
winningMicroMoves micgame player = [ play | play <- (validMicroPlays micgame), checkWin (makeMicroPlay play (fst micgame) player 0) == Just (Won player) ]

makeMicroPlay :: Int -> Microboard -> Player -> Int -> Microboard
makeMicroPlay tl (t:ts) player idx = --NEVER EVER USE THIS OUTSIDE THE CONTEXT OF SCOREGAME
    if (tl == idx) then [Just player] ++ ts
    else [t] ++ (makeMicroPlay tl ts player (idx + 1))



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

decentPlay :: Macrogame -> Int -> Play
decentPlay macgame@(macroboard, player) depth =
    let valPlays      = validPlays macgame
        childStates   = [(makePlay play macgame, play) | play <- valPlays]
        listOfFutures = [(predictMightWin (fst state) (depth - 1), snd state) | state <- childStates]
    in bestPlayFor player (tail listOfFutures) (head listOfFutures)
    where bestPlayFor :: Player -> [(Int, Play)] -> (Int, Play) -> Play
          bestPlayFor player [] future = snd future 
          bestPlayFor player (f:fs) currFut =
              if (player == X) then
                  if (fst f > fst currFut) then bestPlayFor player fs f
                  else bestPlayFor player fs currFut
              else
                  if (fst f < fst currFut) then bestPlayFor player fs f
                  else bestPlayFor player fs currFut
              


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
        board0 = [if cell == Just X then "X," else if cell == Just O then "O," else "E," | cell <- fst b0]
        board1 = [if cell == Just X then "X," else if cell == Just O then "O," else "E," | cell <- fst b1]
        board2 = [if cell == Just X then "X," else if cell == Just O then "O," else "E," | cell <- fst b2]
        board3 = [if cell == Just X then "X," else if cell == Just O then "O," else "E," | cell <- fst b3]
        board4 = [if cell == Just X then "X," else if cell == Just O then "O," else "E," | cell <- fst b4]
        board5 = [if cell == Just X then "X," else if cell == Just O then "O," else "E," | cell <- fst b5]
        board6 = [if cell == Just X then "X," else if cell == Just O then "O," else "E," | cell <- fst b6]
        board7 = [if cell == Just X then "X," else if cell == Just O then "O," else "E," | cell <- fst b7]
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
    let g = readGame str
    putStrLn $ show g
    return $ g

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
    let board = fst game
        xScore = sum [scoreMicrogame (fst micgame) X 0 | micgame <- board]
        oScore = sum [scoreMicrogame (fst micgame) O 0 | micgame <- board]
    in if (snd game == X) then 5 + (xScore - oScore)
       else (xScore - oScore) - 5

  where indexBoard :: Microboard -> Int -> Maybe Player
        indexBoard [a,b,c,d,e,f,g,h,i] 0 = a
        indexBoard [a,b,c,d,e,f,g,h,i] 1 = b
        indexBoard [a,b,c,d,e,f,g,h,i] 2 = c
        indexBoard [a,b,c,d,e,f,g,h,i] 3 = d
        indexBoard [a,b,c,d,e,f,g,h,i] 4 = e
        indexBoard [a,b,c,d,e,f,g,h,i] 5 = f
        indexBoard [a,b,c,d,e,f,g,h,i] 6 = g
        indexBoard [a,b,c,d,e,f,g,h,i] 7 = h
        indexBoard [a,b,c,d,e,f,g,h,i] 8 = i

        checkSpaces :: Int -> [(Int, Int)]
        checkSpaces 0 = [(1,2), (4,8), (3,6)]
        checkSpaces 1 = [(0,2), (4,7)]
        checkSpaces 2 = [(4,6), (0,1), (5,8)]
        checkSpaces 3 = [(0,6), (4,5)]
        checkSpaces 4 = [(3,5), (1,7), (0,8), (2,6)]
        checkSpaces 5 = [(2,8), (3,4)]
        checkSpaces 6 = [(0,3), (7,8), (2,4)]
        checkSpaces 7 = [(1,4), (6,8)]
        checkSpaces 8 = [(2,5), (6,7), (0,4)]
        checkSpaces a = []
 
        scoreMicrogame :: Microboard -> Player -> Int -> Int
        scoreMicrogame micboard player 9 = 0
        scoreMicrogame micboard player tileNum =
            let spaces = checkSpaces tileNum
                scoreLST = [if (indexBoard micboard (fst space)) == Nothing && (indexBoard micboard (snd space)) == Nothing then 1
                                else if (indexBoard micboard (fst space)) == Just player && (indexBoard micboard (snd space)) == Just player then 20
                                else if (indexBoard micboard (fst space)) == Just player && (indexBoard micboard (snd space)) == Nothing then 10
                                else if (indexBoard micboard (fst space)) == Nothing && (indexBoard micboard (snd space)) == Just player then 10
                                else 0 | space <- spaces, indexBoard micboard tileNum == Just player]
                score   = trace (show player ++ ": " ++ show scoreLST) (sum scoreLST)
            in
                if (tileNum `elem` [0,2,6,8]) then (2 * score) + scoreMicrogame micboard player (tileNum + 1)
                else if (tileNum == 4) then (4 * score) + scoreMicrogame micboard player (tileNum + 1)
                else score + scoreMicrogame micboard player (tileNum + 1)

        {-
        determinePlayerScore :: Macroboard -> Player -> Int -> Int
        determinePlayerScore board player 9 = 0
        determinePlayerScore (b:bs) player tileNum =
            let spaces = checkSpaces tileNum
                micboard = fst b
                scoreLST = [if (indexBoard micboard (fst space)) == Nothing && (indexBoard micboard (snd space)) == Nothing then 1
                                else if (indexBoard micboard (fst space)) == Just player && (indexBoard micboard (snd space)) == Just player then 20
                                else if (indexBoard micboard (fst space)) == Just player && (indexBoard micboard (snd space)) == Nothing then 10
                                else if (indexBoard micboard (fst space)) == Nothing && (indexBoard micboard (snd space)) == Just player then 10
                                else 0 | space <- spaces, indexBoard micboard tileNum == Just player]
                score   = trace (show player ++ ": " ++ show scoreLST) (sum scoreLST)
            in
                if (tileNum `elem` [0,2,6,8]) then (2 * score) + determinePlayerScore bs player (tileNum + 1)
                else if (tileNum == 4) then (4 * score) + determinePlayerScore bs player (tileNum + 1)
                else score + determinePlayerScore bs player (tileNum + 1)
-}

{-

    let valPlays = validPlays game
        PotentialWins = potWins valPlays [] [] X
        oPotentialWins = potWins valPlays [] [] O
        xPotTwoMoves = potWins (snd xPotentialWins) [] [] X
        oPotTwoMoves = potWins (snd oPotentialWins) [] [] O
        xScore = 20 * length [microgame | microgame <- fst game, snd microgame == Just (Won X)]
                     + 10 * length xPotentialWins + length xPotTwoMoves
        oScore = 20 * length [microgame | microgame <- fst game, snd microgame == Just (Won O)]
                     + 10 * length oPotentialWins + length oPotTwoMoves   
    in if snd game == X then  xScore + 5 - oScore 
       else if snd game == O then (xScore - 5) - oScore
       else xScore - oScore
 where potWins :: [Play] -> [Play] -> [Play] -> Player -> ([Play], [Play])
       potWins [] winMoves otherMoves player = (winMoves, otherMoves)
       potWins (p:ps) winMoves otherMoves player =
           let micboard = fst $ head (take (snd p + 1) (fst game))
               stateAfterP = makeMicroPlay (fst p) micboard player 0
           in if (checkWin stateAfterP == Just (Won player)) then potWins ps (p:winMoves) otherMoves player
              else potWins ps winMoves (p:otherMoves) player
       
-}



--scoreGame :: Macrogame -> Int
--scoreGame game = 
--    let xWinningMoves = [winningMicroMoves microgame X | microgame <- fst game]
--        oWinningMoves = [winningMicroMoves microgame O | microgame <- fst game]
--        xScore = 20 * length [microgame | microgame <- fst game, snd microgame == Just (Won X)] +
--                     10 * length xWinningMoves
--                     + length [state | state <- xWinningChildStates xWinningMoves, (checkWin state) == Just (Won X)]
--        oScore = 20 * length [microgame | microgame <- fst game, snd microgame == Just (Won O)] +
--                     10 * length oWinningMoves
--                     + length [state | state <- oWinningChildStates oWinningMoves, (checkWin state) == Just (Won O)]
--    in 
--        if snd game == X then  xScore + 5 - oScore 
--        else if snd game == O then (xScore - 5) - oScore
--        else xScore - oScore
--  where xWinningChildStates :: [[Int]] -> [Microboard]
--        xWinningChildStates [] = []
--        xWinningChildStates (l:ls) = [makeMicroPlay tl (fst microgame) X 0 | microgame <- fst game, tl <- l] ++ xWinningChildStates ls
--      
--        oWinningChildStates :: [[Int]] -> [Microboard]
--        oWinningChildStates [] = []
--        oWinningChildStates (l:ls) = [makeMicroPlay tl (fst microgame) O 0 | microgame <- fst game, tl <- l] ++ oWinningChildStates ls
            


    --use winsInNMoves to get a score for each board, then take the sum of all the scores where x is positive and o is negative
   -- let
   --     xScore = 20* length [microgame | microgame <- fst game, snd microgame == Just (Won X)] + 10*sum [winsInNMoves (fst game) i X 1 | (b, i) <-zip (fst game) [0..]] + 
   --         sum [winsInNMoves (fst game) i X 2 | (b, i) <- zip (fst game) [0..]]
   --     oScore = 20* length [microgame | microgame <- fst game, snd microgame == Just (Won O)] + 10*sum [winsInNMoves (fst game) i O 1 | (b, i) <- zip (fst game) [0..]] + 
   --         sum [winsInNMoves (fst game) i O 2 | (b, i) <- zip (fst game) [0..]]
   -- in
    --    if snd game == X then
    --        xScore + 5 - oScore 
   --     else if snd game == O then
  --          xScore - 5 - oScore
  --      else xScore - oScore

--winsInNMoves :: Macroboard -> Int -> Player -> Int -> Int
--winsInNMoves macboard mictile player n = 
--    let
--        --valPlays = [play | play <- validPlays (macboard, player), fst play == mictile]
--        winMoves = [play | play <- winningMoves (macboard, player), fst play == mictile]
--    in
--        if (winMoves /= []) then (length winMoves)
--        else if (n == 0) then 0
--        else let valPlays = [play | play <- validPlays (macboard, player), fst play == mictile] 
--             in sum [winsInNMoves (fst (makePlay p (macboard, player))) mictile player (n-1) | p <- valPlays] 

        
--comfort line
--comfort line 2
