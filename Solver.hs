-- evaluation stuff: difference between how many moves away X and O are from winning in a microgame
--                   ~~average~~ (weighted?) SUM between microgame evaluations for macrogame
--                   # of outs, or possible wins - look for three non-O's in a row - open spaces are a possible win for X
--                   subtract possible wins from both sides
--                   should look "very very similar" to checkWinner code

module Solver where
import Game
-- I love importing things
--import Data.List.Split
import Split
import Control.Monad
import Debug.Trace
import System.IO
import System.Environment
import Data.Char
import Control.Exception

scoreGame :: Macrogame -> Int
scoreGame game =
  case checkMacrowin (fst game) of
        Just (Won X) -> 1000 
        Just (Won O) -> -1000
        Just Tie -> 0
        Nothing -> 
            let weightboard = zip (fst game) [2,1,2,1,4,1,2,1,2]
                score = sum [weight * scoreMicrogame mgame | (mgame, weight) <- weightboard  ]
                {-xScore = sum [scoreMicrogame (fst micgame) X 0 | micgame <- board]
                oScore = sum [scoreMicrogame (fst micgame) O 0 | micgame <- board]-}
            in if (snd game == X) then score +5 else score - 5

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

        scoreTriple lst = 
                case ((Just X)  `elem` lst, (Just O) `elem` lst) of
                        (True, False) -> 1
                        (False, True) -> -1
                        _ -> 0 
        scoreMicrogame ([a,b,c,d,e,f,g,h,i], Nothing) = sum $ map scoreTriple [ [a,b,c], [d,e,f], [g,h,i], [a,d,g], [b,e,h], [c,f,i], [a,e,i], [c,e,g] ]
        scoreMicrogame (_, Just (Won X)) = 20
        scoreMicrogame (_, Just (Won O)) = -20
        scoreMicrogame (_, Just Tie) = 0
        
{-
        scoreMicrogame :: Microboard -> Player -> Int -> Int
        scoreMicrogame micboard player 9 = 0
        scoreMicrogame micboard player tileNum =
            let spaces = checkSpaces tileNum
                scoreLST = [if (indexBoard micboard (fst space)) == Nothing && (indexBoard micboard (snd space)) == Nothing then 1
                                else if (indexBoard micboard (fst space)) == Just player && (indexBoard micboard (snd space)) == Just player then 20
                                else if (indexBoard micboard (fst space)) == Just player && (indexBoard micboard (snd space)) == Nothing then 10
                                else if (indexBoard micboard (fst space)) == Nothing && (indexBoard micboard (snd space)) == Just player then 10
                                else 0 | space <- spaces, indexBoard micboard tileNum == Just player]
                score   = sum scoreLST
            in
                if (tileNum `elem` [0,2,6,8]) then (2 * score) + scoreMicrogame micboard player (tileNum + 1)
                else if (tileNum == 4) then (4 * score) + scoreMicrogame micboard player (tileNum + 1)
                else score + scoreMicrogame micboard player (tileNum + 1)-}


--Unbounded depth functions
predictWin :: Macrogame -> Victory
predictWin macgame@(macroboard, player) =
    case checkMacrowin macroboard of 
        Just outcome -> outcome
        Nothing ->    let valPlays      = validPlays macgame
                          childStates   = [makePlay play macgame | play <- valPlays]
                          listOfFutures = [predictWin state | state <- childStates]
                      in bestVicFor player listOfFutures
    where bestVicFor player listOfFutures = 
              if (Won player) `elem` listOfFutures then Won (snd macgame)
                  else if (Tie `elem` listOfFutures) then Tie
                  else if null listOfFutures then traceShow macgame $ error "AAAH" 
                   else head listOfFutures

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






--Bounded depth functions
predictMightWin :: Macrogame -> Int -> Int
predictMightWin macgame 0 = scoreGame macgame
predictMightWin macgame@(macroboard, player) depth =
    case checkMacrowin macroboard of 
        Just outcome -> scoreGame macgame
        Nothing -> let valPlays      = validPlays macgame
                       childStates   = [(makePlay play macgame) | play <- valPlays]
                       listOfFutures = [(predictMightWin (state) (depth - 1)) | state <- childStates]
                   in bestScoreFor player listOfFutures
  where bestScoreFor player listOfFutures =
            if (player == X) then maximum listOfFutures
            else minimum listOfFutures


decentPlay :: Macrogame -> Int -> Play
decentPlay macgame@(macroboard, player) depth =
    let valPlays      = validPlays macgame
        childStates   = [(makePlay play macgame, play) | play <- valPlays]
        listOfFutures = --probably should abstract these list comprehensions because there's literally one difference but need to actually fix the functions first lol
            if (depth == 0) then [(predictMightWin (state) (depth), play) | (state, play) <- childStates]
            else [(predictMightWin (state) (depth - 1), play) | (state, play) <- childStates]
    in bestPlayFor player listOfFutures
  where bestPlayFor player listOfFutures =
            if (player == X) then snd $ maximum listOfFutures
            else snd $ minimum listOfFutures






--IO functions
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

showGame :: Macrogame -> String
showGame macgame = 
    let
        [b0, b1, b2, b3, b4, b5, b6, b7, b8] = fst macgame
        curr = snd macgame
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


writeGame :: Macrogame -> FilePath -> IO ()
writeGame macgame flpath = 
    let str = showGame macgame
    in writeFile flpath str

loadGame :: FilePath -> IO Macrogame
loadGame flpath = do
    str <- readFile flpath 
    let g = readGame str
    --putStrLn $ show $ length $ validPlays g
    return $ g

putWinner :: Macrogame -> IO ()
putWinner macgame = do
    let play = bestPlay macgame
    putStrLn $ "(" ++ show (fst play) ++ "," ++ show (snd play) ++ ")"


--Unused functions

--winningMoves :: Macrogame -> [Play] --Checking for microboard tiles that result in a macroboard win (and tyingMoves checks for microboard tiles that result in a macroboard tie)
--winningMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just (Won (snd macgame))]

--tyingMoves :: Macrogame -> [Play]
--tyingMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just Tie]

        
--comfort line
--comfort line 2
