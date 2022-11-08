module Main where

data Player = X | O deriving (Show, Eq)
type Play = (Int, Int)
data Victory = Won Player | Tie | Ongoing deriving (Show, Eq)
type Microboard = [Maybe Player]
type Microgame = (Microboard, Victory)
type Macroboard = [Microgame]
type Macrogame = (Macroboard, Player)


-- Board positions are numbered as follows:
-- 0  |  1  |  2
-- ____________
-- 3  |  4  |  5
-- ____________
-- 6  |  7  |  8

-- Checking every move if the individual game has been won - if so, then place a move on the big board
checkWin :: Microboard -> Victory
checkWin [Just X, Just X, Just X, _, _, _, _, _, _] = Won X
checkWin [_, _, _, Just X, Just X, Just X, _, _, _] = Won X
checkWin [_, _, _, _, _, _, Just X, Just X, Just X] = Won X
checkWin [Just X, _, _, Just X, _, _, Just X, _, _] = Won X
checkWin [_, Just X, _, _, Just X, _, _, Just X, _] = Won X
checkWin [_, _, Just X, _, _, Just X, _, _, Just X] = Won X
checkWin [Just X, _, _, _, Just X, _, _, _, Just X] = Won X
checkWin [_, _, Just X, _, Just X, _, Just X, _, _] = Won X
checkWin [Just O, Just O, Just O, _, _, _, _, _, _] = Won O
checkWin [_, _, _, Just O, Just O, Just O, _, _, _] = Won O
checkWin [_, _, _, _, _, _, Just O, Just O, Just O] = Won O
checkWin [Just O, _, _, Just O, _, _, Just O, _, _] = Won O
checkWin [_, Just O, _, _, Just O, _, _, Just O, _] = Won O
checkWin [_, _, Just O, _, _, Just O, _, _, Just O] = Won O
checkWin [Just O, _, _, _, Just O, _, _, _, Just O] = Won O
checkWin [_, _, Just O, _, Just O, _, Just O, _, _] = Won O
checkWin board = if Nothing `elem` board then Ongoing else Tie

checkMacrowin :: Macroboard -> Victory
checkMacrowin [(_, Won X), (_, Won X), (_, Won X), _, _, _, _, _, _] = Won X
checkMacrowin [_, _, _, (_, Won X), (_, Won X), (_, Won X), _, _, _] = Won X
checkMacrowin [_, _, _, _, _, _, (_, Won X), (_, Won X), (_, Won X)] = Won X
checkMacrowin [(_, Won X), _, _, (_, Won X), _, _, (_, Won X), _, _] = Won X
checkMacrowin [_, (_, Won X), _, _, (_, Won X), _, _, (_, Won X), _] = Won X
checkMacrowin [_, _, (_, Won X), _, _, (_, Won X), _, _, (_, Won X)] = Won X
checkMacrowin [(_, Won X), _, _, _, (_, Won X), _, _, _, (_, Won X)] = Won X
checkMacrowin [_, _, (_, Won X), _, (_, Won X), _, (_, Won X), _, _] = Won X
checkMacrowin [(_, Won O), (_, Won O), (_, Won O), _, _, _, _, _, _] = Won O
checkMacrowin [_, _, _, (_, Won O), (_, Won O), (_, Won O), _, _, _] = Won O
checkMacrowin [_, _, _, _, _, _, (_, Won O), (_, Won O), (_, Won O)] = Won O
checkMacrowin [(_, Won O), _, _, (_, Won O), _, _, (_, Won O), _, _] = Won O
checkMacrowin [_, (_, Won O), _, _, (_, Won O), _, _, (_, Won O), _] = Won O
checkMacrowin [_, _, (_, Won O), _, _, (_, Won O), _, _, (_, Won O)] = Won O
checkMacrowin [(_, Won O), _, _, _, (_, Won O), _, _, _, (_, Won O)] = Won O
checkMacrowin [_, _, (_, Won O), _, (_, Won O), _, (_, Won O), _, _] = Won O
checkMacrowin board = if Ongoing `elem` map snd board then Ongoing else Tie

-- Making a play on a specific tile of a specific board

--We are going to assume play is legal because we will check if
--any user-inputted plays are legal in our actual implementation

playToTile :: Play -> Int
playToTile (0,0) = 0
playToTile (0,1) = 1
playToTile (0,2) = 2
playToTile (1,0) = 3
playToTile (1,1) = 4
playToTile (1,2) = 5
playToTile (2,0) = 6
playToTile (2,1) = 7
playToTile (2,2) = 8

makeMicroPlay :: Microgame -> Play -> Player -> Microgame
makeMicroPlay game play player =
    let numOfTile   = playToTile play
        board       = fst game
        vicState    = snd game
        newBoard    = [if ((board !! numOfTile) == tl) then Just player else tl | tl <- board]
        newVicState = checkWin newBoard
    in (newBoard, newVicState)

makeMacroPlay :: Macrogame -> Player ->  Macrogame
makeMacroPlay macrogame nextPlayer = 
    let board       = fst macrogame
    in  ([(b, checkWin b) | (b, v) <- board], nextPlayer)
    
    

-- Checking if a given tile has been played
checkPlay :: Play -> Microboard -> Bool
checkPlay play board = 
    let numOfTile        = playToTile play
        playWithTileHead = drop (numOfTile) board
        currentTile      = head playWithTileHead
    in currentTile /= Nothing
--testing to make sure my branch is working

-- TO-DO - I feel pretty confident about this
-- OBJECTIVE: Check if a move is legal
checkLegal :: Play -> Bool
checkLegal play = play `elem` [(x,y) | x <- [0..8], y <- [0..8]]

-- TO-DO - what to do with original Macrogame input?
-- OBJECTIVE: Return all legal plays (for a Macrogame?)
-- (ORIGINAL): legalPlays :: Macrogame -> [Play]
-- (ORIGINAL): legalPlays macrogame = undefined
legalPlays :: [Play]
legalPlays = [(x,y) | x <- [0..8], y <- [0..8]]

-- Show function
showBoard :: Microboard -> String
showBoard board = [
    if (i > 5 && i < 11) then head "_"
    else if (i > 17 && i < 23) then head "_"
    else if (i `mod` 6) == 0 then head "\n"
    else if (i+1 `mod` 2) == 0 then head "|"
    else if (i `mod` 2) == 0 && (i < 5) then head (show (board !! (i `div` 2)))
    else if (i `mod` 2) == 0 && (i <17) then head (show (board !! (i-6 `div` 2)))
    else if (i `mod` 2) == 0 && (i <29) then head (show (board !! ((i-12 `div` 2))))
    else ' '  
    | i <- [0..29]]


--showMacroboard :: Macroboard -> String
--showMacroboard = [showBoard (fst (board !! i)) | i <- [0..8]]
showMacroboard :: Macroboard -> String
showMacroboard board = concat [showBoard (fst (board !! i)) ++ "\n \n \n"  | i <- [0..8]]

main :: IO ()
main = do
    undefined
