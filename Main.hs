data Player = X | O deriving Show
type Play = (Int, Int)
data Victory = Won Player | Tie | Ongoing deriving Show
type Microboard = [Maybe Player]
type Microgame = (Microboard, Victory)
type Macroboard = [Microgame]
type Macrogame = (Macroboard, Victory)


-- Board positions are numbered as follows:
-- 0  |  1  |  2
-- ____________
-- 3  |  4  |  5
-- ____________
-- 6  |  7  |  8

checkWin :: Microboard -> Victory
checkWin = undefined

checkMacrowin :: Macroboard -> Victory
checkMacrowin = undefined

-- Making a play on a specific tile of a specific board
makePlay :: Macrogame -> Play -> Maybe Macroboard -- if it's legal
makePlay = undefined

-- Checking if a given tile has been played
checkPlay :: Play -> Microboard -> Bool
checkPlay play board = 
    let numOfTile = aux play
                        where aux (0,0) = 0
                              aux (0,1) = 1
                              aux (0,2) = 2
                              aux (1,0) = 3
                              aux (1,1) = 4
                              aux (1,2) = 5
                              aux (2,0) = 6
                              aux (2,1) = 7
                              aux (2,2) = 8
        playWithTileHead = drop (numOfTile) play
        currentTile      = head playWithTileHead
    in currentTile == Nothing --This line with change if we change the implementation of Microboard
    

-- Check if a move is legal
checkLegal :: Play -> Bool
checkLegal = undefined

legalPlays :: Macrogame -> [Play]
legalPlays = undefined

-- Checking every move if the individual game has been won - if so, then place a move on the big board
checkWin :: Macroboard -> Macroboard
checkWin = undefined

-- Show function
showMacroboard :: Macroboard -> String
showMacroboard = undefined
