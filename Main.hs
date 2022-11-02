data Player = X | O | U deriving (Show, Eq)
type Play = (Int, Int)
data Victory = Won Player | Tie | Ongoing deriving Show
type Microboard = [Player]
-- type Microgame = ([Maybe Player], Victory)
type Microgame = (Microboard, Victory)
-- type Macroboard = [([Maybe Player], Victory)]
-- type Macroboard = [(Microboard, Victory)]
type Macroboard = [Microgame]
-- type Macrogame = ([Microgame], Victory)
type Macrogame = (Macroboard, Victory)


-- Board positions are numbered as follows:
-- 0  |  1  |  2
-- ____________
-- 3  |  4  |  5
-- ____________
-- 6  |  7  |  8

-- Checking every move if the individual game has been won - if so, then place a move on the big board
checkWin :: Microboard -> Victory
checkWin = undefined

checkMacrowin :: Macroboard -> Victory
checkMacrowin = undefined

-- Making a play on a specific tile of a specific board
makePlay :: Macrogame -> Play -> Maybe Macroboard -- if it's legal
makePlay = undefined

-- Checking if a given tile has been played
checkPlay :: Play -> Bool
checkPlay = undefined

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
showMacroboard :: Macroboard -> String
showMacroboard = undefined
