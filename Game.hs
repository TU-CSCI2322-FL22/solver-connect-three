module Game where
import Data.Maybe
import GHC.Stack
import Debug.Trace

data Player = X | O deriving (Show, Eq)
type Play = (Int, Int) --First int is tile number of the macroboard, second int is the tile number of the microboard)
data Victory = Won Player | Tie deriving (Show, Eq)
type Microboard = [Maybe Player]
-- type Microgame = Ongoing [Maybe Player] | Solved Victory deriving (Show, Eq) -- someday fix?
type Microgame = (Microboard, Maybe Victory)
type Macroboard = [Microgame]
type Macrogame = (Macroboard, Player)


-- Board positions are numbered as follows:
-- 0  |  1  |  2
-- ____________
-- 3  |  4  |  5
-- ____________
-- 6  |  7  |  8

-- Checking every move if the individual game has been won - if so, then place a move on the big board
checkWin :: Microboard -> Maybe Victory
checkWin [zero, one, two, three, four, five, six, seven, eight] =
    if (zero == one && one == two && zero /= Nothing) then Just $ Won (fromJust zero)
    else if (three == four && four == five && three /= Nothing) then Just $ Won (fromJust three)
    else if (six == seven && seven == eight && six /= Nothing) then Just $ Won (fromJust six)
    else if (zero == three && three == six && zero /= Nothing) then Just $ Won (fromJust zero)
    else if (one == four && four == seven && one /= Nothing) then Just $ Won (fromJust one)
    else if (two == five && five == eight && two /= Nothing) then Just $ Won (fromJust two)
    else if (zero == four && four == eight && zero /= Nothing) then Just $ Won (fromJust zero)
    else if (two == four && four == six && two /= Nothing) then Just $ Won (fromJust two)
    else if (zero /= Nothing && one /= Nothing && two /= Nothing && three /= Nothing && four /= Nothing && five /= Nothing && six /= Nothing && seven /= Nothing && eight /= Nothing) then Just Tie
    else Nothing
--checkWin tag other = errorWithStackTrace $ "CheckWin failure on : " ++ tag ++ " : " ++ show other

macroboardToMicroboard :: Macroboard -> Microboard
macroboardToMicroboard [] = []
macroboardToMicroboard ((microboard, victory):xs) = [if victory == Just (Won X) then Just X
                                                    else if victory == Just (Won O) then Just O
                                                    else Nothing] ++ macroboardToMicroboard xs

checkMacrowin :: Macroboard -> Maybe Victory
checkMacrowin macroboard = 
     let win = checkWin (macroboardToMicroboard macroboard)
         valPlays = validPlays (macroboard, X)
     in
         if (win /= Nothing) then win
         else if valPlays == [] then Just Tie else win



-- Making a play on a specific tile of a specific board
--We are going to assume play is legal because we will check if
--any user-inputted plays are legal in our actual implementation
makePlay :: Play -> Macrogame -> Macrogame
makePlay play macgame =
    let macboard         = fst macgame
        micTup           = aux1 macboard 0 --aux1 will determine correct microgame from the tile given by fst play
        micgame          = fst micTup
        micIdx           = snd micTup
        micboard         = fst micgame
        micboardWithPlay = aux2 micboard 0 --aux2 will make a play on the microgame on the tile given by snd play, with the player given by snd macgame
        newMicgame       = (micboardWithPlay, checkWin micboardWithPlay)
        nextPlayer       = if (snd macgame == X) then O else X
    in ((aux3 newMicgame 0 micIdx macboard), nextPlayer) --aux3 will add the microgame back to the macrogame
        where aux1 (m:ms) 9 = error "Invalid play input (> 8) in makePlay"
              aux1 (m:ms) currIdx =
                  if (currIdx == (fst play)) then (m, currIdx)
                  else aux1 ms (currIdx + 1)
              aux2 (t:ts) 9 = error "Invalid play input (> 8) in makePlay"
              aux2 (t:ts) currIdx =
                  if (currIdx == (snd play)) then (Just (snd macgame)):ts
                  else t:(aux2 ts (currIdx + 1))
              aux3 newMicgame currIdx idx (m:ms) =
                  if (idx == currIdx) then newMicgame:ms
                  else m:(aux3 newMicgame (currIdx + 1) idx ms)
    

-- Checking if a given tile has been played
checkPlay :: Play -> Macroboard -> Bool
checkPlay play board = 
    let numMacTile          = fst play
        numMicTile          = snd play
        macPlayWithTileHead = drop (numMacTile) board
        microboard          = fst $ head macPlayWithTileHead
        micPlayWithTileHead = drop (numMicTile) microboard
        currentTile         = head micPlayWithTileHead
    in (currentTile == Nothing && (checkWin microboard)  == Nothing)


checkMicroPlay :: Int -> Microboard -> Bool
checkMicroPlay tile board =
    let playWithTileHead = drop tile board
        currentTile = head playWithTileHead
    in currentTile == Nothing && checkWin board == Nothing


-- TO-DO - I feel pretty confident about this
-- OBJECTIVE: Check if a move is legal
checkLegal :: Play -> Bool
checkLegal play = play `elem` [(x,y) | x <- [0..8], y <- [0..8]]

-- TO-DO - what to do with original Macrogame input?
-- OBJECTIVE: Return all legal plays (for a Macrogame?)
-- (ORIGINAL): legalPlays :: Macrogame -> [Play]
-- (ORIGINAL): legalPlays macrogame = undefined
validPlays :: Macrogame -> [Play]
validPlays game = [(x,y) | x <- [0..8], y <- [0..8], checkPlay (x,y) (fst game)]

validMicroPlays :: Microgame -> [Int]
validMicroPlays micgame = [y | y <- [0..8], checkMicroPlay y (fst micgame)]

-- Show function

-- X| |X|O|X|O|X| |O
-- -----|-----|-----
-- O|X| |X| |O|X|O|X
-- -----|-----|-----
-- X|O|X|X| |X|O|X|O
-- -----|-----|-----
-- X| |X|O|X|O|X| |O
-- -----|-----|-----
-- O|X|O| |X|O|X|O|X
-- -----|-----|-----
-- X| |X|X|O|X|O| |O
-- -----|-----|-----
-- X|O|X|O| |O| |X|O
-- -----|-----|-----
-- O|X|O|X|X|O|X| |X
-- -----|-----|-----
-- X| |X|X|O| |O|X|O

showPlayer :: Maybe Player -> String
showPlayer Nothing = " "
showPlayer (Just X) = "X"
showPlayer (Just O) = "O"

showBoard :: Microboard -> (String, String, String)
showBoard [pos0, pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8] =
    let
        line1 = showPlayer pos0 ++ "|" ++ showPlayer pos1 ++ "|" ++ showPlayer pos2
        line2 = showPlayer pos3 ++ "|" ++ showPlayer pos4 ++ "|" ++ showPlayer pos5
        line3 = showPlayer pos6 ++ "|" ++ showPlayer pos7 ++ "|" ++ showPlayer pos8
    in
        (line1, line2, line3)
showBoard _ = error "Invalid board in showBoard"


--showMacroboard :: Macroboard -> String
--showMacroboard = [showBoard (fst (board !! i)) | i <- [0..8]]
showMacroboard :: Macroboard -> String

showMacroboard [(game1, _), (game2, _), (game3, _), (game4, _), (game5, _), (game6, _), (game7, _), (game8, _), (game9, _)] = 
    let
        (board1Line1, board1Line2, board1Line3) = showBoard (game1)
        (board2Line1, board2Line2, board2Line3) = showBoard (game2)
        (board3Line1, board3Line2, board3Line3) = showBoard (game3)
        (board4Line1, board4Line2, board4Line3) = showBoard (game4)
        (board5Line1, board5Line2, board5Line3) = showBoard (game5)
        (board6Line1, board6Line2, board6Line3) = showBoard (game6)
        (board7Line1, board7Line2, board7Line3) = showBoard (game7)
        (board8Line1, board8Line2, board8Line3) = showBoard (game8)
        (board9Line1, board9Line2, board9Line3) = showBoard (game9)
        line1 = board1Line1 ++ "||" ++ board2Line1 ++ "||" ++ board3Line1
        line2 = board1Line2 ++ "||" ++ board2Line2 ++ "||" ++ board3Line2
        line3 = board1Line3 ++ "||" ++ board2Line3 ++ "||" ++ board3Line3
        line4 = "-----||-----||-----"
        line5 = board4Line1 ++ "||" ++ board5Line1 ++ "||" ++ board6Line1
        line6 = board4Line2 ++ "||" ++ board5Line2 ++ "||" ++ board6Line2
        line7 = board4Line3 ++ "||" ++ board5Line3 ++ "||" ++ board6Line3
        line8 = "-----||-----||-----"
        line9 = board7Line1 ++ "||" ++ board8Line1 ++ "||" ++ board9Line1
        line10 = board7Line2 ++ "||" ++ board8Line2 ++ "||" ++ board9Line2
        line11 = board7Line3 ++ "||" ++ board8Line3 ++ "||" ++ board9Line3
    in 
        unlines [line1, line2, line3, line4, line5, line6, line7, line8, line9, line10, line11]
showMacroboard _ = error "Invalid macroboard"


