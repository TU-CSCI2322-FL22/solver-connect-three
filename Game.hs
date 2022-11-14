module Game where

data Player = X | O deriving (Show, Eq)
type Play = (Int, Int) --First int is tile number of the macroboard, second int is the tile number of the microboard)
data Victory = Won Player | Tie | Ongoing deriving (Show, Eq)
type Microboard = [Maybe Player]
-- type Microgame = Ongoing [Maybe Player] | Solved Victory deriving (Show, Eq) -- someday fix?
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

--playToTile :: Play -> Int
--playToTile (0,0) = 0
--playToTile (0,1) = 1
--playToTile (0,2) = 2
--playToTile (1,0) = 3
--playToTile (1,1) = 4
--playToTile (1,2) = 5
--playToTile (2,0) = 6
--playToTile (2,1) = 7
--playToTile (2,2) = 8

--makeMicroPlay :: Microgame -> Play -> Player -> Microgame
--makeMicroPlay game play player =
--    let numOfTile   = playToTile play
--        board       = fst game
--        vicState    = snd game
--        newBoard    = [if ((board !! numOfTile) == tl) then Just player else tl | tl <- board]
--        newVicState = checkWin newBoard
--    in (newBoard, newVicState)

--makeMacroPlay :: Macrogame -> Player ->  Macrogame
--makeMacroPlay macrogame nextPlayer = 
--    let board = fst macrogame
--    in  ([(b, checkWin b) | (b, v) <- board], nextPlayer)


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
        where aux1 (m:ms) 9       = error "Invalid play input (> 8) in makePlay"
              aux1 (m:ms) currIdx =
                  if (currIdx == (fst play)) then (m, currIdx)
                  else aux1 ms (currIdx + 1)
              aux2 (t:ts) 9       = error "Invalid play input (> 8) in makePlay"
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
    in currentTile /= Nothing


-- TO-DO - I feel pretty confident about this
-- OBJECTIVE: Check if a move is legal
checkLegal :: Play -> Bool
checkLegal play = play `elem` [(x,y) | x <- [0..8], y <- [0..8]]

-- TO-DO - what to do with original Macrogame input?
-- OBJECTIVE: Return all legal plays (for a Macrogame?)
-- (ORIGINAL): legalPlays :: Macrogame -> [Play]
-- (ORIGINAL): legalPlays macrogame = undefined
validPlays :: [Play] -> Macrogame -> [Play]
validPlays play game = [(x,y) | (x, y) <- play, checkPlay (x,y) (fst game)]

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
showMacroboard _ = error "Invalid Macroboard"


