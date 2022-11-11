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
        playWithTileHead = drop (numOfTile) board
        currentTile      = head playWithTileHead
    in currentTile /= Nothing
    

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
showBoard board =
    let
        line1 = showPlayer (board !! 0) ++ "|" ++ showPlayer (board !! 1) ++ "|" ++ showPlayer (board !! 2)
        line2 = showPlayer (board !! 3) ++ "|" ++ showPlayer (board !! 4) ++ "|" ++ showPlayer (board !! 5)
        line3 = showPlayer (board !! 6) ++ "|" ++ showPlayer (board !! 7) ++ "|" ++ showPlayer (board !! 8)
    in
        (line1, line2, line3)

showMacroboard :: Macroboard -> String
showMacroboard board = 
    let
        (board1Line1, board1Line2, board1Line3) = showBoard (fst (board !! 0))
        (board2Line1, board2Line2, board2Line3) = showBoard (fst (board !! 1))
        (board3Line1, board3Line2, board3Line3) = showBoard (fst (board !! 2))
        (board4Line1, board4Line2, board4Line3) = showBoard (fst (board !! 3))
        (board5Line1, board5Line2, board5Line3) = showBoard (fst (board !! 4))
        (board6Line1, board6Line2, board6Line3) = showBoard (fst (board !! 5))
        (board7Line1, board7Line2, board7Line3) = showBoard (fst (board !! 6))
        (board8Line1, board8Line2, board8Line3) = showBoard (fst (board !! 7))
        (board9Line1, board9Line2, board9Line3) = showBoard (fst (board !! 8))
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