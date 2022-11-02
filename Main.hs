data Player = X | O | U deriving (Eq, Show)
type Play = (Int, Int)
data Victory = Won Player | Tie | Ongoing deriving (Eq, Show)
type Microboard = [Player]
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
checkWin [ X,  X,  X, _, _, _, _, _, _] = Won X
checkWin [_, _, _,  X,  X,  X, _, _, _] = Won X
checkWin [_, _, _, _, _, _,  X,  X,  X] = Won X
checkWin [ X, _, _,  X, _, _,  X, _, _] = Won X
checkWin [_,  X, _, _,  X, _, _,  X, _] = Won X
checkWin [_, _,  X, _, _,  X, _, _,  X] = Won X
checkWin [ X, _, _, _,  X, _, _, _,  X] = Won X
checkWin [_, _,  X, _,  X, _,  X, _, _] = Won X
checkWin [ O,  O,  O, _, _, _, _, _, _] = Won O
checkWin [_, _, _,  O,  O,  O, _, _, _] = Won O
checkWin [_, _, _, _, _, _,  O,  O,  O] = Won O
checkWin [ O, _, _,  O, _, _,  O, _, _] = Won O
checkWin [_,  O, _, _,  O, _, _,  O, _] = Won O
checkWin [_, _,  O, _, _,  O, _, _,  O] = Won O
checkWin [ O, _, _, _,  O, _, _, _,  O] = Won O
checkWin [_, _,  O, _,  O, _,  O, _, _] = Won O
checkWin board = if U `elem` board then Ongoing else Tie

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
checkPlay :: Play -> Bool
checkPlay = undefined

-- Check if a move is legal
checkLegal :: Play -> Bool
checkLegal = undefined

legalPlays :: Macrogame -> [Play]
legalPlays = undefined


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
    else 'U'  
    | i <- [0..29]]

showMacroboard :: Macroboard -> String
showMacroboard = [showBoard (fst (board !! i)) | i <- [0..8]]
