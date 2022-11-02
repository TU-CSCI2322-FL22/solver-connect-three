data State = X | O | U
data Play = Game Int Int State
type Victory = State
type Game = (Board, Victory)
data Board = Board State State State State State State State State State
type Macroboard = Macroboard Board Board Board Board Board Board Board Board Board

-- Board positions are numbered as follows:
-- 0  |  1  |  2
-- ____________
-- 3  |  4  |  5
-- ____________
-- 6  |  7  |  8

checkWin :: Board -> State
checkWin = undefined

checkMacrowin :: Macroboard -> State
checkMacrowin = undefined

makePlay :: Play -> Game
makePlay = undefined

checkPlay :: Play -> Bool
checkPlay = undefined

checkLegal :: Play -> Bool
checkLegal = undefined

checkWin :: Macroboard -> Macroboard
checkWin = undefined

showMacroboard :: Macroboard -> Str
showMacroboard = undefined
