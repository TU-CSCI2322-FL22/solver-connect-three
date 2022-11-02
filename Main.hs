data State = X | O | U
data Play = Game Int Int State
type Victory = State
type Game = (Board, Victory)
data Board = Board State State State State State State State State State
type Macroboard = Macroboard Board Board Board Board Board Board Board Board Board

--makePlay :: Play -> Game
checkWin :: Board -> Bool
checkWin = undefined

checkMacrowin :: Macroboard -> Bool
checkMacrowin = undefined

-- Making a play on a specific tile of a specific board
makePlay :: Play -> Game
makePlay = undefined

-- Checking if a given tile has been played
checkPlay :: Play -> Bool
checkPlay = undefined

-- Check if a move is legal
checkLegal :: Play -> Bool
checkLegal = undefined

-- Checking every move if the individual game has been won - if so, then place a move on the big board
checkWin :: Macroboard -> Macroboard
checkWin = undefined

-- Show function
showMacroboard :: Macroboard -> Str
showMacroboard = undefined
