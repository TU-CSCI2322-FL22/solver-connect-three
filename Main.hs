data State = X | O | U
data Play = Game Int Int State
type Victory = State
type Game = (Board, Victory)
data Board = Board State State State State State State State State State
type Macroboard = Macroboard Board Board Board Board Board Board Board Board Board

--makePlay :: Play -> Game
checkWin :: [Game] -> [Game] 

--Play -> Game Making a play on a specific tile of a specific board
makePlay :: Play -> Game
makePlay = undefined

--Play -> Bool Checking if a given tile has been played
checkPlay :: Play -> Bool
checkPlay = undefined

--Play -> Bool Check if a move is legal
checkLegal :: Play -> Bool
checkLegal = undefined

--Macroboard -> Macroboard Checking every move if the individual game has been won - if so, then place a move on the big board
checkWin :: Macroboard -> Macroboard
checkWin = undefined

--Macroboard -> Str Show function
showMacroboard :: Macroboard -> Str
showMacroboard = undefined
