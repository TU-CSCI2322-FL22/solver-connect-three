data Board = State State State State State State State State State
data State = X | O | U
data Play = Game Int Int State
type Victory = State
type Game = (Board, Victory)



--makePlay :: Play -> Game
checkWin :: [Game] -> [Game] 
