data Board = State State State State State State State State State
data State = X | O | U
type Victory = State
type Game = (Board, Victory)
