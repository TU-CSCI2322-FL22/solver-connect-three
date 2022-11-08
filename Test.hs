module TestCases where
import Main

--Testing data constructors
playerVarX = X
playerVarO = O
victoryVarX = Won playerVarX
victoryVarO = Won playerVarO

--Test Boards
microboardOne    = take 9 (repeat Nothing)
microboardTwo    = take 9 (repeat Nothing)
microboardThree  = take 9 (repeat Nothing)
microboardFour   = take 9 (repeat Nothing)
microboardFive   = take 9 (repeat Nothing)
microboardSix    = take 9 (repeat Nothing)
microboardSeven  = take 9 (repeat Nothing)
microboardEight  = take 9 (repeat Nothing)
microboardNine   = take 9 (repeat Nothing)

-- Other/Optional Test Boards
microboardTen = [X, O, X, O, playerVarX, playerVarO, Nothing, Nothing, Nothing]

testMacroboard   = [(microboardOne, Ongoing), (microboardTwo, Ongoing), (microboardThree, Ongoing), 
                   (microboardFour, Ongoing),(microboardFive, Ongoing), (microboardSix, Ongoing), 
                   (microboardSeven, Ongoing), (microboardEight, Ongoing), (microboardNine, Ongoing)]

