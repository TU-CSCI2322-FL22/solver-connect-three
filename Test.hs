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
-- ERROR: haskell doesn't like the Nothings in microboardTen
-- microboardTen = [X, O, X, O, playerVarX, playerVarO, Nothing, Nothing, Nothing]

testMacroboard   = [(microboardOne, Ongoing), (microboardTwo, Ongoing), (microboardThree, Ongoing), 
                   (microboardFour, Ongoing),(microboardFive, Ongoing), (microboardSix, Ongoing), 
                   (microboardSeven, Ongoing), (microboardEight, Ongoing), (microboardNine, Ongoing)]


--the last thing before String in the type definitions is the expected result of the function
{-testCheckWin :: Microboard -> Victory -> String
testCheckWin micboard vic = 
    if (checkWin micboard /= vic) then "checkWin error"
    else "checkWin test passed"
    
testCheckMacrowin :: Macroboard -> Victory -> String
testCheckMacrowin macboard vic =
    if (checkMacrowin macboard /= vic) then "checkMacrowin error"
    else "checkMacrowin test passed"

testMakeMicroPlay :: Microgame -> Play -> Player -> Microgame -> String
testMakeMicroPlay micgame play player newgame =
    if (makeMicroPlay micgame play player) /= newgame then "makeMicroPlay error"
    else "makeMicroPlay test passed"

testMakeMacroPlay :: Macrogame -> Player -> Macrogame -> String
testMakeMacroPlay macgame player newgame =
    if (makeMacroPlay macgame player) /= newgame then "makeMacroPlay error"
    else "makeMacroPlay test passed"

testCheckPlay :: Play -> Microboard -> Bool -> String
testCheckPlay play micboard expectedBool =
    if (checkPlay play micboard) /= expectedBool then "checkPlay error"
    else "checkPlay test passed"

testCheckLegal :: Play -> Bool -> String
testCheckLegal play expectedBool =
    if (checkLegal play) /= expectedBool then "checkLegal error"
    else "checkLegal test passed"

-}
