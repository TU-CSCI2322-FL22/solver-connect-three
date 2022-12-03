module TestCases where
import Game
import Solver

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
microboardTen    = take 3 (repeat (Just X)) ++ take 6 (repeat Nothing)
microboardEleven = take 3 (repeat (Just X)) ++ take 6 (repeat Nothing)
microboardTwelve = take 2 (repeat (Just X)) ++ take 5 (repeat Nothing) ++ take 2 (repeat (Just X))


micNearVic1 = ([Just X, Nothing, Just O, Nothing, Just O, Just X, Just O, Nothing, Just X], Just (Won O))
micNearVic2 = ([Just X, Just O, Just X, Just O, Just X, Just X, Just O, Just X, Just O], Just Tie)
micNearVic3 = ([Just X, Just X, Just X, Nothing, Just O, Just O, Just O, Nothing, Just X], Just (Won X))
micNearVic4 = ([Just X, Just X, Just X, Just X, Just O, Just O, Just O, Just O, Just X], Just (Won X))
micNearVic5 = ([Just X, Just X, Just O, Nothing, Nothing, Nothing, Just O, Just X, Just O], Nothing)
micNearVic6 = ([Nothing, Just X, Nothing, Just O, Just O, Just O, Just X, Nothing, Just X], Just (Won O))
micNearVic7 = ([Just X, Just O, Just O, Nothing, Nothing, Just O, Just X, Nothing, Just X], Nothing)
micNearVic8 = ([Just X, Just O, Just X, Just O, Just X, Just O, Just X, Just X, Just O], Just (Won X))
micNearVic9 = ([Just O, Just O, Just X, Just X, Just X, Just O, Just O, Just X, Just X], Just Tie)



-- Other/Optional Test Boards
-- ERROR: haskell doesn't like the Nothings in microboardTen
-- microboardTen = [X, O, X, O, playerVarX, playerVarO, Nothing, Nothing, Nothing]

testMacroboard   = [micNearVic5, (microboardTwo, Nothing), (microboardThree, Nothing), 
                   (microboardFour, Nothing),(microboardFive, Nothing), (microboardSix, Nothing), 
                   (microboardSeven, Nothing), (microboardEight, Nothing), micNearVic1]

testMacroboard2  = [(microboardTen, Just (Won X)), (microboardEleven, Just (Won X)), (microboardTwelve, Nothing),
                   (microboardFour, Nothing),(microboardFive, Nothing), (microboardSix, Nothing),
                   (microboardSeven, Nothing), (microboardEight, Nothing), (microboardNine, Nothing)]

testMacroboard3  = [(microboardTen, Just (Won X)), (microboardTwelve, Nothing), (microboardTwelve, Nothing),
                   (microboardFour, Nothing),(microboardFive, Nothing), (microboardSix, Nothing),
                   (microboardSeven, Nothing), (microboardEight, Nothing), (microboardNine, Nothing)]

macboardNearVic  = [micNearVic1, micNearVic2, micNearVic3, micNearVic4, micNearVic5, micNearVic6, micNearVic7, micNearVic8, micNearVic9]

macgameNearVic   = (macboardNearVic, X)


testMacrogame = (testMacroboard, X)

testMacrogame2 = (testMacroboard2, X)

testMacrogame3 = (testMacroboard3, X)


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
