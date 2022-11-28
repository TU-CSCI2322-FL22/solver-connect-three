module Solver where
import Game
import Data.List.Split

predictWin :: Macrogame -> Victory
predictWin macgame =
    let play          = bestPlay macgame
        gameAfterPlay = makePlay play macgame
    in if (checkMacrowin (fst gameAfterPlay)) == Just (Won X) then Won X --fromJust wasn't working lol
       else if (checkMacrowin (fst gameAfterPlay)) == Just (Won O) then Won O
       else if (checkMacrowin (fst gameAfterPlay)) == Just Tie then Tie
       else predictWin gameAfterPlay
           




--    let valPlays = validPlays macgame
--    in if (winningMoves macgame) /= [] then Won (snd macgame)
--       else aux valPlays macgame
--                where aux [] game = if (snd game == X) then Won O else Won X
--                      aux (p:ps) game =
--                          let gameAfterPlay = makePlay p game
--                          in if (checkMacrowin gameAfterPlay == Just (Won (snd game))) then Won (snd game)
--                             else if (winningMoves gameAfterPlay) /= [] then aux ps
--                                  else undefined
                                          

--let winMoves = winningMoves macgame
--        --tieMoves = tyingMoves macgame
--    in if (winMoves /= []) then Won (snd macgame)
--       else aux (validPlays macgame) macgame
--                where aux [] game = 
--                      aux (m:ms) game =
--                          let gameAfterPlay = makePlay m macgame
--                          in 
--                              if (winningMoves gameAfterPlay) /= [] then aux ms game
--                              else aux (validPlays gameAfterPlay) gameAfterPlay

-- taking in Macrogame to determine winning moves for current Player in (Macroboard, Player) tuple
-- "chooses the move with the best outcome for the current player"
--
-- priority: winning moves for current player
-- secondary: moves that block other play from winning on their next turn
-- 
-- change type signature to exclude [Play] at some point? leave Macrogame -> ...
winningMoves :: Macrogame -> [Play] --Checking for microboard tiles that result in a macroboard win (and tyingMoves checks for microboard tiles that result in a macroboard tie)
winningMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just (Won (snd macgame))]


--winningOMoves :: Macrogame -> Play
--winningOMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Won O]

tyingMoves :: Macrogame -> [Play]
tyingMoves macgame = [ play | play <- (validPlays macgame), checkMacrowin (fst $ makePlay play macgame) == Just Tie]

-- tyingMoves
-- "                                                                                                              "-ish == Tie

-- These type signatures on Dr. Fogarty's website have "Game" instead of "Macrogame" - be cautious - we might have to write these for microgames too??

bestPlay :: Macrogame -> Play
bestPlay macgame =
    let winMoves = winningMoves macgame 
        valPlays = validPlays macgame
    in if (winMoves /= []) then intercolate winMoves
       else aux valPlays (head valPlays)
                where aux [] best = best
                      aux (p:ps) best =
                          let gameAfterPlay = makePlay p macgame
                          in if (winningMoves gameAfterPlay) /= [] then aux ps best
                             else if (tyingMoves gameAfterPlay) /= [] then p
                          else aux ps p
--X
--0,X,O,X,X,E,X,X,O,X
--1,X,X,E,O,O,X,X,E,X
--2,X,E,E,X,E,X,X,E,X
readGame :: String -> Macrogame
readGame str = 
    let
        [currentPlayer, b0, b1, b2, b3, b4, b5, b6, b7, b8] = lines str
        curr = if currentPlayer == "X" then X else O
        board0 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b0]
        board1 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b1]
        board2 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b2]
        board3 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b3]
        board4 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b4]
        board5 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b5]
        board6 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b6]
        board7 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b7]
        board8 = [if cell == "X" then Just X else if cell == "O" then Just O else Nothing | cell <- splitOn "," b8]
    in
        ([board0, board1, board2, board3, board4, board5, board6, board7, board8], curr)

showGame :: Macrogame -> String
showGame macgame = 
    let
        [b0, b1, b2, b3, b4, b5, b6, b7, b8] = fst macgame
        curr = snd macgame
        currentPlayer = if curr == X then "X" else "O"
        board0 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b0]
        board1 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b1]
        board2 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b2]
        board3 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b3]
        board4 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b4]
        board5 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b5]
        board6 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b6]
        board7 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b7]
        board8 = [if cell == Just X then "X" else if cell == Just O then "O" else "E" | cell <- b8]
    in
        unlines [currentPlayer, board0, board1, board2, board3, board4, board5, board6, board7, board8] 

writeGame :: Macrogame -> FilePath -> IO ()
writeGame macgame fipath = undefined

loadGame :: FilePath -> IO Macrogame
loadGame fipath = undefined

putWinner :: Macrogame -> IO ()
putWinner macgame = undefined
