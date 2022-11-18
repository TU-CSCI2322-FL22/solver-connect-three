module Solver where
import Game

predictWin :: Macrogame -> Victory
-- call winningMoves on macgame
predictWin macgame =
    undefined
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

bestMove :: Macrogame -> Play
bestMove macgame = undefined

readGame :: String -> Macrogame
readGame str = undefined

showGame :: Macrogame -> String
showGame macgame = undefined

writeGame :: Macrogame -> FilePath -> IO ()
writeGame macgame fipath = undefined

loadGame :: FilePath -> IO Macrogame
loadGame fipath = undefined

putWinner :: Macrogame -> IO ()
putWinner macgame = undefined
