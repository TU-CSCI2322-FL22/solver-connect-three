module Solver where
import Game

predictWin :: Macrogame -> Victory
-- call winningMoves on macgame
predictWin macgame = undefined

-- taking in Macrogame to determine winning moves for current Player in (Macroboard, Player) tuple
-- "chooses the move with the best outcome for the current player"
--
-- priority: winning moves for current player
-- secondary: moves that block other play from winning on their next turn
-- 
-- change type signature to exclude [Play] at some point? leave Macrogame -> ...
winningMoves :: [Play] -> Macrogame -> [(Play, Player)]
-- need to fix left side of pipe??
--
-- should we split winningMoves into winningMoves & tyingMoves??
-- winningMoves                                                                                                          == Won Player
winningMoves validPlays macgame = [(play, snd macgame) | play <- validPlays, checkMacrowin (fst $ makePlay play macgame) /= Ongoing]

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
