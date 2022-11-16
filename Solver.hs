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

bestMove :: Macrogame -> Play
bestMove macgame = undefined

readGame :: String -> Game
readGame str = undefined

showGame :: Game -> String
showGame game = undefined

writeGame :: Game -> FilePath -> IO ()
writeGame game fipath = undefined

loadGame :: FilePath -> IO Game
loadGame fipath = undefined

putWinner :: Game -> IO ()
putWinner game = undefined
