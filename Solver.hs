module Solver where
import Game

predictWin :: Macrogame -> Victory
predictWin macgame = undefined

-- taking in Macrogame to determine winning moves for current Player in (Macroboard, Player) tuple
-- "chooses the move with the best outcome for the current player"
--
-- priority: winning moves for current player
-- secondary: moves that block other play from winning on their next turn
winningMoves :: [Play] -> Macrogame -> [(Play, Player)]
-- need to fix left side of pipe??
winningMoves validPlays macgame = [(play, snd macgame) | play <- validPlays, checkMacrowin fst (makePlay play macgame) /= Ongoing]

bestMove :: Macrogame -> Play
bestMove = undefined
