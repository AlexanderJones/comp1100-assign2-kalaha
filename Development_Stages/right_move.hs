module Opponents.Player_B.Player (
  select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

select_move :: Board -> Lookahead -> Pond_Ix
select_move board depth = head $ filter (legal_move board) [6,5,4,3,2,1]
