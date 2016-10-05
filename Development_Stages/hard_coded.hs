module Opponents.Player_B.Player (
  select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

select_move :: Board -> Lookahead -> Pond_Ix
select_move board depth
      | (no_of_pebbles board 6 == 1) = 6
      | (no_of_pebbles board 5 == 2) = 5
      | (no_of_pebbles board 4 == 3) = 4
      | (no_of_pebbles board 3 == 4) = 3
      | (no_of_pebbles board 2 == 5) = 2
      | (no_of_pebbles board 1 == 6) = 1

      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 5 == 1) = 5
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 4 == 2) = 4
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 3 == 3) = 3
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 2 == 4) = 2
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 1 == 5) = 1

      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 4 == 1) = 4
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 3 == 2) = 3
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 2 == 3) = 2
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 1 == 4) = 1

      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 3 == 1) = 3
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 2 == 2) = 2
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 1 == 3) = 1

      | (no_of_pebbles board 3 == 0) && (no_of_pebbles board 2 == 1) = 2
      | (no_of_pebbles board 3 == 0) && (no_of_pebbles board 1 == 2) = 1

      | (no_of_pebbles board 2 == 0) && (no_of_pebbles board 1 == 1) = 1

      | (no_of_pebbles board 6 == 13) = 6
      | (no_of_pebbles board 5 == 13) = 5
      | (no_of_pebbles board 4 == 13) = 4
      | (no_of_pebbles board 3 == 13) = 3
      | (no_of_pebbles board 2 == 13) = 2
      | (no_of_pebbles board 1 == 13) = 1

      | (no_of_pebbles board 6 `mod` 13 == 1) = 6
      | (no_of_pebbles board 5 `mod` 13 == 2) = 5
      | (no_of_pebbles board 4 `mod` 13 == 3) = 4
      | (no_of_pebbles board 3 `mod` 13 == 4) = 3
      | (no_of_pebbles board 2 `mod` 13 == 5) = 2
      | (no_of_pebbles board 1 `mod` 13 == 6) = 1

      | otherwise = head $ filter (legal_move board) [6,5,4,3,2,1]
