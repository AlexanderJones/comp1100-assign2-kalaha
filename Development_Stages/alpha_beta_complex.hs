module Opponents.Player_B.Player (
   select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

-- type declarations to function definitions more readable
type Score = Int
type Alpha = Int
type Beta  = Int

select_move :: Board -> Lookahead -> Pond_Ix
select_move board depth
  | ((bank_A board) + (bank_B board) == 0) = 1
  | ((bank_A board) + (bank_B board) == 1) = 3
  | otherwise = snd $ maximum $ score_move_pairs (-9999) 9999 moves
  where
    score_move_pairs :: Alpha -> Beta -> [Pond_Ix] -> [(Score, Pond_Ix)]
    score_move_pairs alpha beta moves' = case moves' of
      [] -> []
      (m : ms) -> (get_score alpha beta m, m) : score_move_pairs alpha beta ms
      where
        get_score :: Alpha -> Beta -> Pond_Ix -> Score
        get_score alpha' beta' m = evaluate_move (turn board) depth (pick_n_distribute board m) alpha' beta'

    moves
      | (no_of_pebbles board 6 ==  1) = [6]
      | (no_of_pebbles board 5 ==  2) = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 ==  3) = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 ==  4) = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 ==  5) = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 ==  6) = 1 : filter (legal_move board) [6,5,4,3,2]

      | (no_of_pebbles board 6 == 13) = 6 : filter (legal_move board) [5,4,3,2,1]
      | (no_of_pebbles board 5 == 13) = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 == 13) = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 == 13) = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 == 13) = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 == 13) = 1 : filter (legal_move board) [6,5,4,3,2]

      | (no_of_pebbles board 6 `mod` 13 == 1) = 6 : filter (legal_move board) [5,4,3,2,1]
      | (no_of_pebbles board 5 `mod` 13 == 2) = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 `mod` 13 == 3) = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 `mod` 13 == 4) = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 `mod` 13 == 5) = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 `mod` 13 == 6) = 1 : filter (legal_move board) [6,5,4,3,2]

      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 5 == 1) = 5 : filter (legal_move board) [4,3,2,1]
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 4 == 2) = 4 : filter (legal_move board) [5,3,2,1]
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 3 == 3) = 3 : filter (legal_move board) [5,4,2,1]
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 2 == 4) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 1 == 5) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 4 == 1) = 4 : filter (legal_move board) [5,3,2,1]
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 3 == 2) = 3 : filter (legal_move board) [5,4,2,1]
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 2 == 3) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 1 == 4) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 3 == 1) = 3 : filter (legal_move board) [5,4,2,1]
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 2 == 2) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 1 == 3) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 3 == 1) = 3 : filter (legal_move board) [5,4,2,1]
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 2 == 2) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 1 == 3) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 3 == 0) && (no_of_pebbles board 2 == 1) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 3 == 0) && (no_of_pebbles board 1 == 2) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 2 == 0) && (no_of_pebbles board 1 == 1) = 1 : filter (legal_move board) [5,4,3,2]

      | otherwise = filter (legal_move board) [6,5,4,3,2,1]

evaluate_move :: Players -> Lookahead -> Board -> Alpha -> Beta -> Score
evaluate_move player depth board alpha beta
  | (depth == 0) || (turn board == Finished) = evaluate_board
  | (turn board == player) && (depth > 0) = alpha_beta_max alpha beta moves
  | (turn board /= player) && (depth > 0) = alpha_beta_min alpha beta moves
  | otherwise = 0
  where
    alpha_beta_max :: Alpha -> Beta -> [Pond_Ix] -> Score
    alpha_beta_max alphaMax beta_max moves_max = case moves_max of
      [] -> alphaMax
      (m : ms)
        | new_alpha >= beta_max -> new_alpha
        | max new_alpha alphaMax == new_alpha -> alpha_beta_max new_alpha beta_max ms
        | otherwise -> alpha_beta_max alphaMax beta_max ms
        where
          new_alpha = evaluate_move player (depth - 1) (pick_n_distribute board m) alphaMax beta_max

    alpha_beta_min :: Alpha -> Beta -> [Pond_Ix] -> Score
    alpha_beta_min alpha_min beta_min moves_min = case moves_min of
      [] -> beta_min
      (m : ms)
        | new_beta <= alpha_min -> new_beta
        | min new_beta beta_min == new_beta -> alpha_beta_min alpha_min new_beta ms
        | otherwise -> alpha_beta_min alpha_min beta_min ms
        where
          new_beta = evaluate_move player (depth - 1) (pick_n_distribute board m) alpha_min beta_min

    evaluate_board
      | (my_bank > 36)         = my_bank - op_bank + 72
      | (op_bank > 36)         = my_bank - op_bank - 72
      | (turn board == player) = my_bank - op_bank +  2
      | (turn board /= player) = my_bank - op_bank -  2
      | otherwise              = my_side – op_side
      where
        my_bank
          | (player == Player_A) = bank_A board
          | otherwise            = bank_B board
        op_bank
          | (player == Player_A) = bank_B board
          | otherwise            = bank_A board
        my_ponds
          | (player == Player_A) = ponds_A board
          | otherwise            = ponds_B board
        op_ponds
          | (player == Player_A) = ponds_B board
          | otherwise            = ponds_A board

        my_side = ((evaluate_ponds my_ponds) + my_bank)
        op_side = ((evaluate_ponds op_ponds) + op_bank)
        evaluate_ponds :: Ponds -> Int
        evaluate_ponds ponds = case ponds of
        [_,_,_,_,_,1]  -> 10
        [_,_,_,_,2,_]  ->  7
        [_,_,_,3,_,_]  ->  5
        [_,_,4,_,_,_]  ->  3
        [_,5,_,_,_,_]  ->  3
        [6,_,_,_,_,_]  ->  3
        [_,_,_,_,_,13] ->  3
        [_,_,_,_,13,_] ->  3
        [_,_,_,13,_,_] ->  3
        [_,_,13,_,_,_] ->  3
        [_,13,_,_,_,_] ->  3
        [13,_,_,_,_,_] ->  3
        _              ->  0


    moves
      | (no_of_pebbles board 6 ==  1) = [6]
      | (no_of_pebbles board 5 ==  2) = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 ==  3) = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 ==  4) = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 ==  5) = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 ==  6) = 1 : filter (legal_move board) [6,5,4,3,2]

      | (no_of_pebbles board 6 == 13) = 6 : filter (legal_move board) [5,4,3,2,1]
      | (no_of_pebbles board 5 == 13) = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 == 13) = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 == 13) = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 == 13) = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 == 13) = 1 : filter (legal_move board) [6,5,4,3,2]

      | (no_of_pebbles board 6 `mod` 13 == 1) = 6 : filter (legal_move board) [5,4,3,2,1]
      | (no_of_pebbles board 5 `mod` 13 == 2) = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 `mod` 13 == 3) = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 `mod` 13 == 4) = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 `mod` 13 == 5) = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 `mod` 13 == 6) = 1 : filter (legal_move board) [6,5,4,3,2]

      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 5 == 1) = 5 : filter (legal_move board) [4,3,2,1]
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 4 == 2) = 4 : filter (legal_move board) [5,3,2,1]
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 3 == 3) = 3 : filter (legal_move board) [5,4,2,1]
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 2 == 4) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 6 == 0) && (no_of_pebbles board 1 == 5) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 4 == 1) = 4 : filter (legal_move board) [5,3,2,1]
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 3 == 2) = 3 : filter (legal_move board) [5,4,2,1]
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 2 == 3) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 5 == 0) && (no_of_pebbles board 1 == 4) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 3 == 1) = 3 : filter (legal_move board) [5,4,2,1]
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 2 == 2) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 1 == 3) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 3 == 1) = 3 : filter (legal_move board) [5,4,2,1]
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 2 == 2) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 4 == 0) && (no_of_pebbles board 1 == 3) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 3 == 0) && (no_of_pebbles board 2 == 1) = 2 : filter (legal_move board) [5,4,3,1]
      | (no_of_pebbles board 3 == 0) && (no_of_pebbles board 1 == 2) = 1 : filter (legal_move board) [5,4,3,2]

      | (no_of_pebbles board 2 == 0) && (no_of_pebbles board 1 == 1) = 1 : filter (legal_move board) [5,4,3,2]

      | otherwise = filter (legal_move board) [6,5,4,3,2,1]
