module Opponents.Player_B.Player (
  select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

type Score = Int
type Alpha = Int
type Beta  = Int

select_move :: Board -> Lookahead -> Pond_Ix
select_move board depth = snd $ maximum $ score_move_pairs (-9999) 9999 moves
  where
    score_move_pairs :: Alpha -> Beta -> [Pond_Ix] -> [(Score, Pond_Ix)]
    score_move_pairs alpha beta moves' = case moves' of
      []       -> []
      (m : ms) -> (get_score alpha beta m, m) : score_move_pairs alpha beta ms
      where
        get_score :: Alpha -> Beta -> Pond_Ix -> Score
        get_score alpha' beta' m = evaluate_move (turn board) depth (pick_n_distribute board m) alpha' beta'

    moves
      | (no_of_pebbles board 6 == 1)  = [6]
      | (no_of_pebbles board 5 == 2)  = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 == 3)  = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 == 4)  = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 == 5)  = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 == 6)  = 1 : filter (legal_move board) [6,5,4,3,2]
      | otherwise = filter (legal_move board) [6,5,4,3,2,1]

evaluate_move :: Players -> Lookahead -> Board -> Alpha -> Beta -> Score
evaluate_move player depth board alpha beta
  | (depth == 0) || (turn board == Finished) = evaluate_board
  | (2 * (36 - op_bank)) < alpha = alpha
  | (2 * (op_bank - 36)) > beta  = beta
  | (turn board == player) && (depth > 0) = alphaBetaMax alpha beta moves
  | (turn board /= player) && (depth > 0) = alphaBetaMin alpha beta moves
  | otherwise = 0
    where
      op_bank
        | (player == Player_A) = bank_B board
        | otherwise            = bank_A board

    alpha_beta_max :: Alpha -> Beta -> [Pond_Ix] -> Alpha
    alpha_beta_max current_alpha current_beta moves' = case moves' of
      []                                           -> current_alpha
      (m : ms)
        | new_alpha >= current_beta                -> new_alpha
        | max new_alpha current_alpha == new_alpha -> alpha_beta_max new_alpha current_beta ms
        | otherwise                                -> alpha_beta_max current_alpha current_beta ms
        where
          new_alpha = evaluate_move player (depth - 1) (pick_n_distribute board m) current_alpha current_beta

    alpha_beta_min :: Alpha -> Beta -> [Pond_Ix] -> Beta
    alpha_beta_min current_alpha current_beta moves' = case moves' of
      []                                        -> current_beta
      (m : ms)
        | new_beta <= current_alpha             -> new_beta
        | min new_beta current_beta == new_beta -> alpha_beta_min current_alpha new_beta ms
        | otherwise                             -> alpha_beta_min current_alpha current_beta ms
        where
          new_beta = evaluate_move player (depth - 1) (pick_n_distribute board m) current_alpha current_beta

    evaluate_board -- heuristic
      | (my_bank > 36)         = my_bank - op_bank + 36
      | (op_bank > 36)         = my_bank - op_bank - 36
      | (turn board == player) = my_bank - op_bank +  2
      | (turn board /= player) = my_bank - op_bank -  2
      | otherwise              = my_bank - op_bank
      where
        my_bank
          | (player == Player_A) = bank_A board
          | otherwise            = bank_B board
        op_bank
          | (player == Player_A) = bank_B board
          | otherwise            = bank_A board

    moves
      | (no_of_pebbles board 6 == 1)  = [6]
      | (no_of_pebbles board 5 == 2)  = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 == 3)  = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 == 4)  = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 == 5)  = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 == 6)  = 1 : filter (legal_move board) [6,5,4,3,2]
      | otherwise = filter (legal_move board) [6,5,4,3,2,1]
