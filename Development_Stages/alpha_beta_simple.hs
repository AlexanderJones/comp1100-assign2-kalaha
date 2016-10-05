module Opponents.Player_B.Player (
  select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

type Score = Int
type Alpha = Int
type Beta  = Int

select_move :: Board -> Lookahead -> Pond_Ix
select_move board depth
  | ((bank_A board) + (bank_B board) == 0) = 1
  | ((bank_A board) + (bank_B board) == 1) = 3
  | otherwise = snd $ maximum $ score_move_pairs (-9999) 9999 (filter (legal_move board) [6,5,4,3,2,1])
  where
    score_move_pairs :: Alpha -> Beta -> [Pond_Ix] -> [(Score, Pond_Ix)]
    score_move_pairs alpha beta moves' = case moves' of
      []       -> []
      (m : ms) -> (get_score alpha beta m, m) : score_move_pairs alpha beta ms
      where
        get_score :: Alpha -> Beta -> Pond_Ix -> Score
        get_score alpha' beta' m = evaluate_move (turn board) depth (pick_n_distribute board m) alpha' beta'

evaluate_move :: Players -> Lookahead -> Board -> Alpha -> Beta -> Score
evaluate_move player depth board alpha beta
  | (depth == 0) || (turn board == Finished) = evaluate_board
  | (turn board == player) && (depth > 0)    = alpha_beta_max alpha beta (filter (legal_move board) [6,5,4,3,2,1])
  | (turn board /= player) && (depth > 0)    = alpha_beta_min alpha beta (filter (legal_move board) [6,5,4,3,2,1])
  | otherwise = error "Error"
  where
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

    evaluate_board
      | (player == Player_A) = bank_A board - bank_B board
      | otherwise            = bank_B board - bank_A board
