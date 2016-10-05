module Opponents.Player_B.Player (
  select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

type Score = Int

select_move :: Board -> Lookahead -> Pond_Ix
select_move board depth = snd $ maximum $ score_move_pairs
  where
    score_move_pairs = [((evaluate_move (turn board) depth (pick_n_distribute board x)), x) | x <- filter (legal_move board) [6,5,4,3,2,1]]

evaluate_move :: Players -> Lookahead -> Board -> Score
evaluate_move player depth board
  | (turn board == Finished) || depth == 0 = evaluate_board
  | (turn board == player) && depth > 0 = maximum possible_scores
  | (turn board /= player) && depth > 0 = minimum possible_scores
  | otherwise = error "Error"
  where
    possible_scores = map (evaluate_move player (depth - 1)) possible_boards
    possible_boards = map (pick_n_distribute board) (filter (legal_move board) [6,5,4,3,2,1])

    evaluate_board
      | (player == Player_A) = (bank_A board) - (bank_B board)
      | otherwise            = (bank_B board) - (bank_A board)
