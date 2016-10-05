module Opponents.Player_A.Player (
  select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

-- type declarations to make function definitions more readable
type Score = Int
type Alpha = Int
type Beta  = Int

select_move :: Board -> Lookahead -> Pond_Ix
select_move board depth
  -- I force my program to choose 1 then 3 to gain a free turn and prevent opponent
  | ((bank_A board) + (bank_B board) == 0) = 1
  | ((bank_A board) + (bank_B board) == 1) = 3
  | otherwise = snd $ maximum $ score_move_pairs (-9999) 9999 moves -- choose move with highest score
  where
    -- Initually I used list comprehension to get score-move pairs but replaced
    -- with recursion to experiment with updating alpha and beta after each move.
    score_move_pairs :: Alpha -> Beta -> [Pond_Ix] -> [(Score, Pond_Ix)]
    score_move_pairs alpha beta moves' = case moves' of
      []       -> []
      (m : ms) -> (get_score alpha beta m, m) : score_move_pairs alpha beta ms
      where
        get_score :: Alpha -> Beta -> Pond_Ix -> Score
        get_score alpha' beta' m = evaluate_move (turn board) depth (pick_n_distribute board m) alpha' beta'

    moves -- list of available moves with preference given to free turns
      | (no_of_pebbles board 6 == 1) = [6]
      | (no_of_pebbles board 5 == 2) = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 == 3) = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 == 4) = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 == 5) = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 == 6) = 1 : filter (legal_move board) [6,5,4,3,2]
      | otherwise = filter (legal_move board) [6,5,4,3,2,1]

-- evaluate_move is a mutually recursive implemenration of the minimax algorithm with alpha-beta pruning
evaluate_move :: Players -> Lookahead -> Board -> Alpha -> Beta -> Score
evaluate_move player depth board alpha beta
  -- evaluate the boards using heuristic function if gametree reaches leaves or game is over
  | (depth == 0) || (turn board == Finished) = evaluate_board
  | (turn board == player) && (depth > 0)    = alpha_beta_max alpha beta moves
  | (turn board /= player) && (depth > 0)    = alpha_beta_min alpha beta moves
  | otherwise = error "Error" -- only occurs if inputs are wrong
  where
-- I chose to put the alpha_beta functions in where clauses to reduce function parameters
    -- at a maximiser node (my player), we can only modify the alpha value
    alpha_beta_max :: Alpha -> Beta -> [Pond_Ix] -> Alpha
    alpha_beta_max current_alpha current_beta moves' = case moves' of
      -- base case: if no more boards, return the current_alpha value
      []                                           -> current_alpha
      (m : ms)
        -- pruning condition
        | new_alpha >= current_beta                -> new_alpha
        -- update current_alpha with new_alpha and move to next node
        | max new_alpha current_alpha == new_alpha -> alpha_beta_max new_alpha current_beta ms
        -- if above conditions aren't met, move to next child node until met
        | otherwise                                -> alpha_beta_max current_alpha current_beta ms
        where
          -- generate new alpha value by evaluating children (boards) of current maximiser node
          new_alpha = evaluate_move player (depth - 1) (pick_n_distribute board m) current_alpha current_beta

    -- at a minimiser node (op player), we can only modify the beta value
    alpha_beta_min :: Alpha -> Beta -> [Pond_Ix] -> Beta
    alpha_beta_min current_alpha current_beta moves' = case moves' of
      -- base case: if no more boards, return the current_beta value
      []                                        -> current_beta
      (m : ms)
        -- pruning condition
        | new_beta <= current_alpha             -> new_beta
        -- update current_beta with new_beta and move to next node
        | min new_beta current_beta == new_beta -> alpha_beta_min current_alpha new_beta ms
        -- if above conditions aren't met, move to next child node until met
        | otherwise                             -> alpha_beta_min current_alpha current_beta ms
        where
          -- generate new beta value by evaluating children (boards) of current minimiser node
          new_beta = evaluate_move player (depth - 1) (pick_n_distribute board m) current_alpha current_beta

    evaluate_board -- heuristic 6
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

    moves -- list of available moves with preference given to free turns
      | (no_of_pebbles board 6 == 1) = [6]
      | (no_of_pebbles board 5 == 2) = 5 : filter (legal_move board) [6,4,3,2,1]
      | (no_of_pebbles board 4 == 3) = 4 : filter (legal_move board) [6,5,3,2,1]
      | (no_of_pebbles board 3 == 4) = 3 : filter (legal_move board) [6,5,4,2,1]
      | (no_of_pebbles board 2 == 5) = 2 : filter (legal_move board) [6,5,4,3,1]
      | (no_of_pebbles board 1 == 6) = 1 : filter (legal_move board) [6,5,4,3,2]
      | otherwise = filter (legal_move board) [6,5,4,3,2,1]
