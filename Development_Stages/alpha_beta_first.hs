module Opponents.Player_B.Player (
   select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

type Score = Int
type Alpha = Int
type Beta = Int

select_move :: Board -> Lookahead -> Pond_Ix
select_move board depth = snd (maximum score_move_pairs)
    where
      score_move_pairs = [((evaluate_move (turn board) depth (pick_n_distribute board x) (-9999) 9999), x) | x <- possible_moves board]

evaluate_move :: Players -> Lookahead -> Board -> Alpha -> Beta -> Score
evaluate_move player depth board alpha beta
  | (depth == 0) || (turn board == Finished) = evaluate_board board player
  | (turn board == player) && (depth > 0) = alphaBetaMax player depth board (possible_moves board) alpha beta
  | (turn board /= player) && (depth > 0) = alphaBetaMin player depth board (possible_moves board) alpha beta
  | otherwise = 0
  where

alphaBetaMax :: Players -> Lookahead -> Board -> [Pond_Ix] -> Alpha -> Beta -> Score
alphaBetaMax playerMax depthMax boardMax movesMax alphaMax betaMax = case movesMax of
  [] -> alphaMax
  (m : ms)
    | alphaScore >= betaMax -> alphaScore
    | max alphaScore alphaMax == alphaScore -> alphaBetaMax playerMax depthMax boardMax ms alphaScore betaMax
    | otherwise -> alphaBetaMax playerMax depthMax boardMax ms alphaMax betaMax
    where
      alphaScore = evaluate_move playerMax (depthMax - 1) (pick_n_distribute boardMax m) alphaMax betaMax

alphaBetaMin :: Players -> Lookahead -> Board -> [Pond_Ix] -> Alpha -> Beta -> Score
alphaBetaMin playerMin depthMin boardMin movesMin alphaMin betaMin = case movesMin of
  [] -> betaMin
  (m : ms)
    | betaScore <= alphaMin -> betaScore
    | min betaScore betaMin == betaScore -> alphaBetaMin playerMin depthMin boardMin ms alphaMin betaScore
    | otherwise -> alphaBetaMin playerMin depthMin boardMin ms alphaMin betaMin
    where
      betaScore = evaluate_move playerMin (depthMin - 1) (pick_n_distribute boardMin m) alphaMin betaMin

evaluate_board :: Board -> Players -> Score
evaluate_board board player
  | (player == Player_A) = (bank_A board) - (bank_B board)
  | otherwise            = (bank_B board) - (bank_A board)

possible_moves :: Board -> [Pond_Ix]
possible_moves board
  | (legal_move board 6) && (no_of_pebbles board 6 == 1) = 6 : except_move board 6
  | (legal_move board 5) && (no_of_pebbles board 5 == 2) = 5 : except_move board 5
  | (legal_move board 4) && (no_of_pebbles board 4 == 3) = 4 : except_move board 4
  | (legal_move board 3) && (no_of_pebbles board 3 == 4) = 3 : except_move board 3
  | (legal_move board 2) && (no_of_pebbles board 2 == 5) = 2 : except_move board 2
  | (legal_move board 1) && (no_of_pebbles board 1 == 6) = 1 : except_move board 1
  | otherwise = filter (legal_move board) [6,5..1]

except_move :: Board -> Pond_Ix -> [Pond_Ix]
except_move board m = [x | x <- [6,5..1], legal_move board x && x /= m]
