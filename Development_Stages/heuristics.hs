evaluate_board -- heuristic 1
  | (player == Player_A) = bank_A board - bank_B board
  | otherwise            = bank_B board - bank_A board

evaluate_board -- heuristic 2
  | (player == Player_A) = sum (ponds_A board) + bank_A board - bank_B board
  | otherwise            = sum (ponds_B board) + bank_B board - bank_A board

evaluate_board -- heuristic 3
  | (my_bank > 36)         = my_bank - op_bank + 36
  | (op_bank > 36)         = my_bank - op_bank - 36
  | otherwise              = my_bank - op_bank
  where
    my_bank
      | (player == Player_A) = bank_A board
      | otherwise            = bank_B board
    op_bank
      | (player == Player_A) = bank_B board
      | otherwise            = bank_A board

evaluate_board -- heuristic 4
  | (my_bank > 36)         = my_bank - op_bank + 36
  | (op_bank > 36)         = my_bank - op_bank - 36
  | (turn board == player) = my_bank - op_bank +  2
  | otherwise              = my_bank - op_bank
  where
    my_bank
      | (player == Player_A) = bank_A board
      | otherwise            = bank_B board
    op_bank
      | (player == Player_A) = bank_B board
      | otherwise            = bank_A board


evaluate_board -- heuristic 5
  | (my_bank > 36)         = my_bank - op_bank + 72
  | (op_bank > 36)         = my_bank - op_bank - 72
  | (turn board == player) = my_bank - op_bank +  2
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
