module Main where

    import Checkers
    import Moves
    import GameLogic
    main :: IO ()
    
    main = human apply_move initialGameState
    -- main = redAi (red_ai 10) apply_move initialGameState
    -- main = blackAi (black_ai 10) apply_move initialGameState
    --main = aiTest (red_ai 10)  (black_ai 10) apply_move initialGameState
    
    ---------------------------------------------------------
    --                       YOUR CODE GOES HERE!!!!
    ---------------------------------------------------------
    --   define apply_move and supporting functions here (or make a module)

    apply_move m s = 
        if length (moves s) == 0
            then GameState [] [] [] [] GameOver "Loss"
            else 
                if elem m (moves s) 
                then (applyMove [] (applyMoves m s )) 
                else s