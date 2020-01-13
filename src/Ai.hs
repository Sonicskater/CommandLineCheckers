module CheckersDevonHockley where
    import GameLogic
    import Moves
    import Main

    -- Defining this structure is not nesecarry, but because it is lazily evaluated we can 
    -- safely traverse the searchspace as though we were generating in in place, without actually doing so

    -- Basically we are letting haskell manage the construction of the tree as we need it, based on the definition given here and with expand
    -- instead of manually expanding it.
    data NodeData = NData GameState Move Int deriving (Show, Eq)

    instance Ord NodeData where
        compare (NData gs m i) (NData gs2 m2 i2)
            | i  < i2 =  LT
            | i > i2 = GT
            | otherwise = EQ

    data Tree = Node NodeData [Tree] deriving (Show,Eq)

    altern:: (GameState,Move) -> [(GameState,Move)]
    altern (s,_) = [( (apply_move m s),m) | m <- moves s]

    expand::Status -> Tree -> Tree
    expand st (Node (NData s m i) []) = Node (NData s m i) [expand st t | t<-makeEmptyChildren s st m]

    minmax:: Tree -> Int -> Bool-> NodeData
    minmax (Node t _) 0 _ = t
    minmax (Node t []) _ _ = t
    minmax (Node t ts) i True = maximum [ minmax a (i-1) False | a <- ts]
    minmax (Node t ts) i False = minimum [ minmax a (i-1) True | a <- ts]

    minmaxAB:: Tree -> Int -> Int -> Int -> Bool-> Int
    minmaxAB (Node t _) 0 a b _ = openNDataV t
    minmaxAB (Node t []) _ a b _ = openNDataV t
    minmaxAB (Node t ts) i a b True = forAlpha ts i a b (-1000)
    minmaxAB (Node t ts) i a b False = forBeta ts i a b 1000

    forAlpha::[Tree] -> Int -> Int -> Int -> Int -> Int
    forAlpha (t:ts) i a b v
        | a >= b = v
        | otherwise = forAlpha ts i alpha b val
        where 
            val = max v (minmaxAB t (i-1) a b False)
            alpha = max a val
    forAlpha [] _ _ _ v = v 

    forBeta::[Tree] -> Int -> Int -> Int -> Int -> Int
    forBeta (t:ts) i a b v
        | a >= b = v
        | otherwise = forBeta ts i a beta val
        where 
            val = min v (minmaxAB t (i-1) a b True)
            beta = min b val
    forBeta [] _ _ _ v = v    
    
    --pick the state with the best minmax value according
    min_max::Tree -> Int -> NodeData 
    min_max (Node t ts) i = maximum [ NData s h n | Node (NData s h j) z <- ts, let n = openNDataV(minmax  (Node (NData s h j) z) (i-1) False) ]
    --minmax is started on a minimizing move because this function contains the first ply.

        
    --pick the state with the best minmax value according
    min_max_AB::Tree -> Int -> NodeData 
    min_max_AB (Node t ts) i = maximum [ NData s h n | Node (NData s h j) z <- ts, let n = (minmaxAB  (Node (NData s h j) z) (i-1) (-10000) 10000 False) ]
    --minmax is started on a minimizing move because this function contains the first ply.

    -- normal min-max
    ai_move_old:: GameState -> Status -> Move
    ai_move_old gs s = openNData (min_max (expand s (Node (NData gs [] 0) [])) 4)

    -- ab pruning
    ai_move:: GameState -> Status -> Move
    ai_move gs s = openNData (min_max_AB (expand s (Node (NData gs [] 0) [])) 4)
    
    red_ai:: GameState -> Move
    red_ai gs = ai_move gs Red

    black_ai:: GameState -> Move
    black_ai gs = ai_move gs Black 

    openNData:: NodeData -> Move
    openNData (NData gs m i) = m

    openNDataV:: NodeData -> Int
    openNDataV (NData gs m i) = i
    -- creates childless children for recursive expansion
    makeEmptyChildren:: GameState -> Status -> Move -> [Tree]
    makeEmptyChildren s st m = [Node (NData a b (eval a st) ) [] | (a,b) <- altern (s,m) ]

    eval:: GameState -> Status -> Int
    eval gs Red = evalDumb gs
    eval gs Black = -1 *(evalDumb gs) 
    evalDumb:: GameState -> Int
    evalDumb s = length (_redPieces s)+ 2* length (_redKings s) - (length (_blackPieces s) + 2* length(_blackKings s))

    --tests the lazy evaluation by diving throught the rightmost children i layers deep.
    dive:: Int -> Tree -> [NodeData]
    dive 0 (Node (NData s m _)ts) = [ open t | t <- ts  ] 
    dive i (Node (NData s m _)ts) = dive (i-1) (last ts)

    diveAll:: Int -> Tree -> [NodeData]
    diveAll 0 (Node (NData s m _)ts) = [ open t | t <- ts  ] 
    diveAll i (Node (NData s m _)ts) = concat[ diveAll (i-1) t | t <- ts]

    -- converts cause im dumb
    open:: Tree -> NodeData
    open (Node s ts) = s