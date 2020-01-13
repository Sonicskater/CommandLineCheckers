module Moves where

    import Lens.Micro.Platform
    
    import GameLogic
    import Data.List
    
    
    -------------ADD YOUR PROGRAM FOR MOVES-------------
    --------------------BELOW HERE----------------------

    moves:: GameState -> [Move]
    moves (GameState bs rs bk rk Red message)
        | (length j) == 0 = (concat [pmoves c (GameState bs rs bk rk Red message) | c <- rs ++ rk])
        | otherwise = j
        where 
            j = concat[pjumps c (GameState bs rs bk rk Red message) | c <- rs ++ rk]

    moves (GameState bs rs bk rk Black message)
        | (length j) == 0 = (concat [pmoves c (GameState bs rs bk rk Black message) | c <- bs ++ bk])
        | otherwise = j
        where 
            j = concat[pjumps c (GameState bs rs bk rk Black message) | c <- bs ++ bk]

    pmoves:: Coord -> GameState -> [Move]
    pmoves c s = [ m | m <- potentialSimpleMoves c, validMove m s  ]

    pjumps:: Coord -> GameState -> [Move]
    pjumps c s = concat[pjumpsChain p ( applyMoves p s) | p <- pot]
        where
        pot = [ [c,b] | b <- potNextJumps s c, validMove [c,b] s]

    pjumpsChain:: Move -> GameState -> [Move]
    pjumpsChain m s
        | (length succ) == 0 = [m]
        | otherwise = concat [pjumpsChain j (applyMoves j s) | j <- succ]
        where
            succ = [ m++[b] | b <- potNextJumps s (last m), validMove [last m, b] s ]

    applyMoves:: Move -> GameState -> GameState
    applyMoves ms s = applyMoveR msz s
        where msz = zip ms (tail ms)

    applyMoveR:: [(Coord, Coord)] -> GameState -> GameState
    applyMoveR (m:ms) s = applyMoveR ms (apply_Move m s)
    applyMoveR [] s = s

    apply_Move:: (Coord, Coord) -> GameState -> GameState
    apply_Move (a,b) s = case s^.status of
        Red -> if validMove [a,b] s then newSr
            else s
            where
            newSr
                | elem a (s^.redPieces) = kingMe b (set redPieces ((delete a (s^.redPieces)) ++ [b])  (kill (midPoint a b) s ))
                | elem a (s^.redKings) = set redKings ((delete a (s^.redKings)) ++ [b]) (kill (midPoint a b) s )
        Black -> if validMove [a,b] s then newSb
            else s
            where
            newSb
                | elem a (s^.blackPieces) = kingMe b (set blackPieces ((delete a (s^.blackPieces)) ++ [b]) (kill (midPoint a b) s ))
                | elem a (s^.blackKings) = set blackKings ((delete a (s^.blackKings)) ++ [b]) (kill (midPoint a b) s )



    kill:: Coord -> GameState -> GameState
    kill c s
        | elem c (s^.redPieces) = (set redPieces (delete c (s^.redPieces)) s )
        | elem c (s^.redKings) = (set redKings (delete c (s^.redKings)) s )
        | elem c (s^.blackPieces) = (set blackPieces (delete c (s^.blackPieces)) s )
        | elem c (s^.blackKings) = (set blackKings (delete c (s^.blackKings)) s )
        | otherwise = s

    endMove ::  GameState -> GameState
    endMove  s = case s^.status of
        Red -> setMessage $ set status Black s
        Black -> setMessage $ set status Red s
        _ -> initialGameState

            

    validMove:: Move -> GameState -> Bool
    validMove [a,b] s 

        | elem [a,b] (potentialSimpleMoves a) = case (s^.status) of
            Black -> validPath [a,b] s && validDirection (a,b) s
            Red -> validPath [a,b] s && validDirection (a,b) s
        | elem b (potNextJumps s a) = case (s^.status) of
            Black -> validPath [a,b] s && (elem (midPoint a b) ((s^.redPieces)++(s^.redKings))) && validDirection (a,b) s
            Red -> validPath [a,b] s && (elem (midPoint a b) ((s^.blackPieces)++(s^.blackKings))) && validDirection (a,b) s
        | otherwise = False

    validDirection:: (Coord, Coord) -> GameState -> Bool
    validDirection (a,b) s
        | elem a (s^.redPieces) = (diffY a b ) > 0 
        | elem a (s^.blackPieces) = (diffY a b) < 0
        | otherwise = True

    diffY:: Coord -> Coord -> Int
    diffY (a,b) (j,k) = b-k

    validPath:: Move -> GameState -> Bool
    validPath (a:bs)  s = case (s^.status) of
        Black -> (elem a ((s^.blackPieces)++(s^.blackKings))) && and(map fx bs )
        Red -> (elem a ((s^.redPieces)++(s^.redKings))) && and(map fx bs) 

        where 
            fx = (\c -> (not(elem c ((s^.redPieces)++(s^.redKings))) && not(elem c ((s^.blackPieces)++(s^.blackKings))))  )

    potentialSimpleMoves:: Coord -> [[Coord]]
    potentialSimpleMoves (a,b) =[ [(a,b), z] | z <- [(j,k) | j <- [a-1,a+1], k <- [b-1,b+1], (j)<=7, (j)>=0, (k)<=7, (k)>=0]]

    kingMe:: Coord -> GameState -> GameState
    kingMe a@(x,y) s
        | (elem a (s^.redPieces)) && 0==y = set redKings ((s^.redKings) ++ [a]) ( set redPieces ((delete a (s^.redPieces)) ) s)
        | (elem a (s^.blackPieces)) && 7==y = set blackKings ((s^.blackKings) ++ [a]) ( set blackPieces ((delete a (s^.blackPieces)) ) s )
        | otherwise = s

    potNextJumps::GameState -> Coord -> [Coord]
    potNextJumps s (a,b) = [ (j,k) | j <- [a-2,a+2], k <- [b-2,b+2], (j)<=7, (j)>=0, (k)<=7, (k)>=0]

    killCheck:: Coord -> Coord -> GameState -> Bool
    killCheck a b s
        | (s^.status) == Black = elem (midPoint a b) (s^.redPieces)
        | (s^.status) == Red = elem (midPoint a b) (s^.blackPieces) 

    midPoint:: Coord -> Coord -> Coord
    midPoint (a,b) (j,k) = (div (a+j) 2, div (b+k) 2 ) 