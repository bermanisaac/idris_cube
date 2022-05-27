module Decomp
import Data.Vect

CCHelper : (n : Nat) -> Fin n -> Fin n -> (Vect n (Fin n)) -> List (Fin n)
CCHelper n start cur vec =
    let next = (index cur vec) in
        (if (next == start) then [cur]
        else cur :: CCHelper n start next vec)

{- takes a cycle starting point and a permutation and computes the entire cycle -}
chewCycle : (n : Nat) -> Fin n -> (Vect n (Fin n)) -> List (Fin n)
chewCycle n start vec = (CCHelper n start start vec)

PTCHelper : (n : Nat) -> (Vect n (Fin n)) -> List (Fin n) -> List (List (Fin n))
PTCHelper n perm [] = []
PTCHelper n perm (first :: rest) =
    let cycle = (chewCycle n first perm) in
        cycle :: PTCHelper n perm (filter (not . ((flip elem) cycle)) (first :: rest))

{- takes a permutation and computes the corresponding list of disjoint cycles that compose to it -}
permToCycles : (n : Nat) -> (Vect n (Fin n)) -> List (List (Fin n))
permToCycles n perm = PTCHelper n perm (toList range)

cycleToSwaps : List (Fin n) -> List (Pair (Fin n) (Fin n))
cycleToSwaps [] = []
cycleToSwaps [singleton] = []
cycleToSwaps (head :: next :: rest) = ((head, next) :: (cycleToSwaps (next :: rest)))

permToSwaps : (n : Nat) -> (Vect n (Fin n)) -> List (Pair (Fin n) (Fin n))
permToSwaps n v = ((foldr (++) []) . (map cycleToSwaps) . (permToCycles n)) v

{- turns a 3-cycle in S12 into a product of 3-cycles of the form (12n), represented by the relevant n. -}

reduceThreeCycle : Vect 3 Integer -> List Integer
--one non-{1,2}
reduceThreeCycle [1, 2, c] = [c]
reduceThreeCycle [c, 1, 2] = [c]
reduceThreeCycle [2, c, 1] = [c]
reduceThreeCycle [2, 1, c] = [c, c]
reduceThreeCycle [c, 2, 1] = [c, c]
reduceThreeCycle [1, c, 2] = [c, c]
--two non-{1,2}s
reduceThreeCycle [1, b, c] = [b, b, c]
reduceThreeCycle [b, c, 1] = [b, b, c]
reduceThreeCycle [c, 1, b] = [b, b, c]
reduceThreeCycle [2, b, c] = [b, c, c]
reduceThreeCycle [b, c, 2] = [b, c, c]
reduceThreeCycle [c, 2, b] = [b, c, c]
--three non-{1,2}s
reduceThreeCycle [a,b,c] = [a, c, b, a, c]

{- turns a product of 2-cycles into a product of 3-cycles (if possible) -}
twoToThree : (List (Pair (Fin n) (Fin n))) -> List (Vect 3 (Fin n))
twoToThree [] = []
twoToThree [one] = []
twoToThree ((a, b) :: (c, d) :: rest) = [[a, b, c], [b, c, d]] ++ (twoToThree rest)


decomp : (Pair (Vect 8 (Fin 8)) (Vect 12 (Fin 12))) ->
    (Pair (List (Pair (Fin 8) (Fin 8)))  (List (Vect 3 (Fin 12))))
decomp (corners, edges) = (permToSwaps 8 corners, twoToThree (permToSwaps 12 edges))

{-
parity : (Pair (Vect 8 (Fin 8)) (Vect 12 (Fin 12))) -> Nat
parity state = let d = decomp state in ((length $ fst d) + (length $ snd d)) `mod` 2
-}






