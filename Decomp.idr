module Decomp
import Data.Vect



{- decompHelper takes a vector v of n Nats that represents a permutation f such that f(n) = m if m = v[n], and returns its decomposition into transpositions as a list of pairs of Nats. -}
{- (1 3 5) (6 4 2)
would be represented by

[3 6 5 2 1 4 7 8]

[3 6 5 2 1 4 7 8] 

so the first transposition is



 
and we can recursively compute the decomp of 


-}



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

decomp : (Pair (Vect 8 Nat) (Vect 12 Nat)) -> (Pair (List (Pair Nat Nat)) (List (Pair Nat Nat)))
decomp state = ?todo_decomp 

parity : (Pair (Vect 8 Nat) (Vect 12 Nat)) -> Nat
parity state = let d = decomp state in ((length $ fst d) + (length $ snd d)) `mod` 2 

--Turns a product of 2-cycles into a product of 3-cycles
two_to_three : (List (Vect 2 Nat)) -> List (Vect 3 Nat)
two_to_three = ?todo_223


