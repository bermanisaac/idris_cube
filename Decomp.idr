module Decomp
import Data.Vect

decomp : (Pair (Vect 8 Nat) (Vect 12 Nat)) -> (Pair (List (Pair Nat Nat)) (List (Pair Nat Nat)))
decomp state = ?todo_decomp

parity : (Pair (Vect 8 Nat) (Vect 12 Nat)) -> Nat
{-parity state = (((+) (length (the (List (Pair Nat Nat)) (fst (the (Pair Nat Nat) d)))) 
                     (length (the (List (Pair Nat Nat)) (snd (the (Pair Nat Nat) d))))) `mod` (the Nat 2) where
                                             d = decomp state
-}
parity state = (length $ fst d + length $ snd d) `mod` 2 where
                                             d = decomp state

--Turns a product of 2-cycles into a product of 3-cycles
two_to_three : (List (Vect 2 Nat)) -> List (Vect 3 Nat)
two_to_three = ?todo_223

