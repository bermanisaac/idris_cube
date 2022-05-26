module CubeSolver

import Data.Vect
import CubeRotations
import Algs
import Decomp
import PracticalGroup
import MoveAction

StateHold : Type -> Type
StateHold = State (Pair PCube String)

starter : (Pair PCube String)
starter = (solved, "")

doMove : (PCube -> PCube) -> String -> StateHold ()
doMove move descriptor = modify (\(cube, moves) => (move cube, moves ++ " " ++ descriptor))


checkerBoard : StateHold ()
checkerBoard = do
    doMove (R . R) "R2"
    doMove (L . L) "L2"
    doMove (U . U) "U2"
    doMove (D . D) "D2"
    doMove (F . F) "F2"
    doMove (B . B) "B2"
-- view with execState checkerBoard starter







-- does a 2-cycle of corners 
executeCornerSwap : Pair (Fin 8) (Fin 8) -> StateHold ()
executeCornerSwap = ?cornerswap

-- does a 3-cycle of edges
executeEdgeCycle : Vect 3 (Fin 12) -> StateHold ()
executeEdgeCycle =    

--does all the edge and corner cycles in a decomposition of a piece permutation
executeDecomp : Pair (List (Pair (Fin 8) (Fin 8))) (List (Vect 3 (Fin 12))) -> StateHold ()
executeDecomp (swaps, cycles) = do 
    foldlM (\() => executeCornerSwap) () swaps
    foldlM (\() => executeEdgeCycle) () cycles

--permutes the pieces correctly
permute : PCube -> StateHold ()
permute cube = ?perm

orient : PCube -> StateHold ()
orient cube = ?perm

-- produces a solution given a cube state
solve : PCube -> StateHold ()
solve cube = do
    permute cube
    orient cube

-- produces a solution given a (monadic) scramble action
-- (stretch goal)
--scramble : String -> StateHold ()

 





















