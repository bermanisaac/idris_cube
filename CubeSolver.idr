module CubeSolver

import Data.Vect
import PracticalGroup
import CubeRotations
import Algs
import Decomp
import Control.Monad.State

StateHold : Type -> Type
StateHold = State (Pair PCube String)

starter : (Pair PCube String)
starter = (solved, "")

doMove : (PCube -> PCube) -> String -> StateHold ()
doMove move descriptor = modify (\(cube, moves) => (move cube, moves ++ descriptor))

checkerBoard : StateHold ()
checkerBoard = do
    doMove (R . R) "R2"
    doMove (L . L) "L2"
    doMove (U . U) "U2"
    doMove (D . D) "D2"
    doMove (F . F) "F2"
    doMove (B . B) "B2"
-- view with execState checkerBoard starter
