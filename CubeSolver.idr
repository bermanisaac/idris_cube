module CubeSolver

import Data.Vect
import Data.Fin
import Control.Monad.State
import CubeRotations
import Algs
import Decomp
import PracticalGroup

PState : Type
PState = Pair PCube String

StateHold : Type -> Type
StateHold = State PState

starter : (Pair PCube String)
starter = (solved, "")

doMove : (PCube -> PCube) -> String -> StateHold ()
doMove move descriptor = modify (\(cube, moves) => (move cube, descriptor ++ moves))

doId : StateHold ()
doId = doMove (id) ""

checkerBoard : StateHold ()
checkerBoard = do
    doMove (R . R) "R2"
    doMove (L . L) "L2"
    doMove (U . U) "U2"
    doMove (D . D) "D2"
    doMove (F . F) "F2"
    doMove (B . B) "B2"
--view with execState checkerBoard starter

-- does a 2-cycle of corners
executeCornerSwap : Pair (Fin 8) (Fin 8) -> StateHold ()
executeCornerSwap = ?cornerswap

-- does a 3-cycle of edges
executeEdgeCycle : Vect 3 (Fin 12) -> StateHold ()
executeEdgeCycle cycle = map getEdgeCycle (reduceThreeCycle ((map finToInteger) cycle))

--does all the edge and corner cycles in a decomposition of a piece permutation
executeDecomp : Pair (List (Pair (Fin 8) (Fin 8))) (List (Vect 3 (Fin 12))) -> StateHold ()
executeDecomp (swaps, cycles) = do
    foldlM (\() => executeCornerSwap) () swaps
    foldlM (\() => executeEdgeCycle) () cycles

--permutes the pieces correctly
permute : PCube -> StateHold ()
permute cube = ?perm

-- Edge Orientation

orientEdgeFxns : Vect 11 (PCube -> PCube)
orientEdgeFxns = [oeStep1, oeStep2, oeStep3,
                  oeStep4, oeStep5, oeStep6,
                  oeStep7, oeStep8, oeStep9,
                  oeStep10, oeStep11]

orientEdgeStrings : Vect 11 String
orientEdgeStrings = [oeStep1s, oeStep2s, oeStep3s,
                     oeStep4s, oeStep5s, oeStep6s,
                     oeStep7s, oeStep8s, oeStep9s,
                     oeStep10s, oeStep11s]

orientEdgesN : (Fin 11) -> StateHold ()
orientEdgesN n = do
    ((corners, edges), label) <- get
    if (modTwo . snd $ (index (weaken n)) edges) == 0
            then doId
            else doMove (index n orientEdgeFxns) (index n orientEdgeStrings)

orientEdges : StateHold ()
orientEdges = sequence_ $ map orientEdgesN [0..10]

-- Corner Orientation

orientCornerFxns : Vect 21 (PCube -> PCube)
orientCornerFxns = [id, ocUFR1, ocUFR2,
                    id, ocUBR1, ocUBR2,
                    id, ocUBL1, ocUBL2,
                    id, ocUFL1, ocUFL2,
                    id, ocDFL1, ocDFL2,
                    id, ocDBL1, ocDBL2,
                    id, ocDBR1, ocDBR2]

orientCornerStrings : Vect 21 String
orientCornerStrings = ["", ocUFR1s, ocUFR2s,
                       "", ocUBR1s, ocUBR2s,
                       "", ocUBL1s, ocUBL2s,
                       "", ocUFL1s, ocUFL2s,
                       "", ocDFL1s, ocDFL2s,
                       "", ocDBL1s, ocDBL2s,
                       "", ocDBR1s, ocDBR2s]

theFin4_3 : Fin 4
theFin4_3 = 3

defineParity : (Fin 7) -> CornerState -> Fin 3
defineParity n corners =  restrict 2 (snd (index (weaken n) corners))

defineIdx : (Fin 7) -> CornerState -> (Fin 21)
defineIdx n corners = (n * theFin4_3 + (defineParity n corners))

-- let x = 1; y = 2 in x * y

orientCornersN : (Fin 7) -> StateHold ()
orientCornersN n = do
    ((corners, edges), label) <- get
    doMove (index (defineIdx n corners) orientCornerFxns)
           (index (defineIdx n corners) orientCornerStrings)
    -- should be (Fin (S 6) * Fin (S 3)) + Fin (S 2)
    -- which is the same type as Fin 19 + Fin (S 2)
    -- which is the same type as Fin 21

-- produces a solution given a cube state
solve : PCube -> StateHold ()
solve cube = ?idk

-- produces a solution given a (monadic) scramble action
-- (stretch goal)
--scramble : String -> StateHold ()























