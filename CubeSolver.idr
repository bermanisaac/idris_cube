module CubeSolver

import Data.Vect
import PracticalGroup
import CubeRotations
import Algs
import Decomp

StateHold : Type
StateHold = State (Pair PCube String)

solveCube : PCube -> StateHold
solveCube = do
    orientEdges
    orientCorners
    permuteCorners
    permuteEdges
