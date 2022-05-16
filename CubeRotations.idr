module PracticalGroup

import Group
import Data.Vect
import PracticalGroup

solvedCorners : CornerState
solvedCorners = [(WBR, 0),
    (WOB, 0),
    (WGO, 0),
    (WRG, 0),
    (YGR, 0),
    (YOG, 0),
    (YBO, 0),
    (YRB, 0)]

solvedEdges : EdgeState
solvedEdges = [(WR, 0),
    (WG, 0),
    (WO, 0),
    (WB, 0),
    (YR, 0),
    (YG, 0),
    (YO, 0),
    (YB, 0),
    (RG, 0),
    (GO, 0),
    (OB, 0),
    (BR, 0)]

solved : PCube
solved = (solvedCorners, solvedEdges)

R : PCube -> PCube
R ([(wbr, c0),
    (wob, c1),
    (wgo, c2),
    (wrg, c3),
    (ygr, c4),
    (yog, c5),
    (ybo, c6),
    (yrb, c7)],
   [(wr, e0),
    (wg, e1),
    (wo, e2),
    (wb, e3),
    (yr, e4),
    (yg, e5),
    (yo, e6),
    (yb, e7),
    (rg, e8),
    (go, e9),
    (ob, e10),
    (br, e11)]) = ([(wrg, c3 + 2),
                    (wob, c1),
                    (wgo, c2),
                    (ygr, c4 + 1),
                    (yrb, c7 + 2),
                    (yog, c5),
                    (ybo, c6),
                    (wbr, c0 + 1)],
                    [(rg, e8 + 1),
                     (wg, e1),
                     (wo, e2),
                     (wb, e3),
                     (br, e11),
                     (yg, e5),
                     (yo, e6),
                     (yb, e7),
                     (yr, e4 + 1),
                     (go, e9),
                     (ob, e10),
                     (wr, e0)])
