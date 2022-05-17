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

_F_Corner : CornerState -> CornerState
_F_Corner [(wbr, c0),
    e1,
    e2,
    (wrg, c3),
    (ygr, c4),
    e3,
    e4,
    (yrb, c7)] = [(wrg, c3 + 1),
                    e1,
                    e2,
                    (ygr, c4 + 2),
                    (yrb, c7 + 1),
                    e3,
                    e4,
                    (wbr, c0 + 2)]

_F_Edge : EdgeState -> EdgeState
_F_Edge [(wr, e0),
         f1,
         f2,
         f3,
         (yr, e4),
         f4,
         f5,
         f6,
         (rg, e8),
         f7,
         f8,
         (br, e11)] =  [(rg, e8 + 1),
                        f1,
                        f2,
                        f3,
                        (br, e11),
                        f4,
                        f5,
                        f6,
                        (yr, e4 + 1),
                        f7,
                        f8,
                        (wr, e0)]

F : PCube -> PCube
F (cs, es) = (_R_Corner cs, _R_Edge es)