module PracticalGroup

import Data.Vect
import PracticalGroup
import Control.Monad.State 

%access public export

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
F (cs, es) = (_F_Corner cs, _F_Edge es)

F' : PCube -> PCube
F' = F . F . F

{-corners:
1 wbr
2 wob
3 wgo
4 wrg
5 ygr
6 yog
7 ybo
8 yrb-}

_B_Corner : CornerState -> CornerState
_B_Corner [e0,
    (wob, c1),
    (wgo, c2),
    e3,
    e4,
    (yog, c5),
    (ybo, c6),
    e7] = [e0,
            (ybo, c6 + 2),
            (wob, c1 + 1),
            e3,
            e4,
            (wgo, c2 + 2),
            (yog, c5 + 1),
            e7]

{-edges:
1 wr
2 wg
3 wo
4 wb
5 yr
6 yg
7 yo
8 yb
9 rg
10 go
11 ob
12 br-}

_B_Edge : EdgeState -> EdgeState
_B_Edge [f0,
         f1,
         (wo, e2),
         f3,
         f4,
         f5,
         (yo, e6),
         f7,
         f8,
         (go, e9),
         (ob, e10),
         f11] =  [f0,
                    f1,
                    (ob, e10 + 1),
                    f3,
                    f4,
                    f5,
                    (go, e9),
                    f7,
                    f8,
                    (wo, e2),
                    (yo, e6 + 1),
                    f11]

B : PCube -> PCube
B (cs, es) = (_B_Corner cs, _B_Edge es)

B' : PCube -> PCube
B' = B . B . B

{-corners:
1 wbr
2 wob
3 wgo
4 wrg
5 ygr
6 yog
7 ybo
8 yrb-}

_R_Corner : CornerState -> CornerState
_R_Corner [(wbr, c0),
    (wob, c1),
    e2,
    e3,
    e4,
    e5,
    (ybo, c6),
    (yrb, c7)] = [(yrb, c7+2),
            (wbr, c0+1),
            e2,
            e3,
            e4,
            e5,
            (wob, c1 + 2),
            (ybo, c6 + 1)]

{-edges:
1 wr
2 wg
3 wo
4 wb
5 yr
6 yg
7 yo
8 yb
9 rg
10 go
11 ob
12 br-}

_R_Edge : EdgeState -> EdgeState
_R_Edge [f0,
         f1,
         f2,
         (wb, e3),
         f4,
         f5,
         f6,
         (yb, e7),
         f8,
         f9,
         (ob, e10),
         (br, e11)] =  [f0,
                    f1,
                    f2,
                    (br, e11 + 1),
                    f4,
                    f5,
                    f6,
                    (ob, e10),
                    f8,
                    f9,
                    (wb, e3),
                    (yb, e7 + 1)]

R : PCube -> PCube
R (cs, es) = (_R_Corner cs, _R_Edge es)

R' : PCube -> PCube
R' = R . R . R

{-corners:
1 wbr
2 wob
3 wgo
4 wrg
5 ygr
6 yog
7 ybo
8 yrb-}

_L_Corner : CornerState -> CornerState
_L_Corner [e0,
    e1,
    (wgo, c2),
    (wrg, c3),
    (ygr, c4),
    (yog, c5),
    e6,
    e7] = [e0,
            e1,
            (yog, c5 + 2),
            (wgo, c2 + 1),
            (wrg, c3 + 2),
            (ygr, c4 + 1),
            e6,
            e7]

{-edges:
1 wr
2 wg
3 wo
4 wb
5 yr
6 yg
7 yo
8 yb
9 rg
10 go
11 ob
12 br-}

_L_Edge : EdgeState -> EdgeState
_L_Edge [f0,
         (wg, e1),
         f2,
         f3,
         f4,
         (yg, e5),
         f6,
         f7,
         (rg, e8),
         (go, e9),
         f10,
         f11] =  [f0,
                    (go, e9 + 1),
                    f2,
                    f3,
                    f4,
                    (rg, e8),
                    f6,
                    f7,
                    (wg, e1),
                    (yg, e5 + 1),
                    f10,
                    f11]

L : PCube -> PCube
L (cs, es) = (_L_Corner cs, _L_Edge es)

L' : PCube -> PCube
L' = L . L . L

{-corners:
1 wbr
2 wob
3 wgo
4 wrg
5 ygr
6 yog
7 ybo
8 yrb-}

_U_Corner : CornerState -> CornerState
_U_Corner [e0,
    e1,
    e2,
    e3,
    e4,
    e5,
    e6,
    e7] = [e1,
            e2,
            e3,
            e0,
            e4,
            e5,
            e6,
            e7]

{-edges:
1 wr
2 wg
3 wo
4 wb
5 yr
6 yg
7 yo
8 yb
9 rg
10 go
11 ob
12 br-}

_U_Edge : EdgeState -> EdgeState
_U_Edge [f0,
         f1,
         f2,
         f3,
         f4,
         f5,
         f6,
         f7,
         f8,
         f9,
         f10,
         f11] =  [f3,
                    f0,
                    f1,
                    f2,
                    f4,
                    f5,
                    f6,
                    f7,
                    f8,
                    f9,
                    f10,
                    f11]

U : PCube -> PCube
U (cs, es) = (_U_Corner cs, _U_Edge es)

U' : PCube -> PCube
U' = U . U . U

{-corners:
1 wbr
2 wob
3 wgo
4 wrg
5 ygr
6 yog
7 ybo
8 yrb-}

_D_Corner : CornerState -> CornerState
_D_Corner [e0,
    e1,
    e2,
    e3,
    e4,
    e5,
    e6,
    e7] = [e0,
            e1,
            e2,
            e3,
            e5,
            e6,
            e7,
            e4]

{-edges:
1 wr
2 wg
3 wo
4 wb
5 yr
6 yg
7 yo
8 yb
9 rg
10 go
11 ob
12 br-}

_D_Edge : EdgeState -> EdgeState
_D_Edge [f0,
         f1,
         f2,
         f3,
         f4,
         f5,
         f6,
         f7,
         f8,
         f9,
         f10,
         f11] =  [f0,
                    f1,
                    f2,
                    f3,
                    f5,
                    f6,
                    f7,
                    f4,
                    f8,
                    f9,
                    f10,
                    f11]

D : PCube -> PCube
D (cs, es) = (_D_Corner cs, _D_Edge es)

D' : PCube -> PCube
D' = D . D . D
