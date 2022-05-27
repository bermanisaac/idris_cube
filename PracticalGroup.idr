module PracticalGroup

--import Group
import Data.Vect
import Decomp

%access public export


data EdgeColors = WR |
            WG |
            WO |
            WB |
            YR |
            YG |
            YO |
            YB |
            RG |
            GO |
            OB |
            BR

implementation Eq EdgeColors where
    WR == WR = True
    WG == WG = True
    WO == WO = True
    WB == WB = True
    YR == YR = True
    YG == YG = True
    YO == YO = True
    YB == YB = True
    RG == RG = True
    GO == GO = True
    OB == OB = True
    BR == BR = True
    _  == _  = False


edgeToNum : EdgeColors -> Nat
edgeToNum WR = 0
edgeToNum WG = 1
edgeToNum WO = 2
edgeToNum WB = 3
edgeToNum YR = 4
edgeToNum YG = 5
edgeToNum YO = 6
edgeToNum YB = 7
edgeToNum RG = 8
edgeToNum GO = 9
edgeToNum OB = 10
edgeToNum BR = 11


Edge : Type
Edge = Pair EdgeColors Nat


EdgeState : Type
EdgeState = Vect 12 Edge


data CornerColors = WBR | WOB | WGO | WRG | YGR | YOG | YBO | YRB

implementation Eq CornerColors where
    WBR == WBR = True
    WOB == WOB = True
    WGO == WGO = True
    WRG == WRG = True
    YGR == YGR = True
    YOG == YOG = True
    YBO == YBO = True
    YRB == YRB = True
    _   == _   = False


cornerToNum : CornerColors -> Nat
cornerToNum WBR = 0
cornerToNum WOB = 1
cornerToNum WGO = 2
cornerToNum WRG = 3
cornerToNum YGR = 4
cornerToNum YOG = 5
cornerToNum YBO = 6
cornerToNum YRB = 7


Corner : Type
Corner = Pair CornerColors Nat


CornerState : Type
CornerState = Vect 8 Corner


PCube : Type
PCube = Pair CornerState EdgeState


edgeContribution : Edge -> Nat
edgeContribution = snd


edgeParity : EdgeState -> Nat
edgeParity = sum . (map edgeContribution)


cornerContribution : Corner -> Nat
cornerContribution = snd


cornerParity : CornerState -> Nat
cornerParity = sum . (map cornerContribution)


disjoint : (n : Nat) -> Z = S n -> Void
disjoint n p = replace {P = disjointTy} p ()
    where
        disjointTy : Nat -> Type
        disjointTy Z = ()
        disjointTy (S k) = Void


neq0x : (n : Nat) -> 0 = (S n) -> Void
neq0x n = disjoint n


neqx0 : (n : Nat) -> (S n) = 0 -> Void
neqx0 x p = neq0x x (sym p)

modTwo : Nat -> Nat
modTwo x = modNatNZ x 2 (neqx0 1)

modThree : Nat -> Nat
modThree x = modNatNZ x 3 (neqx0 2)


inParity : PCube -> Bool
inParity (cs, es) = let cPar = cornerParity cs in
    let ePar = edgeParity es in
        modNatNZ cPar 3 (neqx0 2) == 0 && modNatNZ ePar 2 (neqx0 1) == 0


ParityCheck : Type
ParityCheck = Pair (Vect 8 Nat) (Vect 12 Nat)


toParityCheck : PCube -> ParityCheck
toParityCheck (corners, edges) = (map (cornerToNum . fst) corners, map (edgeToNum . fst) edges)

data CubeEqual = Ceq PCube

implementation Eq CubeEqual where
    (Ceq a@(c1, e1)) == (Ceq b@(c2, e2)) = if inParity a && inParity b
        then map fst c1 == map fst c2 && map fst e1 == map fst e2
        else False


