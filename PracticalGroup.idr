module PracticalGroup

--import Group
import Data.Vect
import Decomp

public export
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

public export
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

public export
Edge : Type
Edge = Pair EdgeColors Nat

public export
EdgeState : Type
EdgeState = Vect 12 Edge

public export
data CornerColors = WBR | WOB | WGO | WRG | YGR | YOG | YBO | YRB

public export
cornerToNum : CornerColors -> Nat
cornerToNum WBR = 0
cornerToNum WOB = 1
cornerToNum WGO = 2
cornerToNum WRG = 3
cornerToNum YGR = 4
cornerToNum YOG = 5
cornerToNum YBO = 6
cornerToNum YRB = 7

public export
Corner : Type
Corner = Pair CornerColors Nat

public export
CornerState : Type
CornerState = Vect 8 Corner

public export
PCube : Type
PCube = Pair CornerState EdgeState

public export
edgeContribution : Edge -> Nat
edgeContribution = snd

public export
edgeParity : EdgeState -> Nat
edgeParity = sum . (map edgeContribution)

public export
cornerContribution : Corner -> Nat
cornerContribution = snd

public export
cornerParity : CornerState -> Nat
cornerParity = sum . (map cornerContribution)

public export
ParityCheck : Type
ParityCheck = Pair (Vect 8 Nat) (Vect 12 Nat)

public export
toParityCheck : PCube -> ParityCheck
toParityCheck (corners, edges) = (map (cornerToNum . fst) corners, map (edgeToNum . fst) edges)




