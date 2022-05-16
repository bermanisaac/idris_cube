module PracticalGroup

import Group
import Data.Vect

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

ParityCheck : Type
ParityCheck = Pair (Vect 12 Nat) (Vect 8 Nat)

toParityCheck : PCube -> ParityCheck
toParityCheck (corners, edges) = Pair (map cornerToNum corners) (map edgeToNum edges)

R : PCube -> PCube
R (Pair corners edges) = Pair corners edges