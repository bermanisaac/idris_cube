module Algs

import Data.Vect
import PracticalGroup
import CubeRotations

%access public export


infixr 10 ^
(^) : (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)

-- Orient corners
twist : PCube -> PCube
twist = R' ^ D' ^ R ^ D

ocUFR1 : PCube -> PCube
ocUFR1 = twist ^ twist ^ U ^ twist ^ twist ^ twist ^ twist ^ U'

ocUFR2 : PCube -> PCube
ocUFR2 = twist ^ twist ^ twist ^ twist ^ U ^ twist ^ twist ^ U'

ocUBR1 : PCube -> PCube
ocUBR1 = U ^ twist ^ twist ^ U ^ twist ^ twist ^ twist ^ twist ^ U2

ocUBR2 : PCube -> PCube
ocUBR2 = U ^ twist ^ twist ^ twist ^ twist ^ U ^ twist ^ twist ^ U2

ocUBL1 : PCube -> PCube
ocUBL1 = U2 ^ twist ^ twist ^ U ^ twist ^ twist ^ twist ^ twist ^ U

ocUBL2 : PCube -> PCube
ocUBL2 = U2 ^ twist ^ twist ^ twist ^ twist ^ U ^ twist ^ twist ^ U

ocUFL1 : PCube -> PCube
ocUFL1 = U2 ^ L2 ^ U ^ twist ^ twist ^ U ^ twist ^ twist ^ twist ^ twist ^ U2 ^ L2 ^ U2

ocUFL2 : PCube -> PCube
ocUFL2 = U2 ^ L2 ^ U ^ twist ^ twist ^ twist ^ twist ^ U ^ twist ^ twist ^ U2 ^ L2 ^ U2

ocUFR1s : String
ocUFR1s = "R'D'RD R'D'RD U R'D'RD R'D'RD R'D'RD R'D'RD U'"
ocUFR2s : String
ocUFR2s = "R'D'RD R'D'RD R'D'RD R'D'RD U R'D'RD R'D'RD U'"

ocUBR1s : String
ocUBR1s = "U R'D'RD R'D'RD U R'D'RD R'D'RD R'D'RD R'D'RD U2"
ocUBR2s : String
ocUBR2s = "U R'D'RD R'D'RD R'D'RD R'D'RD U R'D'RD R'D'RD U2"

ocUBL1s : String
ocUBL1s = "U2 R'D'RD R'D'RD U R'D'RD R'D'RD R'D'RD R'D'RD U"
ocUBL2s : String
ocUBL2s = "U2 R'D'RD R'D'RD R'D'RD R'D'RD U R'D'RD R'D'RD U"

ocUFL1s : String
ocUFL1s = "U2 L2 U R'D'RD R'D'RD U R'D'RD R'D'RD R'D'RD R'D'RD U2 L2 U2"
ocUFL2s : String
ocUFL2s = "U2 L2 U R'D'RD R'D'RD R'D'RD R'D'RD U R'D'RD R'D'RD U2 L2 U2"

ocDFL1s : String
ocDFL1s = "L'U'LU L'U'LU D L'U'LU L'U'LU L'U'LU L'U'LU D'"
ocDFL2s : String
ocDFL2s = "L'U'LU L'U'LU L'U'LU L'U'LU D L'U'LU L'U'LU D'"

ocDBL1s : String
ocDBL1s = "D L'U'LU L'U'LU D L'U'LU L'U'LU L'U'LU L'U'LU D2"
ocDBL2s : String
ocDBL2s = "D L'U'LU L'U'LU L'U'LU L'U'LU D L'U'LU L'U'LU D2"

ocDBR1s : String
ocDBR1s = "D2 L'U'LU L'U'LU D L'U'LU L'U'LU L'U'LU L'U'LU D"
ocDBR2s : String
ocDBR2s = "D2 L'U'LU L'U'LU L'U'LU L'U'LU D L'U'LU L'U'LU D"


twistLU : PCube -> PCube
twistLU = L' ^ U' ^ L ^ U

ocDFL1 : PCube -> PCube
ocDFL1 = twistLU ^ twistLU ^ D ^ twistLU ^ twistLU ^ twistLU ^ twistLU ^ D'

ocDFL2 : PCube -> PCube
ocDFL2 = twistLU ^ twistLU ^ twistLU ^ twistLU ^ D ^ twistLU ^ twistLU ^ D'

ocDBL1 : PCube -> PCube
ocDBL1 = D ^ twistLU ^ twistLU ^ D ^ twistLU ^ twistLU ^ twistLU ^ twistLU ^ D2

ocDBL2 : PCube -> PCube
ocDBL2 = D ^ twistLU ^ twistLU ^ twistLU ^ twistLU ^ D ^ twistLU ^ twistLU ^ D2

ocDBR1 : PCube -> PCube
ocDBR1 = D2 ^ twistLU ^ twistLU ^ D ^ twistLU ^ twistLU ^ twistLU ^ twistLU ^ D

ocDBR2 : PCube -> PCube
ocDBR2 = D2 ^ twistLU ^ twistLU ^ twistLU ^ twistLU ^ D ^ twistLU ^ twistLU ^ D

-- Orient edges
-- G-perm changes the orientation
--   of UB and UL edges
-- note: G-perm is a fake permutation that I made as a temporary variable because I didn't want to keep rewriting alg
G : PCube -> PCube
    -- L   R'   F   L'   R   U2   L   R'   F   L    R'  B2   U'   L'   R   B2 L R' U' B2
G = L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L' ^ R ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2

oeStep1 : PCube -> PCube
oeStep1 = U ^ G ^ U'

oeStep1s : String
oeStep1s = "U ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ U'"

oeStep2 : PCube -> PCube
oeStep2 = G

oeStep2s : String
oeStep2s = "L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2"

oeStep3 : PCube -> PCube
oeStep3 = U' ^ G ^ U

oeStep3s : String
oeStep3s = "U' ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ U"

oeStep4 : PCube -> PCube
oeStep4 = F2 ^ U2 ^ G ^ U2 ^ F2

oeStep4s : String
oeStep4s = "F2 ^ U2 ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ U2 ^ F2"

oeStep5 : PCube -> PCube
oeStep5 = F2 ^ L2 ^ U2 ^ G ^ U2 ^ L2 ^ F2

oeStep5s : String
oeStep5s = "F2 ^ L2 ^ U2 ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ U2 ^ L2 ^ F2"

oeStep6 : PCube -> PCube
oeStep6 = L2 ^ B2 ^ G ^ B2 ^ L2

oeStep6s : String
oeStep6s = "L2 ^ B2 ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ B2 ^ L2"

oeStep7 : PCube -> PCube
oeStep7 = D ^ L2 ^ B2 ^ G ^ B2 ^ L2 ^ D'

oeStep7s : String
oeStep7s = "D ^ L2 ^ B2 ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ B2 ^ L2 ^ D'"

oeStep8 : PCube -> PCube
oeStep8 = F ^ R2 ^ U2 ^ G ^ U2 ^ R2 ^ F'

oeStep8s : String
oeStep8s = "F ^ R2 ^ U2 ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ U2 ^ R2 ^ F'"

oeStep9 : PCube -> PCube
oeStep9 = L' ^ U ^ L2 ^ G ^ L2 ^ U' ^ L

oeStep9s : String
oeStep9s = "L' ^ U ^ L2 ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ L2 ^ U' ^ L"

oeStep10 : PCube -> PCube
oeStep10 = B' ^ U' ^ B2 ^ G ^ B2 ^ U ^ B

oeStep10s : String
oeStep10s = "B' ^ U' ^ B2 ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ B2 ^ U ^ B"

oeStep11 : PCube -> PCube
oeStep11 = B ^ R ^ U' ^ G ^ U ^ R' ^ B'

oeStep11s : String
oeStep11s = "B ^ R ^ U' ^ L ^ R' ^ F ^ L' ^ R ^ U2 ^ L ^ R' ^ F ^ L ^ R' ^ B2 ^ U' ^ L' ^ R ^ B2 ^ L ^ R' ^ U' ^ B2 ^ U ^ R' ^ B'"





-- Permute Corners
--
T : PCube -> PCube
T = R ^ U ^ R' ^ U' ^ R' ^ F ^ R2 ^ U' ^ R' ^ U' ^ R ^ U ^ R' ^ F'

--"R U R' U' R' F R2 U' R' U' R U R' F'"

--permuteEdges : Int -> Int -> PCube -> PCube
-- we need to have a case in which permuteEdges matches for n = n and then does Nothing
getCornerSwap : Int -> Int -> (PCube -> PCube)
getCornerSwap 1 2 = T
getCornerSwap 1 3 = B ^ R ^ D' ^ R' ^ T ^ R ^ D ^ R' ^ B'
getCornerSwap 1 4 = L ^ R ^ D2 ^ L' ^ R' ^ T ^ R ^ L ^ D2 ^ R' ^ L'
getCornerSwap 1 5 = U' ^ L2 ^ U ^ T ^ U' ^ L2 ^ U
getCornerSwap 1 6 = B2 ^ T ^ B2
getCornerSwap 1 7 = D ^ B2 ^ T ^ B2 ^ D'
getCornerSwap 1 8 = D2 ^ B2 ^ T ^ B2 ^ D2
getCornerSwap 2 3 = L' ^ R' ^ D2 ^ R ^ L ^ T ^ L' ^ R' ^ D2 ^ R ^ L
getCornerSwap 2 4 = F' ^ R' ^ D ^ R ^ T ^ R' ^ D' ^ R ^ F
getCornerSwap 2 5 = F2 ^ T ^ F2
getCornerSwap 2 6 = D ^ F2 ^ T ^ F2 ^ D'
getCornerSwap 2 7 = D2 ^ F2 ^ T ^ F2 ^ D2
getCornerSwap 2 8 = D' ^ F2 ^ T ^ F2 ^ D
getCornerSwap 3 4 = U2 ^ T ^ U2
getCornerSwap 3 5 = D ^ F2 ^ U2 ^ T ^ U2 ^ F2 ^ D'
getCornerSwap 3 6 = D2 ^ F2 ^ U2 ^ T ^ U2 ^ F2 ^ D2
getCornerSwap 3 7 = D' ^ F2 ^ U2 ^ T ^ U2 ^ F2 ^ D
getCornerSwap 3 8 = F2 ^ U2 ^ T ^ U2 ^ F2
getCornerSwap 4 5 = U2 ^ F2 ^ T ^ F2 ^ U2
getCornerSwap 4 6 = D ^ U2 ^ F2 ^ T ^ F2 ^ U2 ^ D'
getCornerSwap 4 7 = D2 ^ U2 ^ F2 ^ T ^ F2 ^ U2 ^ D2
getCornerSwap 4 8 = D' ^ U2 ^ F2 ^ T ^ F2 ^ U2 ^ D
getCornerSwap 5 6 = B2 ^ F2 ^ T ^ F2 ^ B2
getCornerSwap 5 7 = D ^ B2 ^ D' ^ F2 ^ T ^ F2 ^ D ^ B2 ^ D'
getCornerSwap 5 8 = D ^ B2 ^ F2 ^ U2 ^ T ^ U2 ^ F2 ^ B2 ^ D'
getCornerSwap 6 8 = D ^ F2 ^ B2 ^ T ^ B2 ^ F2 ^ D'
getCornerSwap 6 7 = B2 ^ D' ^ F2 ^ T ^ F2 ^ D ^ B2
getCornerSwap 7 8 = B2 ^ F2 ^ U2 ^ T ^ U2 ^ F2 ^ B2
getCornerSwap n m = if n == m
                    then id
                    else getCornerSwap m n

getCornerSwaps : Int -> Int -> String
getCornerSwaps 1 2 = "T"
getCornerSwaps 1 3 = "B R D' R' T R D R' B'"
getCornerSwaps 1 4 = "L R D2 L' R' T R L D2 R' L'"
getCornerSwaps 1 5 = "U' L2 U T U' L2 U2"
getCornerSwaps 1 6 = "B2 T B2"
getCornerSwaps 1 7 = "D B2 T B2 D'"
getCornerSwaps 1 8 = "D2 B2 T B2 D2"
getCornerSwaps 2 3 = "L' R' D2 R L T L' R' D2 R L"
getCornerSwaps 2 4 = "F' R' D R T R' D' R F"
getCornerSwaps 2 5 = "F2 T F2"
getCornerSwaps 2 6 = "D F2 T F2 D'"
getCornerSwaps 2 7 = "D2 F2 T F2 D2"
getCornerSwaps 2 8 = "D' F2 T F2 D"
getCornerSwaps 3 4 = "U2 T U2"
getCornerSwaps 3 5 = "D F2 U2 T U2 F2 D'"
getCornerSwaps 3 6 = "D2 F2 U2 T U2 F2 D2"
getCornerSwaps 3 7 = "D' F2 U2 T U2 F2 D"
getCornerSwaps 3 8 = "F2 U2 T U2 F2"
getCornerSwaps 4 5 = "U2 F2 T F2 U2"
getCornerSwaps 4 6 = "D U2 F2 T F2 U2 D'"
getCornerSwaps 4 7 = "D2 U2 F2 T F2 U2 D2"
getCornerSwaps 4 8 = "D' U2 F2 T F2 U2 D"
getCornerSwaps 5 6 = "B2 F2 T F2 B2"
getCornerSwaps 5 7 = "D B2 D' F2 T F2 D B2 D'"
getCornerSwaps 5 8 = "D B2 F2 U2 T U2 F2 B2 D'"
getCornerSwaps 6 8 = "D F2 B2 T B2 F2 D'"
getCornerSwaps 6 7 = "B2 D' F2 T F2 D B2"
getCornerSwaps 7 8 = "B2 F2 U2 T U2 F2 B2"
getCornerSwaps n m = if n == m
                    then ""
                    else getCornerSwaps m n


-- Permute edges
A : PCube -> PCube
A = L2 ^ U ^ F' ^ B ^ L2 ^ F ^ B' ^ U ^ L2

--Maps n to the 3-cycle (1, 2, n)
getEdgeCycle : Nat -> (PCube -> PCube)
getEdgeCycle 3 = A
getEdgeCycle 4 = R2 ^ D ^ B2 ^ A ^ B2 ^ D' ^ R2
getEdgeCycle 5 = D2 ^ B2 ^ A ^ B2 ^ D2
getEdgeCycle 6 = D' ^ B2 ^ A ^ B2 ^ D
getEdgeCycle 7 = B2 ^ A ^ B2
getEdgeCycle 8 = D ^ B2 ^ A ^ B2 ^ D'
getEdgeCycle 9 = U' ^ L' ^ U ^ A ^ U' ^ L ^ U
getEdgeCycle 10 = B' ^ A ^ B
getEdgeCycle 11 = U ^ R' ^ U' ^ A ^ U ^ R ^ U'
getEdgeCycle 12 = U2 ^ F' ^ U2 ^ A ^ U2 ^ F ^ U2












