module Algs

--import Group
import Data.Vect
import PracticalGroup
import CubeRotations

%access public export

-- Orient corners
twist : PCube -> PCube
twist = R . R . R . D . D . D . R . D

ocUFR1 : PCube -> PCube
ocUFR1 = twist . twist . U . twist . twist . twist . twist . U . U . U

ocUFR2 : PCube -> PCube
ocUFR2 = twist . twist . twist . twist . U . twist . twist . U . U . U

ocUBR1 : PCube -> PCube
ocUBR1 = U . twist . twist . U . twist . twist . twist . twist . U . U

ocUBR2 : PCube -> PCube
ocUBR2 = U . twist . twist . twist . twist . U . twist . twist . U . U

ocUBL1 : PCube -> PCube
ocUBL1 = U . U . twist . twist . U . twist . twist . twist . twist . U

ocUBL2 : PCube -> PCube
ocUBL2 = U . U . twist . twist . twist . twist . U . twist . twist . U

ocUFL1 : PCube -> PCube
ocUFL1 = U . U . L . L . U . twist . twist . U . twist . twist . twist . twist . U . U . L . L . U . U

ocUFL2 : PCube -> PCube
ocUFL2 = U . U . L . L . U . twist . twist . twist . twist . U . twist . twist . U . U . L . L . U . U

twistLU : PCube -> PCube
twistLU = L . L . L . U . U . U . L . U

ocDFL1 : PCube -> PCube
ocDFL1 = twistLU . twistLU . D . twistLU . twistLU . twistLU . twistLU . D . D . D

ocDFL2 : PCube -> PCube
ocDFL2 = twistLU . twistLU . twistLU . twistLU . D . twistLU . twistLU . D . D . D

ocDBL1 : PCube -> PCube
ocDBL1 = D . twistLU . twistLU . D . twistLU . twistLU . twistLU . twistLU . D . D

ocDBL2 : PCube -> PCube
ocDBL2 = D . twistLU . twistLU . twistLU . twistLU . D . twistLU . twistLU . D . D

ocDBR1 : PCube -> PCube
ocDBR1 = D . D . twistLU . twistLU . D . twistLU . twistLU . twistLU . twistLU . D

ocDBR2 : PCube -> PCube
ocDBR2 = D . D . twistLU . twistLU . twistLU . twistLU . D . twistLU . twistLU . D

-- Orient edges
-- G-perm changes the orientation
--   of UB and UL edges
G : PCube -> PCube
G = L . R' . F . L' . R . U . U . L . R' . F . L . R' . B . B . U' . L' . R . B . B . L . R' . U' . B . B


