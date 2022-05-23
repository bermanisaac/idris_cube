module Algs

import Group
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
ocUBR2 = U . twist . twist . U twist . twist . twist . twist . U . U

ocUBR2 : PCube -> PCube
ocUBR2 = U . twist . twist . twist . twist . U . twist . twist . U . U
