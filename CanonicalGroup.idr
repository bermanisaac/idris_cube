import Group

data Turn = I | R | L | U | D | F | B

data CanonRubik = Position (List Turn)

ptol : CanonRubik -> List Turn
ptol (Position l) = l

normalize : (List Turn) -> (List Turn)
normalize [] = []
normalize (I :: turns) = normalize turns
normalize (x :: I :: turns) = normalize (x :: turns)
normalize (x :: y :: I :: turns) = normalize (x :: y :: turns)
normalize (x :: y :: z :: I :: turns) = normalize (x :: y :: z :: turns)
normalize (R :: R :: R :: R :: turns) = normalize turns
normalize (L :: L :: L :: L :: turns) = normalize turns
normalize (U :: U :: U :: U :: turns) = normalize turns
normalize (D :: D :: D :: D :: turns) = normalize turns
normalize (F :: F :: F :: F :: turns) = normalize turns
normalize (B :: B :: B :: B :: turns) = normalize turns
normalize (x :: turns) = (x :: (normalize turns))

appendturns : CanonRubik -> CanonRubik -> CanonRubik
appendturns (Position a) (Position b) = Position (normalize (a ++ b))

invturns : (List Turn) -> (List Turn)
invturns [] = []
invturns (I :: turns) = invturns turns
invturns (R :: R:: R :: turns) = (invturns turns) ++ [R]
invturns (L :: L:: L :: turns) = (invturns turns) ++ [L]
invturns (U :: U:: U :: turns) = (invturns turns) ++ [U]
invturns (D :: D:: D :: turns) = (invturns turns) ++ [D]
invturns (F :: F:: F :: turns) = (invturns turns) ++ [F]
invturns (B :: B:: B :: turns) = (invturns turns) ++ [B]
invturns (R :: R :: turns) = (invturns turns) ++ [R, R]
invturns (L :: L :: turns) = (invturns turns) ++ [L, L]
invturns (U :: U :: turns) = (invturns turns) ++ [U, U]
invturns (D :: D :: turns) = (invturns turns) ++ [D, D]
invturns (F :: F :: turns) = (invturns turns) ++ [F, F]
invturns (B :: B :: turns) = (invturns turns) ++ [B, B]
invturns (x :: turns) = (invturns turns) ++ [x, x, x]

Group CanonRubik where
    times = appendturns 
    e = Position []
    inv (Position turns) = (Position (invturns turns))
    assocProof = ?assocProofhole
    eProofL = ?eProofLhole
    invProofL = ?invProofLhole

    --fuck man idk