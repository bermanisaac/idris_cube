module Group

public export
interface Group g where
    times : g -> g -> g
    e : g
    inv : g -> g
    assocProof : (a, b, c : g) -> times (times a b) c = times a (times b c)
    eProofL : (a : g) -> times e a = a
    invProofL : (a : g) -> times (inv a) a = e

unitIsDumb : (a : ()) -> a = ()
unitIsDumb () = Refl

Group () where
    times x y = y
    inv x = x
    e = ()
    assocProof = \x,y,z => Refl
    eProofL = \x => Refl
    invProofL = unitIsDumb

rtimes : (Group g) => g -> g -> g
rtimes a b = Group.times b a 

rmultEq : (Group g) => (a, b, c : g) -> b = c -> times b a = times c a
rmultEq a b c eq = ?pain 







--invProofR : (Group g) => (a : g) -> times a (inv a) = Group.e
--invProofR a = 

--TwiceInvNeutral : (Group g) => (a : g) -> a = inv (inv a) 





--tin_1 : (Group g) => (a : g) -> (rtimes a) Group.e  = (rtimes a) (times (inv (inv a)) (inv a))
--tin_1 a = cong (sym $ invProofL (inv a))    

-- a = ea = (a''a')a = a''(a'a) = a''

--ipr_2 : (Group g) => (a : g) -> times


-- aa' = eaa' =  a''a'aa' = a''a' = e

eProofR : (Group g) => (a : g) -> times a Group.e = a
eProofR a = ?epr 

epr_1 : (Group g) => (a :g) -> times a Group.e = times a (times (inv a) a)
epr_1 a = cong $ sym $ Group.invProofL a  

epr_2 : (Group g) => (a :g) -> times a (times (inv a) a) = times (times a (inv a)) a
epr_2 a = sym $ Group.assocProof a (inv a) a


--epr_3 : (Group g) => (a :g) -> rtimes a (times a (inv a)) = rtimes a Group.e
--epr_3 a = cong $ invProofR a



{- eIsUnique : (Group g) => (x : g) -> (f : g) -> (times x f = x) -> (f = Group.e)
eIsUnique x f xf_eq_x = 
    (trans (the (f = times Group.e f) (sym (eProofL f)))
        (trans (the (times Group.e f = times (times (inv x) x) f) (cong (\y => times y f)))
            (trans (the (times (times (inv x) x) f = times (inv x) (times x f)) ?todo2)
                (trans (the (times (inv x) (times x f) = times (inv x) x) ?todo3)
                    (the (times (inv x) x = Group.e) ?todo4)))))  
-}
 






