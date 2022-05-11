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


rmultEq : (Group g) => (a, b, c : g) -> a = b -> times a c = times b c
rmultEq a b c eq = cong {f = (\y => times y c)} eq

{- eProofR : (Group g) => (a : g) -> times a Group.e = a
eProofR a = trans (trans (trans
                (cong {f = (\x => times a x)} (sym $ invProofL a)) -- a e = a (a' a)
                (sym $ assocProof a (inv a) a))                    -- a (a' a) = (a a') a
                (cong {f = (\x => times x a)} (invProofL a)))      -- (a a') a = e a
                (eProofL a)                                        -- e a = a  

-}

--invProofR : (Group g) => (a : g) -> times a (inv a) = Group.e
--invProofR a = 

TwiceInvNeutral : (Group g) => (a : g) -> a = inv (inv a) 
TwiceInvNeutral a = trans (trans (trans (trans
                        (sym $ eProofL a)                                         -- a = e a
                        (sym (cong {f = (\x => times x a)} (invProofL (inv a))))) -- e a = (a'' a') a 
                        (assocProof (inv (inv a)) (inv a) a))                     -- (a'' a') a = a'' (a' a) 
                        (cong {f = (\x => times (inv (inv a)) x)} (invProofL a))) -- a'' (a' a) = a'' e
                        (eProofR (inv (inv a)))                                   -- a'' e = a''


--tin_1 : (Group g) => (a : g) -> (rtimes a) Group.e  = (rtimes a) (times (inv (inv a)) (inv a))
--tin_1 a = cong (sym $ invProofL (inv a))    

-- a = ea = (a''a')a = a''(a'a) = a''

--ipr_2 : (Group g) => (a : g) -> times


-- aa' = eaa' =  a''a'aa' = a''a' = e


{- eIsUnique : (Group g) => (x : g) -> (f : g) -> (times x f = x) -> (f = Group.e)
eIsUnique x f xf_eq_x = 
    (trans (the (f = times Group.e f) (sym (eProofL f)))
        (trans (the (times Group.e f = times (times (inv x) x) f) (cong (\y => times y f)))
            (trans (the (times (times (inv x) x) f = times (inv x) (times x f)) ?todo2)
                (trans (the (times (inv x) (times x f) = times (inv x) x) ?todo3)
                    (the (times (inv x) x = Group.e) ?todo4)))))  
-}
 






