module Group

interface Group G where
    times : G -> G -> G
    timesAssoc : (a, b, c : G) -> times (times a b) c = times a (times b c)
    inv : G -> G
    e : G
    eWorks : (a : G) -> times e a = a
    invWorks : (a : G) -> times (inv a) a = e

unitIsDumb : (a : ()) -> a = ()
unitIsDumb () = Refl

Group () where
    times x y = y
    timesAssoc = \x,y,z => Refl
    inv x = x
    e = ()
    eWorks = \x => Refl
    invWorks = unitIsDumb

rightInvisleftInv : (G : Group, g, inv' : G) -> times g inv' = e -> inv' = inv g
rightInvisleftInv = ?proof
