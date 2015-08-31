module LawsTests where

import Common

type TB a = BatchT (Req Integer) (Writer [Integer]) a
type V = Integer
type F = Fun' Integer Integer
type MF = Fun' Integer (TB Integer)

-- TODO: dunno if these Arbitrary instances are any good.

instance Arbitrary (TB Integer) where
    arbitrary = oneof
        [ pure <$> arbitrary
        , liftA2 (>>) (lift . tell . pure <$> arbitrary) arbitrary
        , liftA2 (\f r -> f <$> request r) arbitrary arbitrary
        , liftA2 (<*>) (fmap apply' <$> (arbitrary :: Gen (TB (Fun' Integer Integer)))) arbitrary
        ]

instance Arbitrary (TB F) where
    arbitrary = oneof
        [ pure <$> arbitrary
        , liftA2 (>>) (arbitrary :: Gen (TB Integer)) arbitrary
        ]

instance Show a => Show (TB a) where
    show = show . run

run = runWriter . runBatchT handleReq

handleReq :: [Req a] -> Writer [Integer] [a]
handleReq = pure . map unReq

-- A property meaning 'a and b have the same effect and return value'
a =~= b = run a === run b
infix 0 =~=

-- Functor laws
prop_functorId (b :: TB V) =
    fmap id b =~= b

prop_functorComp :: TB V -> F -> F -> Property
prop_functorComp b (apply' -> f) (apply' -> g) =
    fmap (f . g) b =~= fmap f (fmap g b)

-- Applicative laws
prop_applicativeId (b :: TB V) =
    pure id <*> b =~= b

prop_applicativeComp :: TB F -> TB F -> TB V -> Property
prop_applicativeComp (fmap apply' -> u) (fmap apply' -> v) w =
    pure (.) <*> u <*> v <*> w =~= u <*> (v <*> w)

prop_applicativeHomomorphism :: F -> V -> Property
prop_applicativeHomomorphism (apply' -> f) x =
    pure f <*> pure x =~= pure (f x)

-- Monad laws
prop_monadRightId (b :: TB V) =
    b >>= return =~= b

prop_monadLeftId (v :: V) (apply' -> k) =
    return v >>= k =~= k v

prop_monadAssoc :: TB V -> MF -> MF -> Property
prop_monadAssoc b (apply' -> f) (apply' -> g) =
    (b >>= f) >>= g =~= b >>= (\x -> f x >>= g)

return []
runTests = $quickCheckAll
