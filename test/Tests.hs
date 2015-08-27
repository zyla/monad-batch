{-# LANGUAGE TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, ViewPatterns #-}
module Tests where

import Control.Applicative
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Function
import Control.Monad.Batch
import Data.List

newtype Req a = Req { unReq :: a } deriving (Arbitrary)
type instance Result (Req a) = a
handleReq = pure . map unReq

-- TODO: dunno if these Arbitrary instances are any good.

instance Arbitrary (TB Integer) where
    arbitrary = oneof
        [ pure <$> arbitrary
        , liftA2 (\f r -> f <$> request r) arbitrary arbitrary
        , liftA2 (<*>) (fmap apply' <$> (arbitrary :: Gen (TB (Fun' Integer Integer)))) arbitrary
        ]

instance Arbitrary (TB F) where
    arbitrary = oneof
        [ pure <$> arbitrary
        , liftA2 (>>) (arbitrary :: Gen (TB Integer)) arbitrary
        ]

instance Show a => Show (TB a) where
    show = show . runBatch handleReq

infix 0 =~=
a =~= b = runBatch handleReq a == runBatch handleReq b

newtype Fun' a b = Fun' { unFun' :: Fun a b } deriving (Arbitrary)
apply' = apply . unFun'

instance Show a => Show (Fun' Integer a) where
    show (apply' -> f) = "{" ++ intercalate ", " (map (\x -> show x ++ "->" ++ show (f x)) [-10..10]) ++ "}"

type TB a = Batch (Req Integer) a
type V = Integer
type F = Fun' Integer Integer
type MF = Fun' Integer (TB Integer)

-- Functor laws
prop_functorId (b :: TB V) =
    fmap id b =~= b

prop_functorComp :: TB V -> F -> F -> Bool
prop_functorComp b (apply' -> f) (apply' -> g) =
    fmap (f . g) b =~= fmap f (fmap g b)

-- Applicative laws
prop_applicativeId (b :: TB V) =
    pure id <*> b =~= b

prop_applicativeComp :: TB F -> TB F -> TB V -> Bool
prop_applicativeComp (fmap apply' -> u) (fmap apply' -> v) w =
    pure (.) <*> u <*> v <*> w =~= u <*> (v <*> w)

prop_applicativeHomomorphism :: F -> V -> Bool
prop_applicativeHomomorphism (apply' -> f) x =
    pure f <*> pure x =~= pure (f x)

-- Monad laws
prop_monadRightId (b :: TB V) =
    b >>= return =~= b

prop_monadLeftId (v :: V) (apply' -> k) =
    return v >>= k =~= k v

prop_monadAssoc :: TB V -> MF -> MF -> Bool
prop_monadAssoc b (apply' -> f) (apply' -> g) =
    (b >>= f) >>= g =~= b >>= (\x -> f x >>= g)

return []
main = $quickCheckAll
