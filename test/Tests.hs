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
handleReq = map unReq

type TB a = Batch (Req Integer) a

-- TODO: dunno if these Arbitrary instances are any good.

instance Arbitrary (TB Integer) where
    arbitrary = oneof
        [ pure <$> arbitrary
        , liftA2 (\f r -> f <$> request r) arbitrary arbitrary
        , liftA2 (<*>) (fmap apply' <$> (arbitrary :: Gen (TB (Fun' Integer Integer)))) arbitrary
        ]

instance Arbitrary (TB (Fun' Integer Integer)) where
    arbitrary = oneof
        [ pure <$> arbitrary
        ]

instance Show a => Show (TB a) where
    show = show . runBatch handleReq

infix 0 =~=
a =~= b = runBatch handleReq a == runBatch handleReq b

newtype Fun' a b = Fun' { unFun' :: Fun a b } deriving (Arbitrary)
apply' = apply . unFun'

instance Show a => Show (Fun' Integer a) where
    show (apply' -> f) = "{" ++ intercalate ", " (map (\x -> show x ++ "->" ++ show (f x)) [-10..10]) ++ "}"

-- Functor laws
prop_functorId (b :: TB Integer) =
    fmap id b =~= b
prop_functorComp (b :: TB Integer) (apply' -> f) (apply' -> g) =
    fmap (f . g) b =~= fmap f (fmap g b)

-- Applicative laws
prop_applicativeId (b :: TB Integer) =
    pure id <*> b =~= b
prop_applicativeComp (fmap apply' -> u) (fmap apply' -> v) (w :: TB Integer) =
    pure (.) <*> u <*> v <*> w =~= u <*> (v <*> w)
prop_applicativeHomomorphism (apply' -> f) x =
    pure f <*> pure x =~= pure (f x)

return []
main = $quickCheckAll
