module Common (module Common, module X) where

import Control.Applicative as X
import Control.Monad.Writer as X
import Test.QuickCheck as X hiding (Result)
import Test.QuickCheck.Function as X
import Control.Monad.Batch as X
import Data.List as X

newtype Req a = Req { unReq :: a } deriving (Arbitrary)
type instance Result (Req a) = a

-- A shorthand for making requests.
req :: Applicative m => Integer -> BatchT (Req Integer) m Integer
req = request . Req

pair :: Applicative f => f a -> f b -> f (a, b)
pair = liftA2 (,)

-- The Show instance for Fun tries to print an infinite domain
-- This is a wrapper to work around this
newtype Fun' a b = Fun' { unFun' :: Fun a b } deriving (Arbitrary)
apply' = apply . unFun'

-- Prints function values in range [-10..10]. This is usually sufficient
-- because values generated by QuickCheck (after shrinking) are small.
instance Show a => Show (Fun' Integer a) where
    show (apply' -> f) = "{" ++ intercalate ", " (map (\x -> show x ++ "->" ++ show (f x)) [-10..10]) ++ "}"