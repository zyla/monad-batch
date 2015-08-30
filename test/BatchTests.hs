{-# LANGUAGE TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, ViewPatterns #-}
module Tests where

import Control.Applicative
import Control.Monad.Writer
import Test.QuickCheck hiding (Result)
import Control.Monad.Batch

newtype Req a = Req { unReq :: a } deriving (Arbitrary)
type instance Result (Req a) = a

handleReq :: Handler (Req a) (Writer [[a]])
handleReq reqs = let results = map unReq reqs in tell [results] >> pure results

req :: Applicative m => Integer -> BatchT (Req Integer) m Integer
req = request . Req

run = execWriter . runBatchT handleReq

pair = liftA2 (,)

prop_batchAp x y = run (pair (req x) (req y)) === [[x, y]]
prop_noBatchM x y = run (req x >> req y) === [[x], [y]]

prop_batchLeftBindAp x y z = run (pair (req x >> req z) (req y)) === [[x, y], [z]]
prop_batchRightBindAp x y z = run (pair (req x) (req y >> req z)) === [[x, y], [z]]

prop_twoBatches x y z w = run (pair (req x >> req z) (req y >> req w)) === [[x, y], [z, w]]

prop_anyBatches xs ys =
        run (pair (mapM_ req xs) (mapM_ req ys))
    === zipWith (\a b -> [a,b]) xs ys ++ map return (leftovers xs ys)

leftovers xs ys = drop n xs ++ drop n ys
  where
    n = min (length xs) (length ys)

return []
main = $quickCheckAll
