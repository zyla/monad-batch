{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables #-}
-- | A data type for computations with requests that can be batched together
-- and possibly executed more efficiently.
--
-- To write code in BatchT monad, you must provide a request type and a result
-- type (via 'Result' type family).  For now, each request type can have only
-- one result type.
--
-- To actually run the code, you have to supply a 'Handler' function that does
-- actual request processing.
--
-- Things to improve:
--
--  * encode the first Handler law in the type (like the Cartesian Store
--    Comonad from Multiplate paper)
--
--  * support multiple response types (request type indexed by response type)
--
module Control.Monad.Batch (
    BatchT
  , Batch
  , Result
  , Handler
  , request
  , runBatch
  , runBatchT
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.List (splitAt)

type family Result req :: *

-- Handler function for batched requests.
-- Functions @handler :: Handler req@ should satisfy the following laws:
-- @
--  length xs = length (handler xs)
--  handler (xs ++ ys) = handler xs ++ handler ys
-- @
type Handler req m = [req] -> m [Result req]

type Batch r = BatchT r Identity

data BatchT r (m :: * -> *) a where
    Lift :: m a -> BatchT r m a
    Bind :: BatchT r m a -> (a -> BatchT r m b) -> BatchT r m b
    Request :: [r] -> ([Result r] -> BatchT r m a) -> BatchT r m a

instance Applicative m => Functor (BatchT r m) where
    fmap f x = pure f <*> x

request :: Applicative m => r -> BatchT r m (Result r)
request req = Request [req] (pure . head)

instance Applicative m => Applicative (BatchT r m) where
    pure = Lift . pure

    Lift mf <*> Lift mx = Lift $ mf <*> mx
    Bind mf k <*> Lift mx = Bind mf ((<*> Lift mx) . k)
    Lift mf <*> Bind mx k = Bind mx ((Lift mf <*>) . k)
    f <*> x =
        let (rf, kf) = toRequest f
            (rx, kx) = toRequest x

            combine results =
                let (resultsF, resultsX) = splitAt (length rf) results
                in kf resultsF <*> kx resultsX

        in Request (rf ++ rx) combine

-- Return a pair (requests, continuation) for given BatchT.
-- 'Lift' can be represented as @Request [] . const . Lift@ - a "request"
-- with no requests, always returning the same value.
toRequest :: Applicative m => BatchT r m a -> ([r], [Result r] -> BatchT r m a)
toRequest (Lift x) = ([], const $ Lift x)
toRequest (Bind m k) =
    (reqs, k1 >=> k)
  where
    (reqs, k1) = toRequest m
toRequest (Request r k) = (r, k)

instance Applicative m => Monad (BatchT r m) where
    return = Lift . pure
    (>>=) = Bind

runBatchT :: Monad m => Handler r m -> BatchT r m a -> m a
runBatchT handle = go
  where
    go (Lift m) = m
    go (Bind m k) = runBatchT handle m >>= go . k
    go (Request r k) = handle r >>= go . k

runBatch handle = runIdentity . runBatchT handle
