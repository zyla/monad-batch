{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
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
  , lift
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.List (splitAt)

#define TRACE 0

#if TRACE
import Debug.Trace (trace)
#else
trace _ = id
#endif

type family Result req :: *

-- Handler function for batched requests.
-- Functions @handler :: Handler req m@ should satisfy the following laws:
-- @
--  pure (length xs) = length (handler xs)
--  pure handler (xs ++ ys) = liftA2 (++) (handler xs) (handler ys)
-- @
type Handler req m = [req] -> m [Result req]

type Batch r = BatchT r Identity

data BatchT r (m :: * -> *) a where
    Request :: [r] -> (Handler r m -> [Result r] -> m a) -> BatchT r m a

instance Applicative m => Functor (BatchT r m) where
    fmap f x = pure f <*> x

request :: Applicative m => r -> BatchT r m (Result r)
request req = Request [req] (const $ pure . head)

instance Applicative m => Applicative (BatchT r m) where
    pure = Request [] . const . const . pure

    f <*> x | trace (describe f ++ " <*> " ++ describe x) False = undefined

    Request rf kf <*> Request rx kx =
        let combine handle results =
                let (resultsF, resultsX) = splitAt (length rf) results
                in kf handle resultsF <*> kx handle resultsX
        in Request (rf ++ rx) combine

describe :: BatchT r m a -> String
describe (Request reqs _) = "Request " ++ show (length reqs)

instance Monad m => Monad (BatchT r m) where
    return = Request [] . const . const . return
    Request r k >>= f = Request r (\handle -> k handle >=> (runBatchT handle . f))

lift = Request [] . const . const

runBatchT :: Monad m => Handler r m -> BatchT r m a -> m a
runBatchT handle (Request r k) = handle r >>= k handle

runBatch handle = runIdentity . runBatchT handle
