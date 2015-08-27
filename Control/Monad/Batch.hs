{-# LANGUAGE GADTs, TypeFamilies #-}
-- | A data type for computations with requests that can be batched together
-- and possibly executed more efficiently.
--
-- To write code in Batch monad, you must provide a request type and a result
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
--  * make it a monad transformer
module Control.Monad.Batch (
    Batch
  , Result
  , Handler
  , request
  , runBatch
) where

import Control.Applicative
import Control.Monad
import Data.List (splitAt)

type family Result req :: *

-- Handler function for batched requests.
-- Functions @handler :: Handler req@ should satisfy the following laws:
-- @
--  length xs = length (handler xs)
--  handler (xs ++ ys) = handler xs ++ handler ys
-- @
type Handler req = [req] -> [Result req]

data Batch r a where
    Pure :: a -> Batch r a
    Request :: [r] -> ([Result r] -> Batch r a) -> Batch r a

instance Functor (Batch r) where
    fmap f x = pure f <*> x

request :: r -> Batch r (Result r)
request req = Request [req] (pure . head)

instance Applicative (Batch r) where
    pure = Pure
    Pure f <*> Pure x = Pure $ f x
    f <*> x = 
        let (rf, kf) = toRequest f
            (rx, kx) = toRequest x

            -- Return a pair (requests, continuation) for given Batch.
            -- 'Pure' can be represented as @Request [] . const . pure@ - a "request"
            -- with no requests, always returning the same value.
            toRequest (Pure x) = ([], const $ pure x)
            toRequest (Request r k) = (r, k)

            combine results =
                let (resultsF, resultsX) = splitAt (length rf) results
                in kf resultsF <*> kx resultsX

        in Request (rf ++ rx) combine

instance Monad (Batch r) where
    return = pure
    Pure x >>= f = f x
    Request r k >>= f = Request r (k >=> f)

runBatch :: Handler r -> Batch r b -> b
runBatch handle = go
  where
    go (Pure x) = x
    go (Request r k) = go $ k $ handle r
