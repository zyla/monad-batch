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
--  * make it a Monad (now it's only Applicative)
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
    Request :: [r] -> ([Result r] -> a) -> Batch r a

instance Functor (Batch r) where
    fmap f x = pure f <*> x

request :: r -> Batch r (Result r)
request req = Request [req] head

instance Applicative (Batch r) where
    pure = Request [] . const
    Request rf kf <*> Request rx kx =
        let nf = length rf
            combine results =
                let (resultsF, resultsX) = splitAt nf results
                in kf resultsF $ kx resultsX
        in Request (rf ++ rx) combine

runBatch :: Handler r -> Batch r b -> b
runBatch handle = go
  where
    go (Request r k) = k $ handle r
