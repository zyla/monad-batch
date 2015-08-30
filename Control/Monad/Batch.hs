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
    Lift :: m a -> BatchT r m a
    Bind :: BatchT r m a -> (a -> BatchT r m b) -> BatchT r m b
    Ap :: BatchT r m (a -> b) -> BatchT r m a -> BatchT r m b
    Request :: r -> BatchT r m (Result r)

instance Applicative m => Functor (BatchT r m) where
    fmap = (<*>) . pure

request :: Applicative m => r -> BatchT r m (Result r)
request req = Request req

instance Applicative m => Applicative (BatchT r m) where
    pure = Lift . pure
    (<*>) = Ap

instance (Applicative m, Monad m) => Monad (BatchT r m) where
    return = Lift . return
    (>>=) = Bind

lift :: m a -> BatchT r m a
lift = Lift

data View r m a where
    Pure :: a -> View r m a
    More :: [r] -> ([Result r] -> BatchT r m a) -> View r m a

view :: (Functor m, Applicative m, Monad m) => BatchT r m a -> m (View r m a)
view (Lift m) = Pure <$> m
view (m `Bind` f) = view m >>= bindView f
view (mf `Ap` mx) = uncurry combine <$> liftA2 (,) (view mf) (view mx)
view (Request r) = return $ More [r] (return . head)

bindView :: (Functor m, Applicative m, Monad m)
         => (a -> BatchT r m b) -> View r m a -> m (View r m b)
bindView f (Pure x) = view $ f x
bindView f (More reqs k) = return $ More reqs (k >=> f)

combine :: (Applicative m) => View r m (a -> b) -> View r m a -> View r m b
combine (Pure f) (Pure x) = Pure $ f x
combine (Pure f) (reqs `More` g) = More reqs (fmap f . g)
combine (reqs `More` f) (Pure x) = More reqs (fmap ($ x) . f)
combine (rf `More` kf) (rx `More` kx) = More (rf ++ rx) $ \results ->
    let (resultsF, resultsX) = splitAt (length rf) results
    in kf resultsF <*> kx resultsX

runBatchT :: (Applicative m, Monad m) => Handler r m -> BatchT r m a -> m a
runBatchT handle = view >=> eval
  where
    eval (Pure x) = return x
    eval (More reqs k) = handle reqs >>= runBatchT handle . k

runBatch handle = runIdentity . runBatchT handle
