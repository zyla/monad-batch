{-# LANGUAGE GADTs, TypeFamilies, DeriveFunctor #-}
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
import Control.Monad.Trans (MonadTrans, lift)
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

newtype BatchT r (m :: * -> *) a = BatchT { view :: m (View r m a) } deriving (Functor)

request :: Applicative m => r -> BatchT r m (Result r)
request req = BatchT $ pure $ More [req] (pure . head)

instance Applicative m => Applicative (BatchT r m) where
    pure = BatchT . pure . Pure
    mf <*> mx = BatchT $ liftA2 (<*>) (view mf) (view mx)

instance (Functor m, Monad m) => Monad (BatchT r m) where
    return = lift . return
    m >>= f = BatchT $ view m >>= bindView f

instance MonadTrans (BatchT r) where
    lift = BatchT . (>>= return . Pure)

data View r m a where
    Pure :: a -> View r m a
    More :: [r] -> ([Result r] -> BatchT r m a) -> View r m a

instance Functor m => Functor (View r m) where
    fmap f (Pure x) = Pure $ f x
    fmap f (More reqs k) = More reqs (fmap f . k)

instance Applicative m => Applicative (View r m) where
    pure = Pure
    Pure f     <*> mx         = f <$> mx
    mf         <*> Pure x     = ($ x) <$> mf
    More rf kf <*> More rx kx = More (rf ++ rx) $ \results ->
        let (resultsF, resultsX) = splitAt (length rf) results
        in kf resultsF <*> kx resultsX

bindView :: (Functor m, Monad m)
         => (a -> BatchT r m b) -> View r m a -> m (View r m b)
bindView f (Pure x) = view $ f x
bindView f (More reqs k) = return $ More reqs (k >=> f)

runBatchT :: (Applicative m, Monad m) => Handler r m -> BatchT r m a -> m a
runBatchT handle = view >=> eval
  where
    eval (Pure x) = return x
    eval (More reqs k) = handle reqs >>= runBatchT handle . k

runBatch handle = runIdentity . runBatchT handle
