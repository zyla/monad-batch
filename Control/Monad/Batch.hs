{-# LANGUAGE GADTs, TypeFamilies, DeriveFunctor #-}
-- | A data type for computations with requests that can be batched together
-- and possibly executed more efficiently.
--
-- Inspired by Facebook's <https://github.com/facebook/Haxl Haxl>, but the implementation is pure (no IO).
--
-- Things to improve:
--
--  * encode the first Handler law in the type (like the Cartesian Store
--    Comonad from Multiplate paper)
--
--  * support multiple response types (request type indexed by response type)
--
module Control.Monad.Batch (
  -- * The monad
    BatchT

  -- * Usage
  -- | To use 'BatchT' effectively, you have to provide several things:
  --
  --  * a request type @req@,
  --  * a response type @'Result' req@,
  --  * a handler function of type @'Handler' req m@, running in monad @m@.
  --
  , Result
  , Handler

  -- ** Making requests
  -- | The 'BatchT' monad transformer adds one special operation to the underlying monad:
  , request

  -- ** Running computations
  , runBatchT

  -- ** Pure usage
  , Batch
  , runBatch
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans (MonadTrans, lift)
import Data.List (splitAt)

-- | Mapping from request type to result type.
-- You have to provide an instance for your request type.
type family Result req :: *

-- | Handler function for batched requests.
-- Functions @handler :: Handler req m@ should satisfy the following laws:
--
-- > pure (length xs) = length (handler xs)
-- > pure handler (xs ++ ys) = liftA2 (++) (handler xs) (handler ys)
--
type Handler req m = [req] -> m [Result req]

-- | The BatchT monad transformer.
--
-- Applicative combinator @'<*>'@ will batch 'request's together.
newtype BatchT r (m :: * -> *) a = BatchT { view :: m (View r m a) } deriving (Functor)

-- Naming conventions:
--
--  * m - monadic values
--  * k - continuations
--  * f - pure functions, monadic functions (when using >>=)
--  * x - values applied to functions
--  * r - request lists

instance Applicative m => Applicative (BatchT r m) where
    pure = BatchT . pure . Pure
    mf <*> mx = BatchT $ liftA2 (<*>) (view mf) (view mx)

instance Monad m => Monad (BatchT r m) where
    return = lift . return
    m >>= f = BatchT $ view m >>= bindView f
      where
        -- Plumbing required to traverse all the monads.
        bindView f (Pure x) = view $ f x
        bindView f (More reqs k) = return $ More reqs (k >=> f)

instance MonadTrans (BatchT r) where
    lift = BatchT . (>>= return . Pure)

-- | Internal type, representing state of the computation.
data View r m a where
    -- Finished with a value.
    Pure :: a -> View r m a
    -- Blocked on some requests. The continuation is in @BatchT r m@ monad,
    -- so after handling requests it may issue another batch.
    More :: [r] -> ([Result r] -> BatchT r m a) -> View r m a

instance Functor m => Functor (View r m) where
    -- can't set @fmap f x = pure f <*> x@, because fmap itself is used in (<*>).
    fmap f (Pure x) = Pure $ f x
    fmap f (More reqs k) = More reqs (fmap f . k)

instance Applicative m => Applicative (View r m) where
    pure = Pure
    Pure f     <*> mx         = f <$> mx
    mf         <*> Pure x     = ($ x) <$> mf
    -- Actual magic happens here.
    More rf kf <*> More rx kx = More (rf ++ rx) $ \results ->
        let (resultsF, resultsX) = splitAt (length rf) results
        in kf resultsF <*> kx resultsX

-- | Introduce a request into the computation.
-- When ran it will be passed to handler function for processing - possibly
-- with other requests.
request :: Applicative m => r -> BatchT r m (Result r)
request req = BatchT $ pure $ More [req] (pure . head)

-- | Run the computation.
runBatchT :: Monad m
          => Handler r m -- ^ function to handle requests
          -> BatchT r m a
          -> m a
runBatchT handle = view >=> eval
  where
    eval (Pure x) = return x
    eval (More reqs k) = handle reqs >>= runBatchT handle . k

-- | 'BatchT' specialized to 'Identity' monad.
type Batch r = BatchT r Identity

-- | Run computation in 'Identity' monad.
runBatch :: Handler r Identity -> Batch r a -> a
runBatch handle = runIdentity . runBatchT handle
