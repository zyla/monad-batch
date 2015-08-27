{-# LANGUAGE TypeFamilies #-}
module BatchExample where

import Control.Applicative
import Data.Traversable
import Debug.Trace

import Control.Monad.Batch

data TestReq = MultiplyByTwo Int | AddOne Int

type instance Result TestReq = Int

handleTestReq :: Handler TestReq
handleTestReq reqs =
    -- Using Debug.Trace here for demonstration purposes; it will be replaced
    -- by putStrLn when BatchT will be implemented.
    trace ("Handling " ++ show (length reqs) ++ " requests at once") $
    -- Not an actual optimization; just a demonstration
    map runTestReq reqs

runTestReq :: TestReq -> Int
runTestReq (MultiplyByTwo x) = x * 2
runTestReq (AddOne x) = x + 1

example1Monad :: (Int, Int)
example1Monad = runBatch handleTestReq $ do
    x <- request $ MultiplyByTwo 5
    y <- request $ AddOne 10
    return (x, y)
    -- should output "Handling 1 requests at once" twice
    -- with ApplicativeDo should behave the same as example1Applicative

example1Applicative :: (Int, Int)
example1Applicative = runBatch handleTestReq $
    liftA2 (,) (request $ MultiplyByTwo 5) (request $ AddOne 10)
    -- should output "Handling 2 requests at once"

example2 :: [Int]
example2 = runBatch handleTestReq $
        traverse (request . MultiplyByTwo) [1..13]
    -- should output "Handling 13 requests at once"
