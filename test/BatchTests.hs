module BatchTests where

import Common

handleReq :: Handler (Req a) (Writer [[a]])
handleReq reqs = let results = map unReq reqs in tell [results] >> pure results

run = execWriter . runBatchT handleReq

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

prop_twoBatchesWithLift x y z w =
        run (pair (lift (return 0) >> req x >> req z) (req y >> req w))
    === [[x, y], [z, w]]

return []
runTests = $quickCheckAll
