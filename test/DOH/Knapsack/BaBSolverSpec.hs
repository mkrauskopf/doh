module DOH.Knapsack.BaBSolverSpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit hiding (path)
import Test.Hspec
import DOH.Knapsack.BaBSolver (solve, debugSolve, Item)
import Control.Applicative((<$>))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BaB, simple problems, using" $ do
    it "zero items" $
      (0, []) @=? debugSolve 10 []

    it "one item unused" $ do
      let items = [ (1, 20) ]
      (1, [False]) @=? debugSolve 10 items

    it "one item used" $ do
      let items = [ (1, 2) ]
      (2, [True]) @=? debugSolve 10 items

    it "two items" $ do
      let items = [(1, 2), (3, 1)]
      (5, [False,True]) @=? debugSolve 2 items

    it "three items 1" $ do
      let items = [ (5, 4), (6, 5), (3, 2) ]
      (7, [True,True,False]) @=? debugSolve 9 items

    it "three items 2" $ do
      let items = [ (45, 5), (48, 8), (35, 3) ]
      (7, [True,False,True]) @=? debugSolve 10 items

  describe "BaB, harder problems, using" $ do
    -- TODO: far from optimal
    it "19 items" $ assertValue "test/data/ks_19_0" 11555 -- optimal: 12248
    it "30 items" $ assertValue "test/data/ks_30_0" 90000 -- optimal: 99798


assertValue :: String -> Double -> Assertion
assertValue path optVal = do
    (_, val) <- uncurry solve . processFile <$> readFile path
    optVal @=? val


-- | Parses sample knapsack file
processFile :: String -> (Integer, [Item])
processFile content = (cap, items)
  where ls = lines content
        cap = read . (!! 1) . words $ head ls
        items = map (tuplify2 . words) (tail ls)
        tuplify2 [v,w] = (read v, read w)
        tuplify2 _ = error "tuplify2 must be passed two-elements list"

