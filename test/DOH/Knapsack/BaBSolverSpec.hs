module DOH.Knapsack.BaBSolverSpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit hiding (path)
import Test.Hspec
import DOH.Knapsack.BaBSolver (solve, debugSolve)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Branch and Bound algorithm knapsack using" $ do
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

