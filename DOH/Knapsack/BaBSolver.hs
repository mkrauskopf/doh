-- | Branch and Bound knapsack algorithm with some build-in statistics.
--
-- /TODO/:
--
--  * possibility to restrict number of visited nodes which is going to be used
--    by large instances of the problem
--
--  * optimize by sorting input items
--
--  * linear relaxation
--

module DOH.Knapsack.BaBSolver (solve, Item, debugSolve) where

import Control.Monad.State.Lazy(State, runState, get, put)
import Control.Applicative((<*>), (<$>))
import Debug.Trace(trace)

-- | Describe knapsack configuration.
data Knapsack = Knapsack
  { kCapacity  :: Integer -- ^ Knapsack capacity.
  , kAvailable :: [Item]  -- ^ List of available items.
  , kValue     :: Double  -- ^ Knapsack value. Sum of values of all 'selected' items.
  , kSelection :: [Bool]  -- ^ sequence of nodes how they were visited. Used rather for debugging purposes.
  } | Empty deriving Show

data KState = KState
  { nOfNodes :: Integer          -- number of visited nodes
  , nodeSeq  :: [(Integer,Bool)] -- maximum estimate
  , maxEst   :: Double           -- maximum estimate
  } deriving Show

type Item = (Double, Integer) -- (value, weight)

solve :: Integer          -- ^ knapsack capacity
      -> [Item]           -- ^ all available items
      -> ([Bool], Double) -- ^ found solution
solve c allItems = (selectedItems resKs, kValue resKs)
  where
    (resKs, _) = runSolve c allItems

debugSolve :: Integer           -- knapsack capacity
           -> [Item]            -- all available items
           -> (Integer, [Bool]) -- found solution
debugSolve c allItems = trace ("Traversal: " ++ (show . nodeSeq $ resState))
                              (nOfNodes resState, selectedItems resKs)
  where
    (resKs, resState) = runSolve c allItems

selectedItems :: Knapsack -> [Bool]
selectedItems ks = (reverse . kSelection $ ks) ++ replicate (length $ kAvailable ks) False

runSolve :: Integer -- knapsack capacity
        -> [Item]  -- all available items
        -> (Knapsack, KState)
runSolve c allItems = runState (doSolve ks 0) (KState 0 [] (itemsValue allItems))
  where
    ks = Knapsack c allItems 0 []

itemsValue :: [Item] -> Double
itemsValue = sum . map fst

doSolve :: Knapsack              -- current knapsack configuration
        -> Integer               -- current tree level
        -> State KState Knapsack -- resulting state
doSolve ks level
  | null kAvailable'  = return ks
  | otherwise = do
      m <- maxEst <$> get
      if m < kValue'
         then return ks
         else if newRemCap < 0
                 then notUsed
                 else maxValue <$> used <*> notUsed
  where
    (Knapsack kCapacity' kAvailable' kValue' kSelection') = ks
    ((v,w):xs) = kAvailable'
    newRemCap = kCapacity' - w
    used :: State KState Knapsack
    used = do
      s <- get
      put $ s { nOfNodes = nOfNodes s + 1
              , nodeSeq = (level,True):nodeSeq s
              }
      doSolve (Knapsack newRemCap xs (kValue' + v) (True:kSelection')) (level+1)
    notUsed :: State KState Knapsack
    notUsed = do
      s <- get
      put $ s { nOfNodes = nOfNodes s + 1
              , maxEst = maxEst s - v
              , nodeSeq = (level,False):nodeSeq s
              }
      doSolve (Knapsack kCapacity' xs kValue' (False:kSelection')) (level+1)
    maxValue :: Knapsack -> Knapsack -> Knapsack
    maxValue ks1 ks2 = if kValue ks1 > kValue ks2 then ks1 else ks2

