module Dynamic where
    
-- import Data.MemoTrie (memo3)
import Control.Monad.Memo
import Control.Monad.Identity

-- $setup
-- >>>:set -XScopedTypeVariables
--
-- >>> import Test.QuickCheck
-- >>> import Control.Arrow
-- >>> import Data.List
--
-- >>> newtype Bounded = Bounded Int deriving Show
-- >>> instance Arbitrary Bounded where arbitrary = Bounded . (`mod` 5) <$> arbitrary
-- >>> newtype BoundedPositive = BoundedPositive Int deriving Show
-- >>> instance Arbitrary BoundedPositive where arbitrary = BoundedPositive  . succ . abs . (`mod` 5) <$> arbitrary

type Pos = (Int, Int)   -- ^ position is a pir of Int's

type MemoQ = MemoT (Int, Int, Int) [Double]
type MemoV = MemoT (Int, Int, Int) Double
type MemoQV = MemoQ (MemoV Identity)

-- | gives (Q, V) for moving from curretn point to arbitraty point
--
-- In the setup below moving up is a best choise
-- >>> let costs = fst (dynamic (0, 0) (0, 3)) in elemIndex (minimum costs) costs
-- Just 3
--
-- In the setup below moving right is a best choise
-- >>> let costs = fst (dynamic (0, 0) (3, 0)) in elemIndex (minimum costs) costs
-- Just 1
--
-- In the setup below moving right and up has the same cost (the same for down and left). And moving left is worse than moving right
-- >>> uncurry (==) $ (!!1) &&& (!!3) $ fst (dynamic (0, 0) (3, 3))
-- True
-- >>> uncurry (==) $ (!!0) &&& (!!2) $ fst (dynamic (0, 0) (3, 3))
-- True
-- >>> uncurry (>) $ (!!0) &&& (!!1) $ fst (dynamic (0, 0) (3, 3))
-- True
dynamic :: Pos          -- ^ current position
        -> Pos          -- ^ target position
        -> ([Double], Double) -- ^ pair (Q V)
dynamic (x0, y0) (x, y) = dynamic0 (x0 - x) (y0 - y)

-- | gives stabilized (Q, V) pair
--
-- This property says that stable V == min stable Q (not holds for x == y == 0)
-- prop> \(BoundedPositive x) (BoundedPositive y) -> True == (uncurry (==) $ minimum *** id $ dynamic0 x y)
--
-- In the setup below moving left is a best choise
-- >>> let costs = fst (dynamic0 3 0) in elemIndex (minimum costs) costs
-- Just 0
--
-- In the setup below moving down is a best choise
-- >>> let costs = fst (dynamic0 0 3) in elemIndex (minimum costs) costs
-- Just 2
dynamic0 :: Int              -- ^ x coordinate
         -> Int              -- ^ y coordinate
         -> ([Double], Double)     -- ^ return pair (Q, V)
dynamic0 x y = lastUnique $ map (qv x y) [1 ..]
    where lastUnique (x0:x1:xs) | x0 == x1  = x0 
                                | otherwise = lastUnique (x1:xs)
          lastUnique _          = undefined     -- this never happens just to get rid of compiler warnings

-- | gives pair of memoized Q and V
--
qv :: Int -> Int -> Int -> ([Double], Double)
qv x y n = (evalQ x y n, evalV n x y)

-- | calculates Q 
--
-- prop> evalQ x y 0 == [0,0,0,0]
--
-- This property says V always <= min Q. Not holds for n == 0
-- prop> \(Positive n) (Bounded x) (Bounded y) -> (minimum $ evalQ x y n) >= evalV n x y
evalQ :: Int -> Int -> Int -> [Double]
evalQ x y n = startEvalMemo . startEvalMemoT $ fqmon  x y n

-- | calculates V
--
-- prop> evalV n 0 0 == cost (0, 0)
-- prop> evalV 0 x y == cost (x, y)
--
-- V is symmetric to substituting x and y
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> evalV n x y == evalV n y x
--
-- V is symmetric to changing sign of x and/or y
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> evalV n x y == evalV n (-x) (-y)
evalV :: Int -> Int -> Int -> Double
evalV n x y = startEvalMemo . startEvalMemoT $ fvmon  n x y

fqmon :: Int -> Int -> Int -> MemoQV [Double]
fqmon _ _ 0 = return [0,0,0,0]
fqmon x y n = do
    let pts = neighbours (x, y)
        v = for3 memol1 fvmon n
        c = cost (x, y)
        q = fmap (c +) . uncurry v
    traverse q pts

fvmon :: Int -> Int -> Int -> MemoQV Double
fvmon _ 0 0 = return $ cost (0, 0)
fvmon 0 x y = return $ cost (x, y)
fvmon n x y | limit     = return 1000000000
            | otherwise = liftM minimum $ for3 memol0 fqmon x' y' (n - 1)
            where x' = abs x
                  y' = abs y
                  limit = x' > 25 || y' > 25
          
-- | cost function
--
cost :: Pos -> Double
cost (x, y) = 1 / (1 + exp (-s)) -- if s < 5 then s else 5
    where s = fromIntegral $ abs x + abs y

-- | gives 4 neighbour points
--
neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]