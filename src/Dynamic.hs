module Dynamic where
    
import Data.MemoTrie (memo3)

-- $setup
-- >>>:set -XScopedTypeVariables
--
-- >>> import Test.QuickCheck
-- >>> import Control.Arrow
-- >>> import Data.List
--
-- >>> newtype Bounded = Bounded Int deriving Show
-- >>> instance Arbitrary Bounded where arbitrary = Bounded . (`mod` 30) <$> arbitrary
-- >>> newtype BoundedPositive = BoundedPositive Int deriving Show
-- >>> instance Arbitrary BoundedPositive where arbitrary = BoundedPositive . (`mod` 30) . (1+) . abs <$> arbitrary

type Pos = (Int, Int)   -- ^ position is a pir of Int's

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
        -> ([Int], Int) -- ^ pair (Q V)
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
         -> ([Int], Int)     -- ^ return pair (Q, V)
dynamic0 x y = lastUnique $ map (qv x y) [1 ..]
    where lastUnique (x0:x1:xs) | x0 == x1  = x0 
                                | otherwise = lastUnique (x1:xs)
          lastUnique _          = undefined     -- this never happens just to get rid of compiler warnings

-- | gives pair of memoized Q and V
--
qv :: Int -> Int -> Int -> ([Int], Int)
qv x y n = (fqmem x y n, fvmem n x y)

-- | memoized version of fq
-- 
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> fqmem x y n == fq x y n
fqmem :: Int -> Int -> Int -> [Int]
fqmem = memo3 fq

-- | memoized version of fv
--
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> fvmem n x y == fv n x y
fvmem :: Int -> Int -> Int -> Int
fvmem = memo3 fv

-- | calculates Q 
--
-- prop> fq x y 0 == [0,0,0,0]
--
-- This property says V always <= min Q. Not holds for n == 0
-- prop> \(Positive n) (Bounded x) (Bounded y) -> (minimum $ fq x y n) >= fv n x y
fq  :: Int      -- ^ x coordinate 
    -> Int      -- ^ y coordinate
    -> Int      -- ^ step number
    -> [Int]    -- ^ returns list of 4 costs for each possible step
fq _ _ 0 = [0, 0, 0, 0]           -- Q at 0 step is 0 in all directions
fq x y n = (cost (x, y) +) . (uncurry $ fvmem n) <$> neighbours (x, y)

-- | calculates V
--
-- prop> fv n 0 0 == 0
-- prop> fv 0 x y == cost (x, y)
--
-- V is symmetric to substituting x and y
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> fv n x y == fv n y x
--
-- V is symmetric to changing sign of x and/or y
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> fv n x y == fv n (-x) (-y)
fv  :: Int  -- ^ step number 
    -> Int  -- ^ x coordinate 
    -> Int  -- ^ y coordinate
    -> Int  -- ^ returns cost of moving to (x, y)
fv _ 0 0 = 0                        -- V at (0, 0) is 0 at any atep
fv 0 x y = cost (x, y)              -- V at 0 step is a cost
fv n x y | y' > x' = fv n y' x'     -- function is symmetric to change x by y (due to properties of cost function)
         | x' > 100 = 1000000000    -- limits on the board size
         | otherwise = minimum $ fqmem x' y' (n - 1)
    where x' = abs x
          y' = abs y
          
-- | cost function
--
cost :: Pos -> Int
cost (x, y) = abs x + abs y

-- | gives 4 neighbour points
--
neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]