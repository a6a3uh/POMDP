module Dynamic where
    
import Data.MemoTrie (memo3)

-- $setup
-- >>>:set -XGeneralizedNewtypeDeriving
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import Control.Arrow
--
-- >>> newtype Small = Small Int deriving Show
-- >>> newtype Positive = Positive Int deriving Show
-- >>> newtype NonNegative = NonNegative Int deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 30) <$> arbitrary
-- >>> instance Arbitrary NonNegative where arbitrary = NonNegative . abs . (`mod` 30) <$> arbitrary
-- >>> instance Arbitrary Positive where arbitrary = Positive . (1+) . abs . (`mod` 30) <$> arbitrary
--

type Pos = (Int, Int)   -- ^ position is a pir of Int's

-- | gives (Q, V) for moving from curretn point to arbitraty point
--
dynamic :: Pos          -- ^ current position
        -> Pos          -- ^ target position
        -> ([Int], Int) -- ^ pair (Q V)
dynamic (x0, y0) (x, y) = dynamic0 (x - x0) (y - y0)

-- | gives stabilized (Q, V) pair
--
-- This property says that stable V == min stable Q (not holds for x == y == 0)
-- prop> \(Positive x) (Positive y) -> True == (uncurry (==) $ minimum *** id $ dynamic0 x y)
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
-- prop> \(NonNegative n) (Small x) (Small y) -> fqmem x y n == fq x y n
fqmem :: Int -> Int -> Int -> [Int]
fqmem = memo3 fq

-- | memoized version of fv
--
-- prop> \(NonNegative n) (Small x) (Small y) -> fvmem n x y == fv n x y
fvmem :: Int -> Int -> Int -> Int
fvmem = memo3 fv

-- | calculates Q 
--
-- prop> fq x y 0 == [0,0,0,0]
--
-- This property says V always <= min Q. Not holds for n == 0
-- prop> \(Positive n) (Small x) (Small y) -> (minimum $ fq x y n) >= fv n x y
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
-- prop> \(NonNegative n) (Small x) (Small y) -> fv n x y == fv n y x
--
-- V is symmetric to changing sign of x and/or y
-- prop> \(NonNegative n) (Small x) (Small y) -> fv n x y == fv n (-x) (-y)
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