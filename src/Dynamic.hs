module Dynamic where
    
import Data.MemoTrie (memo3)

-- $setup
-- >>>:set -XGeneralizedNewtypeDeriving
-- >>> import Test.QuickCheck
-- >>> import Control.Monad
-- >>> import Control.Arrow
--
-- >>> newtype TInt = TInt {getInt :: Int} deriving (Eq, Ord, Show, Num, Integral, Real, Enum)
--
-- >>> instance Arbitrary TInt where arbitrary = liftM TInt (choose (1, 30) :: Gen Int) 
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
-- This property says that stable V == min stable Q
-- prop> True == (uncurry (==) $ minimum *** id $ dynamic0 (getInt x) (getInt y))
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
-- prop> fqmem (getInt x) (getInt y) (getInt n) == fq (getInt x) (getInt y) (getInt n)
fqmem :: Int -> Int -> Int -> [Int]
fqmem = memo3 fq

-- | memoized version of fv
--
-- prop> fvmem (getInt n) (getInt x) (getInt y) == fv (getInt n) (getInt x) (getInt y)
fvmem :: Int -> Int -> Int -> Int
fvmem = memo3 fv

-- | calculates Q 
--
-- prop> fq x y 0 == [0,0,0,0]
-- prop> (minimum $ fq (getInt x) (getInt y) (getInt n)) >= fv (getInt n) (getInt x) (getInt y)
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
-- prop> fv (getInt n) (getInt x) (getInt y) == fv (getInt n) (getInt y) (getInt x)
--
-- V is symmetric to changing sign of x and/or y
-- prop> fv (getInt n) (getInt x) (getInt y) == fv (getInt n) (getInt (-x)) (getInt y)
-- prop> fv (getInt n) (getInt x) (getInt y) == fv (getInt n) (getInt x) (getInt (-y))
-- prop> fv (getInt n) (getInt x) (getInt y) == fv (getInt n) (getInt (-x)) (getInt (-y))
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