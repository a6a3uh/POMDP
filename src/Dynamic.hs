module Dynamic where
    
import Data.MemoTrie (memo3)
import Control.Monad.Reader

-- $setup
-- >>>:set -XScopedTypeVariables
--
-- >>> import Test.QuickCheck
-- >>> import Control.Arrow
-- >>> import Data.List
--
-- >>> newtype Bounded = Bounded Int deriving Show
-- >>> instance Arbitrary Bounded where arbitrary = Bounded . (`mod` 4) <$> arbitrary
-- >>> newtype BoundedPositive = BoundedPositive Int deriving Show
-- >>> instance Arbitrary BoundedPositive where arbitrary = BoundedPositive  . succ . (`mod` 4) . abs <$> arbitrary

type Pos = (Int, Int)   -- ^ position is a pir of Int's
type Cost = Pos -> Double

-- | gives (Q, V) for moving from curretn point to arbitraty point
--
-- In the setup below moving up is a best choise
-- >>> let costs = fst (runReader (dynamic (0, 0) (0, 3)) cost) in elemIndex (minimum costs) costs
-- Just 3
--
-- In the setup below moving right is a best choise
-- >>> let costs = fst (runReader (dynamic (0, 0) (3, 0)) cost) in elemIndex (minimum costs) costs
-- Just 1
--
-- In the setup below moving right and up has the same cost (the same for down and left). And moving left is worse than moving right
-- >>> uncurry (==) $ (!!1) &&& (!!3) $ fst (runReader (dynamic (0, 0) (3, 3)) cost)
-- True
-- >>> uncurry (==) $ (!!0) &&& (!!2) $ fst (runReader (dynamic (0, 0) (3, 3)) cost)
-- True
-- >>> uncurry (>) $ (!!0) &&& (!!1) $ fst (runReader (dynamic (0, 0) (3, 3)) cost)
-- True
dynamic :: Pos          -- ^ current position
        -> Pos          -- ^ target position
        -> Reader Cost ([Double], Double) -- ^ pair (Q V)
dynamic (x0, y0) (x, y) = dynamic0 (x0 - x) (y0 - y)

-- | gives stabilized (Q, V) pair
--
-- This property says that stable V == min stable Q (not holds for x == y == 0)
-- prop> \(BoundedPositive x) (BoundedPositive y) -> True == (uncurry (==) $ minimum *** id $ runReader (dynamic0 x y) cost)
--
-- In the setup below moving left is a best choise
-- >>> let costs = fst (runReader (dynamic0 3 0) cost) in elemIndex (minimum costs) costs
-- Just 0
--
-- In the setup below moving down is a best choise
-- >>> let costs = fst (runReader (dynamic0 0 3) cost) in elemIndex (minimum costs) costs
-- Just 2
dynamic0 :: Int              -- ^ x coordinate
         -> Int              -- ^ y coordinate
         -> Reader Cost ([Double], Double)     -- ^ return pair (Q, V)
dynamic0 x y = do
    qv <- traverse (qv x y) [1 ..]
    return $ lastUnique qv
    where lastUnique (x0:x1:xs) | x0 == x1  = x0 
                                | otherwise = lastUnique (x1:xs)
          lastUnique _          = undefined     -- this never happens just to get rid of compiler warnings

-- | gives pair of memoized Q and V
--
qv :: Int -> Int -> Int -> Reader Cost ([Double], Double)
qv x y n = do
    q <- fqmem x y n
    v <- fvmem n x y
    return (q, v)

-- | memoized version of fq
-- 
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> runReader (fqmem x y n) cost == runReader (fq x y n) cost
fqmem :: Int -> Int -> Int -> Reader Cost [Double]
fqmem = memo3 fq

-- | memoized version of fv
--
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> runReader (fvmem n x y) cost == runReader (fv n x y) cost
fvmem :: Int -> Int -> Int -> Reader Cost Double
fvmem = memo3 fv

-- | calculates Q 
--
-- prop> runReader (fq x y 0) cost == [0,0,0,0]
--
-- This property says V always <= min Q. Not holds for n == 0
-- prop> \(Positive n) (Bounded x) (Bounded y) -> (minimum $ runReader (fq x y n) cost) >= runReader (fv n x y) cost
fq  :: Int          -- ^ x coordinate 
    -> Int          -- ^ y coordinate
    -> Int          -- ^ step number
    -> Reader Cost [Double]
fq _ _ 0 = return [0, 0, 0, 0]           -- Q at 0 step is 0 in all directions
fq x y n = do
    c <- ask
    let v = fvmem n
        q = fmap (c (x, y) +) . uncurry v
        pts = neighbours (x, y)
    traverse q pts

-- | calculates V
--
-- prop> runReader (fv n 0 0) cost == cost (0, 0)
-- prop> runReader (fv 0 x y) cost == cost (x, y)
--
-- V is symmetric to substituting x and y
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> runReader (fv n x y) == runReader (fv n y x)
--
-- V is symmetric to changing sign of x and/or y
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> runReader (fv n x y) cost == runReader (fv n (-x) (-y)) cost
fv  :: Int      -- ^ step number 
    -> Int      -- ^ x coordinate 
    -> Int      -- ^ y coordinate
    -> Reader Cost Double
fv _ 0 0 = do
    c <- ask
    return $ c (0, 0)               -- V at (0, 0) is 0 at any atep
fv 0 x y = do
    c<- ask
    return $ c (x, y)               -- V at 0 step is a cost
fv n x y | y' > x' = fv n y' x'     -- function is symmetric to change x by y (due to properties of cost function)
         | x' > 100 = return 1000000000    -- limits on the board size
         | otherwise = liftM minimum $ fqmem x' y' (n - 1)
    where x' = abs x
          y' = abs y
          
-- | cost function
--
cost :: Pos -> Double
cost (x, y) = 1 / (1 + exp (-s)) -- if s < 5 then s else 5
    where s = fromIntegral $ abs x + abs y

-- | gives 4 neighbour points
--
neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]