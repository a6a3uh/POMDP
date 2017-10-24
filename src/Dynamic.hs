{-# LANGUAGE  FlexibleContexts
            , ConstraintKinds #-}

module Dynamic where
    
-- import Data.MemoTrie (memo3)
import Control.Monad.Reader
import Control.Monad.Memo

-- $setup
-- >>>:set -XScopedTypeVariables
--
-- >>> import Test.QuickCheck
-- >>> import Control.Arrow
-- >>> import Data.List
--
-- >>> newtype Bounded = Bounded Int deriving Show
-- >>> instance Arbitrary Bounded where arbitrary = Bounded . (`mod` 3) <$> arbitrary
-- >>> newtype BoundedPositive = BoundedPositive Int deriving Show
-- >>> instance Arbitrary BoundedPositive where arbitrary = BoundedPositive  . succ . (`mod` 2) . abs <$> arbitrary

type Pos n = (n, n)   -- ^ position is a pir of Int's
type Cost n r = (n, n) -> r
type Dynamic n r m = (Integral n, Floating r, Ord r, MonadReader (Cost n r) m, MonadMemo (n, n, n) [r] m)
-- type MemoQ = MemoT (Int, Int, Int) [Double]

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
dynamic :: Dynamic n r m--(Integral n, Real r, MonadReader Cost m, MonadMemo (n, n, n) [r] m)
        => Pos n          -- ^ current position
        -> Pos n          -- ^ target position
        -> m ([r], r) -- ^ pair (Q V)
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
dynamic0 :: Dynamic n r m --(Integral n, Real r, MonadReader Cost m, MonadMemo (n, n, n) [r] m)
         => n              -- ^ x coordinate
         -> n              -- ^ y coordinate
         -> m ([r], r)     -- ^ return pair (Q, V)
dynamic0 x y = do
    qv <- traverse (qv x y) [1 ..]
    return $ lastUnique qv
    where lastUnique (x0:x1:xs) | x0 == x1  = x0 
                                | otherwise = lastUnique (x1:xs)
          lastUnique _          = undefined     -- this never happens just to get rid of compiler warnings

-- | gives pair of memoized Q and V
--
qv  :: Dynamic n r m --(Integral n, Real r, MonadReader Cost m, MonadMemo (n, n, n) [r] m)
    => n 
    -> n 
    -> n 
    -> m ([r], r)
qv x y n = do
    q <- fq x y n
    v <- fv n x y
    return (q, v)

-- -- | memoized version of fq
-- -- 
-- -- prop> \(NonNegative n) (Bounded x) (Bounded y) -> runReader (fqmem x y n) cost == runReader (fq x y n) cost
-- fqmem :: MonadReader Cost m => Int -> Int -> Int -> m [Double]
-- fqmem = memo3 fq

-- -- | memoized version of fv
-- --
-- -- prop> \(NonNegative n) (Bounded x) (Bounded y) -> runReader (fvmem n x y) cost == runReader (fv n x y) cost
-- fvmem :: MonadReader Cost m => Int -> Int -> Int -> m Double
-- fvmem = memo3 fv

-- | calculates Q 
--
-- prop> runReader (fq x y 0) cost == [0,0,0,0]
--
-- This property says V always <= min Q. Not holds for n == 0
-- prop> \(Positive n) (Bounded x) (Bounded y) -> (minimum $ runReader (fq x y n) cost) >= runReader (fv n x y) cost
fq  :: Dynamic n r m --(Integral n, Real r, MonadReader Cost m, MonadMemo (n, n, n) [r] m)
    => n          -- ^ x coordinate 
    -> n          -- ^ y coordinate
    -> n          -- ^ step number
    -> m [r]
fq _ _ 0 = return [0, 0, 0, 0]           -- Q at 0 step is 0 in all directions
fq x y n = do
    c <- ask
    let v = fv n
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
fv  :: Dynamic n r m --(Integral n, Real r, MonadReader Cost m, MonadMemo (n, n, n) [r] m)
    => n --Int      -- ^ step number 
    -> n --Int      -- ^ x coordinate 
    -> n --Int      -- ^ y coordinate
    -> m r
fv _ 0 0 = do
    c <- ask
    return $ c (0, 0)               -- V at (0, 0) is 0 at any atep
fv 0 x y = do
    c <- ask
    return $ c (x, y)               -- V at 0 step is a cost
fv n x y | y' > x' = fv n y' x'     -- function is symmetric to change x by y (due to properties of cost function)
         | x' > 100 = return 1000000000    -- limits on the board size
         | otherwise = liftM minimum $ for3 memo fq x' y' (n - 1)
    where x' = abs x
          y' = abs y
          
-- | cost function
--
cost :: (Integral n, Floating r) => Cost n r
cost (x, y) = 1 / (1 + exp (-s)) -- if s < 5 then s else 5
    where s = fromIntegral $ abs x + abs y

-- | gives 4 neighbour points
--
neighbours :: Integral n => Pos n -> [Pos n]
neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]