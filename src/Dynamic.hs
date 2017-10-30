{-# LANGUAGE  FlexibleContexts
            , ConstraintKinds 
            , TemplateHaskell #-}

module Dynamic where
    
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Memo
import Control.Lens

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
-- >>> env = DynamicEnv {_cost = costLogistic, _lim = 10, _logQ = False, _maxSteps = 5}
-- >>> eval = fst . fst . startRunMemo . startRunMemoT . fmap fst . runWriterT . flip runReaderT env

type Pos n = (n, n)   -- ^ position is a pair
type Cost n r = Pos n -> r
data DynamicEnv n r = DynamicEnv 
    { _dynamicCost :: Cost n r
    , _dynamicLim :: n
    , _dynamicLog :: Bool
    , _dynamicMaxSteps :: n
    }
type MemoQ n r = MemoT (n, n, n) [r]
type MemoV n r = MemoT (n, n, n) r
type MemoQV n r = MemoQ n r (MemoV n r Identity)
type DynamicConext n r m = ReaderT (DynamicEnv n r) (WriterT String m)
type DynamicConstraint n r = (Integral n, Floating r, Ord r, Show n)--, MonadReader (DynamicEnv n r) m, MonadWriter String m, MonadMemo (n, n, n) k m)
type Dynamic n r a = DynamicConext n r (MemoQV n r) a
-- type Dynamic n r a = ReaderT (DynamicEnv n r) (WriterT String (MemoQV n r)) a


makeLenses ''DynamicEnv

-- | gives (Q, V) for moving from curretn point to arbitraty point
--
-- In the setup below moving up is a best choise
-- >>> let costs = fst (eval (dynamic (0, 0) (0, 3))) in elemIndex (minimum costs) costs
-- Just 3
--
-- In the setup below moving right is a best choise
-- >>> let costs = fst (eval (dynamic (0, 0) (3, 0))) in elemIndex (minimum costs) costs
-- Just 1
--
-- In the setup below moving right and up has the same cost (the same for down and left). And moving left is worse than moving right
-- >>> uncurry (==) $ (!!1) &&& (!!3) $ fst (eval (dynamic (0, 0) (3, 3)))
-- True
-- >>> uncurry (==) $ (!!0) &&& (!!2) $ fst (eval (dynamic (0, 0) (3, 3)))
-- True
-- >>> uncurry (>) $ (!!0) &&& (!!1) $ fst (eval (dynamic (0, 0) (3, 3)))
-- True
dynamic :: DynamicConstraint n r
        => Pos n        -- ^ current position
        -> Pos n        -- ^ target position
        -> Dynamic n r ([r], r)   -- ^ pair (Q V)
dynamic (x0, y0) (x, y) = dynamic0 (x0 - x) (y0 - y)

-- | gives stabilized (Q, V) pair
--
-- This property says that stable V == min stable Q (not holds for x == y == 0)
-- prop> \(BoundedPositive x) (BoundedPositive y) -> True == (uncurry (==) $ minimum *** id $ eval (dynamic0 x y))
--
-- In the setup below moving left is a best choise
-- >>> let costs = fst (eval (dynamic0 3 0)) in elemIndex (minimum costs)
-- Just 0
--
-- In the setup below moving down is a best choise
-- >>> let costs = fst (eval (dynamic0 0 3)) in elemIndex (minimum costs)
-- Just 2
dynamic0 :: DynamicConstraint n r
         => n              -- ^ x coordinate
         -> n              -- ^ y coordinate
         -> Dynamic n r ([r], r)     -- ^ return pair (Q, V)
dynamic0 x y = do
    e <- ask
    let qvs = map (qv x y) $ if e ^. dynamicMaxSteps == 0 then [1 ..] else [1 .. e ^. dynamicMaxSteps]
    lastUnique qvs
        where   lastUnique (x':[]) = x'
                lastUnique (x':xs) = do x0 <- x'
                                        x1 <- xs !! 0
                                        if x0 == x1 then return x0 else lastUnique xs

-- | gives pair of memoized Q and V
--
qv  :: DynamicConstraint n r
    => n 
    -> n 
    -> n 
    -> Dynamic n r ([r], r)
qv x y n = do
    q <- fq x y n
    v <- fv n x y
    return (q, v)

-- | calculates Q 
--
-- prop> eval (fq x y 0) == [0,0,0,0]
--
-- This property says V always <= min Q. Not holds for n == 0
-- prop> \(Positive n) (Bounded x) (Bounded y) -> (minimum $ eval (fq x y n)) >= eval (fv n x y)
fq  :: DynamicConstraint n r
    => n          -- ^ x coordinate 
    -> n          -- ^ y coordinate
    -> n          -- ^ step number
    -> Dynamic n r [r]
fq _ _ 0 = return [0, 0, 0, 0]           -- Q at 0 step is 0 in all directions
fq x y n = do
    e <- ask
    when (e ^. dynamicLog) (tell $ show (x, y, n) ++ "; ")
    let v = for3 memol3 fv n
        q = fmap ((e ^. dynamicCost) (x, y) +) . uncurry v
        pts = neighbours (x, y)
    traverse q pts

-- | calculates V
--
-- prop> eval (fv n 0 0) == costLogistic (0, 0)
-- prop> eval (fv 0 x y) == costLogistic (x, y)
--
-- V is symmetric to substituting x and y
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> eval (fv n x y) == eval (fv n y x)
--
-- V is symmetric to changing sign of x and/or y
-- prop> \(NonNegative n) (Bounded x) (Bounded y) -> eval (fv n x y) == eval (fv n (-x) (-y))
fv  :: DynamicConstraint n r
    => n      -- ^ step number 
    -> n      -- ^ x coordinate 
    -> n      -- ^ y coordinate
    -> Dynamic n r r
fv _ 0 0 = do
    e <- ask
    return $ (e ^. dynamicCost) (0, 0)               -- V at (0, 0) is 0 at any atep
fv 0 x y = do
    e <- ask
    return $ (e ^. dynamicCost) (x, y)               -- V at 0 step is a cost
fv n x y = do
    e <- ask
    let fv' n' x' y'    | y' > x'                   = fv n' y' x'     -- function is symmetric to change x by y (due to properties of cost function)
                        | x' > (e ^. dynamicLim)    = return 1000000000    -- limits on the board size
                        | otherwise                 = liftM minimum $ for3 memol2 fq x' y' (n' - 1)
    fv' n (abs x) (abs y)
          
-- | cost function
--
costLogistic :: (Integral n, Floating r) => Cost n r
costLogistic (x, y) = 1 / (1 + exp (-s)) -- if s < 5 then s else 5
    where s = fromIntegral $ abs x + abs y

-- | gives 4 neighbour points
--
neighbours :: Integral n => Pos n -> [Pos n]
neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
