module Markov where

import Control.Monad.Reader
import Dynamic

-- $setup
-- >>>:set -XScopedTypeVariables
--
-- >>> import Test.QuickCheck
-- >>> import Control.Arrow
--
-- >>> newtype Bounded = Bounded Int deriving Show
-- >>> instance Arbitrary Bounded where arbitrary = Bounded . (`mod` 4) <$> arbitrary
-- >>> newtype Command = Command Int deriving Show
-- >>> instance Arbitrary Command where arbitrary = Command . abs . (`mod` 4) <$> arbitrary
--
-- >> type PosProb = ([Pos], [Double])
-- >> instance Arbitrary PosProb where arbitrary = (arbitrary, arbitrary)
--
-- >>> type BoundedPos = (Bounded, Bounded)
-- >>> b2p (Bounded x, Bounded y) = (x, y)
--



-- | combines many targets with their probabilities to give 4 cost values in current position
-- 
-- ---In the setup below moving right (!!1) should be prefferable to moving up (!!3)
-- --->>> uncurry (<) $ (!! 1) &&& (!! 3) $ runReader (markovOut (0,0) [(1,0), (0,3)] [0.5, 0.5]) cost
-- ---True
-- this is not ture actually
markovOut   :: Pos      -- ^ initial position
            -> [Pos]    -- ^ list of targets positions
            -> [Double] -- ^ list of probabilities (should sum to 1) for each target (lengths should be the same)
            -> Reader Cost [Double] -- ^ 4 cost values for each of 4 directions
markovOut x0 xs pr = do
    costs <- traverse (liftM fst . dynamic x0) xs
    let f p cs = (p *) <$> cs
        pcosts = zipWith f pr costs
    return $ foldl (zipWith (+)) [0.0,0.0,0.0,0.0] pcosts

-- | 'bayesPrior' apriory probability of command for particular target
-- P (a | x, y, n) = exp (V (x, y, n) - Q (x, y, a, n))
-- as V == min Q this will be: exp (-inf, 0] -> (0, 1]
-- I believe this 'exp' is just a dirty hack. It just produces plausible values
--
-- Probability is less than 1
-- prop> \(p0::BoundedPos) (p1::BoundedPos) -> runReader (bayesPrior 3 (b2p p0) (b2p p1)) cost <= 1.0
bayesPrior  :: Int      -- ^ command
            -> Pos      -- ^ current position
            -> Pos      -- ^ target position
            -> Reader Cost Double   -- ^ apriory probability of user choosing command in current situation
bayesPrior a p0 p = do
    (q, v) <- dynamic p0 p
    return $ exp $ v - (q !! a)


-- | 'bayesPosterior' posteriory probability of particular target given command a
-- p (a | x, y, n) = P (a | x, y, n) / Sum (P (a | x, y, ...))
--
-- Probability is less than 1
-- prop> \(Command a) (p::BoundedPos) (NonEmpty (ps::[BoundedPos])) (Positive n) -> runReader (bayesPosterior a (b2p p) (b2p <$> ps) (n `mod` length ps)) cost <= 1.0 
bayesPosterior  :: Int      -- ^ command
                -> Pos      -- ^ current position
                -> [Pos]    -- ^ target positions
                -> Int      -- ^ target index
                -> Reader Cost Double   -- ^ posteriory probability after user issued some command
bayesPosterior a p0 ps n = do
    let prob = bayesPrior a p0
    
    p <- prob (ps !! n) 
    ps <- liftM sum (traverse prob ps)
    return $ p / ps

-- | This function recalculates probability of target when command from user arrives
--
-- Probability is less than 1
-- prop> \(Bounded x) (Bounded y) (Bounded x1) (Bounded x2) (Bounded y1) (Bounded y2) -> runReader (markovInEach (x, y) [(x1,y1),(x2,y2)] [0.75,0.25] 0 0) cost <= 1.0
-- prop \(p::BoundedPos) (NonEmpty (ps::[BoundedPos])) (NonEmpty (pr::[Positive Int])) (Positive n) (Command a) -> 
--          markovInEach (b2p p) (b2p <$> ps) (((/ (fromIntegral.sum) (getPositive<$>pr)).fromIntegral.getPositive) <$> pr) (n `mod` length ps) a
markovInEach    :: Pos      -- ^ current position
                -> [Pos]    -- ^ positons of targets
                -> [Double] -- ^ probabilities of targets before user command
                -> Int      -- ^ index of target
                -> Int      -- ^ command issued by user
                -> Reader Cost Double   -- ^ new probability of target
markovInEach p ps pr n a =  do
    let bayes = bayesPosterior a p ps
    bayeses <- sequence $ bayes <$> [0 .. length pr - 1]
    p <- liftM (* (pr !! n)) (bayes n) 
    let ps = (sum . zipWith (*) pr) bayeses
    return $ p / ps

-- | Updates all probabilities on user's input
-- 
-- All probabilities should sum to 1
-- prop> \(Bounded x) (Bounded y) (Bounded x1) (Bounded x2) (Bounded y1) (Bounded y2) -> abs (sum (runReader (markovIn (x, y) [(x1,y1),(x2,y2)] [0.75,0.25] 1) cost)- 1.0) < 1e-2
markovIn    :: Pos      -- ^ current position
            -> [Pos]    -- ^ positons of targets
            -> [Double] -- ^ probabilities of targets before user command
            -> Int      -- ^ command issued by user
            -> Reader Cost [Double] -- ^ new probability of target
markovIn p ps pr a = do
    let markovInEach' n = markovInEach p ps pr n a
    sequence $ markovInEach' <$> [0 .. length pr - 1]
    