module Markov where

import Dynamic

-- $setup
-- >>>:set -XGeneralizedNewtypeDeriving
-- >>> import Test.QuickCheck
-- >>> import Control.Monad
--
-- >>> newtype TInt = TInt {getInt :: Int} deriving (Eq, Ord, Show, Num, Integral, Real, Enum)
--
-- >>> instance Arbitrary TInt where arbitrary = liftM TInt (choose (1, 30) :: Gen Int) 
--

-- | combines many targets with their probabilities to give 4 cost values in current position
--
markovOut   :: Pos      -- ^ initial position
            -> [Pos]    -- ^ list of targets positions
            -> [Double] -- ^ list of probabilities (should sum to 1) for each target (lengths should be the same)
            -> [Double] -- ^ 4 cost values for each of 4 directions
markovOut x0 xs ps = foldl (zipWith (+)) [0,0,0,0] pcosts
    where   costs :: [[Int]]        -- ^ list of 4 element lists of costs for moving in each direction
            costs = fst . dynamic x0 <$> xs
            pcosts :: [[Double]]    -- ^ same costs but taking into account the probabilities of each target
            pcosts = let f p cs = (p *) . fromIntegral <$> cs 
                     in zipWith f ps costs

-- | 'bayesPrior' apriory probability of command for particular target
-- P (a | x, y, n) = exp (V (x, y, n) - Q (x, y, a, n))
-- as V == min Q this will be: exp (-inf, 0] -> (0, 1]
-- I believe this 'exp' is just a dirty hack. It just produces plausible values
--
-- Probability is less than 1
-- prop> bayesPrior 3 (getInt x, getInt y) (getInt x1, getInt y1) <= 1.0
bayesPrior  :: Int      -- ^ command
            -> Pos      -- ^ current position
            -> Pos      -- ^ target position
            -> Double   -- ^ apriory probability of user choosing command in current situation
bayesPrior a p0 p = exp . fromIntegral $ v - (q !! a)
    where (q, v) = dynamic p0 p


-- | 'bayesPosterior' posteriory probability of particular target given command a
-- p (a | x, y, n) = P (a | x, y, n) / Sum (P (a | x, y, ...))
--
-- Probability is less than 1
-- prop> bayesPosterior 3 (getInt x, getInt y) [(getInt x1, getInt y1),(getInt x2, getInt y2)] 1 <= 1.0
bayesPosterior  :: Int      -- ^ command
                -> Pos      -- ^ current position
                -> [Pos]    -- ^ target positions
                -> Int      -- ^ target index
                -> Double   -- ^ posteriory probzbility after user issued some command
bayesPosterior a p0 ps n = let prob = bayesPrior a p0
                    in prob (ps !! n) / sum (prob <$> ps)

-- | This function recalculates probability of target when command from user arrives
--
-- Probability is less than 1
-- prop> markovInEach (getInt x, getInt y) [(x1,y1),(x2,y2)] [0.75,0.25] 0 0 <= 1.0
markovInEach    :: Pos      -- ^ current position
                -> [Pos]    -- ^ positons of targets
                -> [Double] -- ^ probabilities of targets before user command
                -> Int      -- ^ index of target
                -> Int      -- ^ command issued by user
                -> Double   -- ^ new probability of target
markovInEach p ps pr n a =  bayes n * (pr !! n) / sum (zipWith (*) bayeses pr)
                    where bayes = bayesPosterior a p ps 
                          bayeses = fmap bayes [0 .. length pr - 1]

-- | Updates all probabilities on user's input
-- 
-- All probabilities should sum to 1
-- prop> abs (sum (markovIn (getInt x, getInt y) [(x1,y1),(x2,y2)] [0.75,0.25] 1) - 1.0) < 1e-6
markovIn    :: Pos      -- ^ current position
            -> [Pos]    -- ^ positons of targets
            -> [Double] -- ^ probabilities of targets before user command
            -> Int      -- ^ command issued by user
            -> [Double] -- ^ new probability of target
markovIn p ps pr a = markovInEach' <$> [0 .. length pr - 1]
    where markovInEach' n = markovInEach p ps pr n a
