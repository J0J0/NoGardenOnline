-- | This module contains literal copies of some functions that
-- are only available in newer versions of the indicated packages.
module Backports ((!?), hoistMaybe) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Data.Array.IArray (IArray, Ix, bounds, inRange)
import Data.Array.Base (unsafeAt)
import GHC.Ix (unsafeIndex)


-- | Convert a 'Maybe' computation to 'MaybeT'.
-- 
-- Taken from @transformers ^>= 0.6@ (BSD-3-Clause:
-- https://hackage.haskell.org/package/transformers-0.6.0.0/src/LICENSE ).
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure


{-# INLINE (!?) #-}
-- | Returns 'Just' the element of an immutable array at the specified index,
-- or 'Nothing' if the index is out of bounds.
-- 
-- Taken from @array ^>= 0.5.6@ (BSD-3-Clause:
-- https://hackage.haskell.org/package/array-0.5.6.0/src/LICENSE ).
(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
(!?) arr i = let b = bounds arr in
             if inRange b i
             then Just $ unsafeAt arr $ unsafeIndex b i
             else Nothing
