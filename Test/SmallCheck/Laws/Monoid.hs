{-# LANGUAGE CPP #-}
module Test.SmallCheck.Laws.Monoid
  (
  -- * Identity
    leftIdentity
  , rightIdentity
  -- * Associativity
  , associativity
  , associativitySum
  -- * 
  , mconcat
  ) where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (mconcat)
#else
import Data.Monoid (Monoid, mappend, mempty)
import Data.Traversable (sequenceA)
#endif
import Data.Monoid ((<>))
import qualified Data.Monoid as Monoid (mconcat)
import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Series)
import Test.SmallCheck.Series.Utils (zipLogic3)

-- * Identity

-- | Check the /left identity/ law holds for the given 'Monoid' 'Series':
--
-- @
-- 'mempty' '<>' x ≡ x
-- @
leftIdentity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
leftIdentity s = over s $ \x -> mempty <> x == x

-- | Check the /right identity/ law holds for the given 'Monoid' 'Series':
--
-- @
-- x '<>' 'mempty' ≡ x
-- @
rightIdentity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
rightIdentity s = over s $ \x -> x <> mempty == x

-- * Associativity

-- | Check the /associativity/ law holds for the given 'Monoid' 'Series':
--
-- @
-- x '<>' (y '<>' z) ≡ (x '<>' y) '<>' z
-- @
--
-- This uses the product of the 3 'Series', be aware of combinatorial explosion.
associativity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Series m a -> Series m a -> Property m
associativity xs ys zs =
    over xs $ \x ->
        over ys $ \y ->
            over zs $ \z ->
    x <> (y <> z) == (x <> y) <> z

-- | Check the /associativity/ law hold for the given 'Monoid' 'Series':
--
-- @
-- x '<>' (y '<>' z) ≡ (x '<>' y) '<>' z
-- @
--
-- This uses the sum of the 3 'Series'.
associativitySum
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Series m a -> Series m a -> Property m
associativitySum xs ys zs =
    over (zipLogic3 xs ys zs) $ \(x,y,z) ->
        x <> (y <> z) == (x <> y) <> z

-- | When implementing 'mconcat' yourself this law must hold:
--
-- @
-- 'mconcat' ≡ 'foldr' 'mappend' 'mempty'
-- @
mconcat
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
mconcat s = over (sequenceA $ replicate 3 s) $ \l ->
    Monoid.mconcat l == foldr mappend mempty l
