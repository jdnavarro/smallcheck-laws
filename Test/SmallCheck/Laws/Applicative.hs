{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.SmallCheck.Laws.Applicative
  (
  -- * Applicative laws
    identity
  , composition
  , compositionSum
  , homomorphism
  , homomorphismSum
  , interchange
  , interchangeSum
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), (<*>), pure)
#endif
import Data.Proxy (Proxy)
import Data.Functor.Identity (Identity)
import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Serial, Series)
import Test.SmallCheck.Series.Utils (zipLogic, zipLogic3)

-- | Check the /identity/ law hold for the given 'Applicative' 'Series':
--
-- @
-- 'pure' id '<*>' v ≡ v
-- @
identity
  :: (Eq (f a), Monad m, Show (f a), Applicative f)
  => Series m (f a) -> Property m
identity xs = over xs $ \x -> (pure id <*> x) == x

-- | Check the /composition/ law hold for the given 'Applicative' 'Series':
--
-- @
-- '(.)' '<$>' u '<*>' v '<*>' w ≡  u '<*>' (v '<*>' w)
-- @
--
-- Exhaustive generation for the 'Series' of @v@, @u@ and @w@. Be aware of
-- combinatorial explosion.
composition
  :: ( Eq (f b)
     , Monad m
     , Show (f c)
     , Show (f (a -> b))
     , Show (f (c -> a))
     , Applicative f
     )
  => Series m (f (c -> a))
  -> Series m (f c)
  -> Series m (f (a -> b))
  -> Property m
composition vs ws us =
    over vs $ \v ->
        over ws $ \w ->
            over us $ \u ->
    (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

-- | Check the /composition/ law hold for the given 'Applicative' 'Series':
--
-- @
-- '(.)' '<$>' u '<*>' v '<*>' w ≡  u '<*>' (v '<*>' w)
-- @
-- This uses 'zipLogic3' for the generation 'Series' of @v@, @u@ and @w@.
compositionSum
  :: ( Eq (f b)
     , Monad m
     , Show (f c)
     , Show (f (a -> b))
     , Show (f (c -> a))
     , Applicative f
     )
  => Series m (f (c -> a))
  -> Series m (f c)
  -> Series m (f (a -> b))
  -> Property m
compositionSum vs ws us = over (zipLogic3 vs ws us) $ \(v,w,u) ->
    (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

-- | Check the /homomorphism/ law hold for the given 'Applicative' 'Series':
--
-- @
-- 'pure' f '<*>' 'pure' x ≡ 'pure' (f x)
-- @
--
-- Exhaustive generation for the 'Series' of @x@ and @f@. Be aware of
-- combinatorial explosion.
homomorphism
  :: forall m f a b .
     ( Monad m
     , Applicative f
     , Eq b
     , Eq (f b)
     , Show a, Show b
     , Serial Identity a
     , Serial Identity b
     )
  => Proxy f -> Series m a -> Series m (a -> b) -> Property m
homomorphism _ xs fs =
    over xs $ \x ->
        over fs $ \f ->
    (pure f <*> (pure x :: f a)) == pure (f x)

-- | Check the /homomorphism/ law hold for the given 'Applicative' 'Series':
--
-- @
-- 'pure' f '<*>' 'pure' x ≡ 'pure' (f x)
-- @
--
-- This uses 'zipLogic' for the generation 'Series' of @x@ and @f@.
homomorphismSum
  :: forall m f a b .
     ( Monad m
     , Applicative f
     , Eq b
     , Eq (f b)
     , Show a, Show b
     , Serial Identity a
     , Serial Identity b
     )
  => Proxy f -> Series m a -> Series m (a -> b) -> Property m
homomorphismSum _ xs fs = over (zipLogic xs fs) $ \(x,f) ->
    (pure f <*> (pure x :: f a)) == pure (f x)

-- | Check the /interchange/ law hold for the given 'Applicative' 'Series':
--
-- @
-- u '<*>' 'pure' y ≡ 'pure' ($ y) '<*>' u
-- @
--
-- Exhaustive generation for the 'Series' of @y@ and @u@. Be aware of
-- combinatorial explosion.
interchange
  :: ( Eq (f b)
     , Monad m
     , Show a
     , Show (f (a -> b))
     , Applicative f
     )
  => Series m a -> Series m (f (a -> b)) -> Property m
interchange ys us =
    over ys $ \y ->
        over us $ \u ->
            (u <*> pure y) == (pure ($ y) <*> u)

-- | Check the /interchange/ law hold for the given 'Applicative' 'Series':
--
-- @
-- u '<*>' 'pure' y ≡ 'pure' ($ y) '<*>' u
-- @
--
-- This uses 'zipLogic' for the generation 'Series' of @y@ and @u@.
interchangeSum
  :: ( Eq (f b)
     , Monad m
     , Show a
     , Show (f (a -> b))
     , Applicative f
     )
  => Series m a -> Series m (f (a -> b)) -> Property m
interchangeSum ys us = over (zipLogic ys us) $ \(y,u) ->
    (u <*> pure y) == (pure ($ y) <*> u)
