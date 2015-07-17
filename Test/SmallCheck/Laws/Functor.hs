{-# LANGUAGE FlexibleContexts #-}
module Test.SmallCheck.Laws.Functor where

import Data.Functor.Identity (Identity)
import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Serial, Series)
import Test.SmallCheck.Series.Utils (zipLogic3)

fmapIdentity
  :: (Eq (f a), Monad m, Show (f a), Functor f)
  => Series m (f a) -> Property m
fmapIdentity s = over s $ \x -> fmap id x == x

fmapCompose
  :: ( Monad m, Functor f, Show a, Show b, Show c
     , Show (f a), Eq (f c)
     , Serial Identity a, Serial Identity b
     )
  => Series m (f a) -> Series m (b -> c) -> Series m (a -> b) -> Property m
fmapCompose xs fs gs = over (zipLogic3 xs fs gs) $ \(x,f,g) ->
    fmap (f . g) x == (fmap f . fmap g) x
