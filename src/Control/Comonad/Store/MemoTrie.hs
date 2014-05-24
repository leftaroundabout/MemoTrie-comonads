{-# LANGUAGE CPP, TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Store.MemoTrie
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The trie-memoizing store (state-in-context/costate) comonad transformer is 
-- subject to the laws:
-- 
-- > x = seek (pos x) x
-- > y = pos (seek y x)
-- > seek y x = seek y (seek z x)
--
-- Thanks go to Russell O'Connor and Daniel Peebles for their help formulating 
-- and proving the laws for this comonad transformer.
----------------------------------------------------------------------------
module Control.Comonad.Store.MemoTrie
  ( 
  -- * The Store comonad
    Store, store, runStore
  -- * The Store comonad transformer
  , StoreT(..), storeT, runStoreT
  -- * Operations
  , module Control.Comonad.Store.Class
  ) where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Control.Comonad.Store.Class
import Control.Comonad.Env.Class
import Control.Comonad.Traced.Class
import Control.Comonad.Traced.MemoTrie
import Data.Functor.Identity
import Data.Functor.Apply
import Data.MemoTrie
import Data.Semigroup
import Data.Functor.Extend
import Data.Monoid hiding ((<>))

import qualified Test.QuickCheck as QC

type Store s = StoreT s Identity

store :: HasTrie s => (s -> a) -> s -> Store s a 
store f s = StoreT (Identity (trie f)) s

runStore :: HasTrie s => Store s a -> (s -> a, s)
runStore (StoreT (Identity f) s) = (untrie f, s)

data StoreT s w a = StoreT (w (s :->: a)) s
deriving instance (Eq (w (s :->: a)), Eq s) => Eq (StoreT s w a)
deriving instance (Show (w (s :->: a)), Show s) => Show (StoreT s w a)

storeT :: (Functor w, HasTrie s) => w (s -> a) -> s -> StoreT s w a 
storeT wf s = StoreT (trie <$> wf) s

runStoreT :: (Functor w, HasTrie s) => StoreT s w a -> (w (s -> a), s)
runStoreT (StoreT wf s) = (untrie <$> wf, s)

instance (Functor w, HasTrie s) => Functor (StoreT s w) where
  fmap f (StoreT wf s) = StoreT (fmap (fmap f) wf) s

instance (Apply w, Semigroup s, HasTrie s) => Apply (StoreT s w) where
  StoreT ff m <.> StoreT fa n = StoreT ((<*>) <$> ff <.> fa) (m <> n)

instance (Applicative w, Semigroup s, Monoid s, HasTrie s) => Applicative (StoreT s w) where
  pure a = StoreT (pure (pure a)) mempty
  StoreT ff m <*> StoreT fa n = StoreT ((<*>) <$> ff <*> fa) (m `mappend` n)

instance (Extend w, HasTrie s) => Extend (StoreT s w) where
  duplicated (StoreT wf s) = StoreT (extended (trie . StoreT) wf) s

instance (Comonad w, HasTrie s) => Comonad (StoreT s w) where
  extract (StoreT wf s) = untrie (extract wf) s

instance HasTrie s => ComonadTrans (StoreT s) where
  lower (StoreT f s) = fmap (`untrie` s) f

instance ComonadHoist (StoreT s) where
  cohoist t (StoreT f s) = StoreT (t f) s

instance (Comonad w, HasTrie s) => ComonadStore s (StoreT s w) where
  pos (StoreT _ s) = s
  seek s (StoreT f _) = StoreT f s
  seeks f (StoreT g s) = StoreT g (f s)
  peek s (StoreT g _) = untrie (extract g) s
  peeks f (StoreT g s) = untrie (extract g) (f s)

instance (ComonadTraced m w, HasTrie s) => ComonadTraced m (StoreT s w) where
  trace m = trace m . lower

instance (ComonadEnv m w, HasTrie s) => ComonadEnv m (StoreT s w) where 
  ask = ask . lower

instance (Functor w, QC.Arbitrary (w (Traced s a)), QC.Arbitrary s)
               => QC.Arbitrary (StoreT s w a) where
  arbitrary = StoreT <$> (fmap (\(TracedT (Identity q))->q)<$>QC.arbitrary)
                     <*> QC.arbitrary
