{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.TotalMap
-- Copyright   :  (c) Conal Elliott 2012--2019
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Finitely represented /total/ maps. Represented by as a partial map and
-- a default value. Has Applicative and Monad instances (unlike "Data.Map").
----------------------------------------------------------------------

module Data.TotalMap
  ( TMap,fromPartial
  , empty, insert, singleton
  , (!),tabulate,trim
  , intersectionPartialWith,range
  , mapKeysWith
  -- , tmapRepr
  ) where

import Data.Monoid (Monoid(..),(<>))
import Control.Applicative (Applicative(..),liftA2,(<$>))
import Data.Maybe (fromMaybe)
#if MIN_VERSION_base(4,11,0)
import qualified Data.Semigroup as Sem
#endif

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Semiring

-- import Control.Comonad  -- TODO

-- | Total map
data TMap k v = TMap v (Map k v) deriving Show

-- The representation is a default value and a finite map for the rest.

-- | Create a total map from a default value and a partial map.
fromPartial :: v -> Map k v -> TMap k v
fromPartial = TMap

-- | A total map only default
empty :: v -> TMap k v
empty v = TMap v M.empty

-- | Insert a key\/value pair
insert :: Ord k => k -> v -> TMap k v -> TMap k v
insert k v (TMap d m) = TMap d (M.insert k v m)

-- | Singleton plus default
singleton :: Ord k => k -> v -> v -> TMap k v
singleton k v d = insert k v (empty d)

infixl 9 !
-- | Sample a total map. Semantic function.
(!) :: Ord k => TMap k v -> k -> v
TMap dflt m ! k = fromMaybe dflt (M.lookup k m)

-- | Construct a total map, given a default value, a set of keys, and a
-- function to sample over that set. You might want to 'trim' the result.
tabulate :: Eq k => v -> Set k -> (k -> v) -> TMap k v
tabulate dflt keys f = TMap dflt (f <$> idMap keys)

-- | Optimize a 'TMap', weeding out any explicit default values.
-- A semantic no-op, i.e., @(!) . trim == (!)@.
trim :: (Ord k, Eq v) => TMap k v -> TMap k v
trim (TMap dflt m) = TMap dflt (M.filter (/= dflt) m)

{-
-- Variation that weeds out values equal to the default. Requires Eq.
tabulate' :: (Ord k, Eq v) => v -> Set k -> (k -> v) -> TMap k v
tabulate' = (fmap.fmap.fmap) trim tabulate
-}

-- | Intersect a total map with a partial one using an element combinator.
intersectionPartialWith ::
   (Ord k) =>
   (a -> b -> c) -> TMap k a -> Map k b -> Map k c
intersectionPartialWith f (TMap ad am) bm =
   M.intersectionWith f am bm
   `M.union`
   fmap (f ad) bm

-- | Witness the finiteness of the support concretely by giving its image.
range :: Ord v => TMap k v -> Set v
range (TMap dflt m) = S.fromList (dflt : M.elems m)

{--------------------------------------------------------------------
    Instances
--------------------------------------------------------------------}

-- These instances follow the principle that semantic functions (here (!))
-- must be type class morphism (TCM) for all inhabited type classes.

#if MIN_VERSION_base(4,11,0)
instance (Ord k, Sem.Semigroup v) => Sem.Semigroup (TMap k v) where
  (<>) = liftA2 (<>)
#endif

instance (Ord k, Monoid v) => Monoid (TMap k v) where
  mempty  = pure mempty
  mappend = liftA2 mappend

instance Functor (TMap k) where
  fmap f (TMap d m) = TMap (f d) (fmap f m)

instance Ord k => Applicative (TMap k) where
  pure v = TMap v mempty
  fs@(TMap df mf) <*> xs@(TMap dx mx) = 
    tabulate (df dx)
             (M.keysSet mf `mappend` M.keysSet mx)
             ((!) fs <*> (!) xs)

-- | Alternative implementation of (<*>) using complex Map operations. Might be
-- more efficient. Can be used for testing against the canonical implementation
-- above.
_app :: Ord k => TMap k (a -> b) -> TMap k a -> TMap k b
_app (TMap fd fm) (TMap ad am) =
   TMap (fd ad) $
      fmap ($ad) (M.difference fm am) <>
      fmap (fd$) (M.difference am fm) <>
      M.intersectionWith ($) fm am

-- Note: I'd like to 'trim' the tabulate result in <*>, but doing so would
-- require the Eq constraint on values, which breaks Applicative.

instance Ord k => Monad (TMap k) where
  return  = pure
  m >>= f = joinT (f <$> m)

joinT :: Ord k => TMap k (TMap k v) -> TMap k v
joinT (TMap (TMap dd dm) mtm) = 
  TMap dd (M.mapWithKey (flip (!)) mtm `M.union` dm)

{-
joinT' :: (Ord k,Eq v) => TMap k (TMap k v) -> TMap k v
joinT' = trim . joinT
-}

{-
joinT (tt@(TMap (TMap (dd,dm),mtm))) = 
  tabulate dd 
           undefined
           -- (join ((!) ((!) <$> tt)))
           ((!) tt >>= (!))
-}

{-

-- tt :: TMap k (TMap k v)
-- fmap (!) tt :: TMap k (k -> v)
-- (!) (fmap (!) tt) :: k -> (k -> v)

tt :: TMap k (TMap k v)
dd :: v
dm :: Map k v
mtm :: Map k (TMap k v)

mapWithKey (flip (!)) mtm :: Map k v
mapWithKey (flip (!)) mtm `union` dm :: Map k v

TMap (dd,M.mapWithKey (flip (!)) mtm `M.union` dm) :: TMap k v

spec:

-}

instance (Ord k, Semiring v) => Semiring (TMap k v) where
  zero = pure zero
  one = pure one
  (<+>) = liftA2 (<+>)
  (<.>) = liftA2 (<.>)

instance (Ord k, StarSemiring v) => StarSemiring (TMap k v) where
  star = fmap star
  plus = fmap plus

instance (Ord k, DetectableZero v) => DetectableZero (TMap k v) where
  isZero (TMap d m) = isZero d && M.null m -- or: isZero d && all isZero (elems m)

{--------------------------------------------------------------------
    Comonad
--------------------------------------------------------------------}

-- TODO: Based on the function-of-monoid comonad.
-- TODO: Also a version with a pointer.

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

mapKeysWith :: (Semiring z, Ord b) => (z -> z -> z) -> (a -> b) -> TMap a z -> TMap b z
mapKeysWith comb f (TMap d m) = TMap d (M.mapKeysWith comb f m)

-- liftA2Keys :: Ord b => (a -> b -> c) -> TMap a z -> TMap b z -> TMap c z
-- liftA2Keys f (TMap c m) (TMap d n) = ... -- ??

idMap :: Eq k => Set k -> Map k k
idMap = M.fromAscList . map (\ k -> (k,k)) . S.toAscList

-- or ... map (join (,)) ...

-- -- | Reveal representation. For use while experimenting.
-- tmapRepr :: TMap k v -> (v, Map k v)
-- tmapRepr (TMap d m) = (d,m)
