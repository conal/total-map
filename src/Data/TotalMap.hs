{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.TotalMap
-- Copyright   :  (c) Conal Elliott 2012
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Finitely represented /total/ maps. Represented by as a partial map and
-- a default value. Has Applicative and Monad instances (unlike "Data.Map").
----------------------------------------------------------------------

module Data.TotalMap (TMap,(!),tabulate,trim) where

import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(..),(<$>))
import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

-- | Total map
data TMap k v = TMap v (Map k v) deriving Functor

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

instance Ord k => Applicative (TMap k) where
  pure v = TMap v mempty
  fs@(TMap df mf) <*> xs@(TMap dx mx) = 
    tabulate (df dx)
             (M.keysSet mf `mappend` M.keysSet mx)
             ((!) fs <*> (!) xs)

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

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

idMap :: Eq k => Set k -> Map k k
idMap = M.fromAscList . map (\ k -> (k,k)) . S.toAscList

-- or ... map (join (,)) ...
