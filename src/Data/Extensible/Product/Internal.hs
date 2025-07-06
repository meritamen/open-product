{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Extensible.Product.Internal (
  OpenProduct (..)
  , nil
  , Key (..)
  , insert
  , get
  , update
  , delete
  , upsert
  ) where

import Data.Kind
import Data.Proxy
import Data.Vector (Vector)
import Data.Vector qualified as V
import Fcf hiding (Any)
import Fcf.Data.List
import GHC.OverloadedLabels
import GHC.TypeLits
import Unsafe.Coerce

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key

type UniqueKey (key :: k) (ts :: [(k, t)]) = Null =<< Filter (TyEq key <=< Fst) ts

type family RequireUniqueKey
    (result :: Bool)
    (key :: Symbol)
    (t :: k)
    (ts :: [(Symbol, k)]) :: Constraint where
  RequireUniqueKey 'True _ _ _ = ()
  RequireUniqueKey 'False key t ts =
    TypeError
         ( 'Text "Attempting to add a field named `"
           ':<>: 'Text key
           ':<>: 'Text "' with type "
           ':<>: 'ShowType t
           ':<>: 'Text " to an OpenProduct."
           ':$$: 'Text "But the OpenProduct already has a field `"
           ':<>: 'Text key
           ':<>: 'Text "' with type "
           ':<>: 'ShowType (LookupType key ts)
           ':$$: 'Text"Consider using `update'"
           ':<>: 'Text "instead of `insert'."
         )

insert :: RequireUniqueKey (Eval (UniqueKey key ts)) key t ts
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type FindElem (key :: Symbol) (ts :: [(Symbol, k)])
  = Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type family FriendlyFindElem (funcName :: Symbol)
                             (key :: Symbol)
                             (ts :: [(Symbol, k)]) where
  FriendlyFindElem funcName key ts =
    Eval ( FromMaybe
           ( TypeError
             ( 'Text "Attempted to call `" ':<>: 'Text funcName
               ':<>: 'Text "' with key `"
               ':<>: 'Text key
               ':<>: 'Text "'."
               ':$$: 'Text "But the OpenProduct only has keys :" ':$$: 'Text " "
               ':<>: 'ShowType (Eval (Map Fst ts))
             )) =<< FindIndex (TyEq key <=< Fst) ts)

type LookupType (key :: k) (ts :: [(k, t)]) = FromMaybe Stuck =<< Lookup key ts

get :: forall key ts f. KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> f (Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)])
  = SetIndex (FindElem key ts) '(key, t) ts

update
  :: forall key ts t f.
    ( KnownNat (FriendlyFindElem "update" key ts)
    , KnownNat (FindElem key ts)
    )
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)])
  = Uncurry (++) =<< Map (FromMaybe Stuck <=< Tail) =<< (Break ((TyEq key) <=< Fst) ts)

delete
  :: forall key ts f.
  ( KnownNat (FriendlyFindElem "delete" key ts)
  , KnownNat (FindElem key ts)
  )
  => Key key
  -> OpenProduct f ts
  -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) = let (a, b) = V.splitAt (findElem @key @ts) v
                           in OpenProduct $ a V.++ V.tail b

type UpsertElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  FromMaybe ('(key, t) ': ts)
  =<< Map (Placeholder1Of3 SetIndex '(key, t) ts)
  =<< FindIndex (TyEq key <=< Fst) ts

data Placeholder1Of3 :: (a -> b -> c -> Exp r) -> b -> c -> a -> Exp r

type instance Eval (Placeholder1Of3 f b c a) = Eval (f a b c)

type UpsertLoc (key :: Symbol) (ts :: [(Symbol, k)])
  = Eval (FindIndex (TyEq key <=< Fst) ts)

class FindUpsertElem (a :: Maybe Nat) where
  upsertElem :: Maybe Int

instance FindUpsertElem 'Nothing where
  upsertElem = Nothing

instance KnownNat n => FindUpsertElem ('Just n) where
  upsertElem = Just . fromIntegral . natVal $ Proxy @n

upsert
  :: forall key ts t f. FindUpsertElem (UpsertLoc key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpsertElem key t ts))
upsert _ ft (OpenProduct v) =
  OpenProduct $ case upsertElem @(UpsertLoc key ts) of
  Nothing -> V.cons (Any ft) v
  Just n -> v V.// [(n, Any ft)]
