{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrus.API.Utils where

import Data.Aeson ( ToJSON(..), Value(..), FromJSON(..), Value, withObject, (.:), decode )
import GHC.Base (Symbol, Constraint)
import Data.Kind (Type)
import qualified Data.Aeson.KeyMap as KM (filter, union, fromMapText, singleton)
import GHC.TypeLits (Nat, KnownSymbol, symbolVal)
import Data.Data (Proxy(..))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import GHC.OverloadedLabels (IsLabel (..))
import Data.Function ((&))
import Control.Applicative ((<|>), Const (..))
import Control.Monad.Identity (Identity(..))

-- import Lens.Micro (Lens')

-- type Lens' s a = Lens s s a a
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

set :: Lens' t a -> a -> t -> t
set l x = runIdentity . l (const (Identity x))

get :: Lens' t a -> t -> a
get l t = getConst (l Const t)

type family Concat (xs :: [*]) (ys :: [*]) :: [*] where
  Concat '[] ys = ys
  Concat (x ': xs) ys = x ': Concat xs ys

mergeAeson :: Value -> Value -> Value
mergeAeson Null x = x
mergeAeson x Null = x
mergeAeson (Object x) (Object y) = Object (KM.union x y)

data Nil = Nil deriving Show
--type (:*:) = Merge
data a :*: b = a :*: b deriving Show
infixr 4 :*:


type family MergeAll (xs :: [*]) :: * where
  MergeAll '[] = Nil
  MergeAll (x ': xs) = x :*: MergeAll xs

newtype KeyVal (s :: Symbol) (a :: *) = KeyVal { getVal :: a }
instance (KnownSymbol s, Show a) => Show (KeyVal s a) where
  show kv@(KeyVal x) = "KeyVal " ++ symbolVal (keyValStr kv) ++ " (" ++ show x ++ ")"

keyValStr :: KeyVal s a -> Proxy s
keyValStr _ = Proxy

instance ToJSON Nil where
  toJSON Nil = Null

instance (KnownSymbol s, ToJSON a) => ToJSON (KeyVal s a) where
  toJSON kv@(KeyVal x) =
    case toJSON x of
      Null -> Null
      o -> Object (KM.singleton (fromString (symbolVal (keyValStr kv))) o)

instance (ToJSON a, ToJSON b) => ToJSON (a :*: b) where
  toJSON (x :*: y) = mergeAeson (toJSON x) (toJSON y)

parseKeyValJSON :: forall s a . (KnownSymbol s, FromJSON a) => Value -> Parser (KeyVal s a)
parseKeyValJSON = withObject "" $ \v -> KeyVal
  <$> v .: fromString (symbolVal (Proxy @s))

instance (KnownSymbol s, FromJSON a) => FromJSON (KeyVal s a) where
  parseJSON = parseKeyValJSON

instance (FromJSON a, KnownSymbol k, FromJSON b) => FromJSON (KeyVal k a :*: b) where
  parseJSON v = (:*:) <$> parseKeyValJSON v <*> parseJSON v

instance FromJSON Nil where
  parseJSON _ = pure Nil

data a :+: b = L a | R b
infix 5 :+:

instance (ToJSON a, ToJSON b) => ToJSON (a :+: b) where
  toJSON = \case
    L x -> toJSON x
    R x -> toJSON x

instance (FromJSON a, FromJSON b) => FromJSON (a :+: b) where
  parseJSON v = (L <$> parseJSON v) <|> (R <$> parseJSON v)

--------------------------------------------------------------------------------

data IxPos = Here | There IxPos | NotFound

type family IncrIx n :: IxPos where
  IncrIx NotFound = NotFound
  IncrIx n = There n

type family FindKey k p :: IxPos where
  FindKey k Nil = NotFound
  FindKey k (KeyVal k a :*: b) = Here
  FindKey k (b :*: c) = IncrIx (FindKey k c)

class KeyInto (p :: *) (n :: IxPos) where
  type Val p n
  ixInto :: Proxy n -> Lens' p (Val p n) -- -> p -> p

instance KeyInto (KeyVal k v :*: b) Here where
  type Val (KeyVal k v :*: b) Here = v
  ixInto _ f (KeyVal v :*: y) = fmap ((:*: y) . KeyVal) (f v)
  -- ixInto _ x' (_ :*: y) = KeyVal x' :*: y

instance KeyInto b n => KeyInto (a :*: b) (There n) where
  type Val (a :*: b) (There n) = Val b n
  ixInto p f (x :*: y) = fmap (x :*:) (ixInto (prevIx p) f y)
  -- ixInto p y' (x :*: y) = x :*: ixInto (prevIx p) y' y
    where
      prevIx :: Proxy (There n) -> Proxy n
      prevIx _ = Proxy

data Key s = Key
instance s ~ l => IsLabel l (Key s) where
  fromLabel = Key

keyAsFind :: Key k -> Proxy p -> Proxy (FindKey k p)
keyAsFind _ _ = Proxy

withOpt :: forall (k :: Symbol) v p .
           (KeyInto p (FindKey k p), Val p (FindKey k p) ~ Maybe v)
        => Key k -> v -> p -> p
withOpt _ v = set (ixInto (keyAsFind (Key @k) (Proxy @p))) (Just v)

ix :: forall (k :: Symbol) v p .
           (KeyInto p (FindKey k p), Val p (FindKey k p) ~ v)
   => Key k -> p -> v
ix k = get (ixInto (keyAsFind (Key @k) (Proxy @p)))

