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

type Test = MergeAll
  [ KeyVal "lmao" String
  , KeyVal "the_key" (Maybe Int)
  , KeyVal "the_other_key" (Maybe String)
  ]

data Key s = Key
instance s ~ l => IsLabel l (Key s) where
  fromLabel = Key

exx :: Test
exx = mkTest "asd"

mkTest :: String -> Test
mkTest s = KeyVal s :*: KeyVal Nothing :*: KeyVal Nothing :*: Nil

lbl :: Key "the_key"
lbl = Key

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

exx' :: Test
exx' = exx & withOpt #the_key 99 & withOpt #the_other_key "qwewewe"

-- prpr :: Maybe Test
-- prpr = decode "{ \"lmao\":\"lll\", \"the_key\":\"kkk\",\"the_other_key\":\"uhmm\" }"

type Test2 = MergeAll
  [ KeyVal "lmao" String
  , KeyVal "the_key" (Maybe Int)
  , KeyVal "the_other_key" (Maybe String)
  ]

prpr :: Maybe Test2
prpr = decode "{ \"lmao\":\"lll\", \"the_key\":10, \"the_other_key\":\"uhmm\" }"

-- haa :: Test -> Maybe Int
-- haa = getVal . ixIntoLabel #the_key

-- withOpt :: forall (k :: Symbol) v p .
--            IxInto p (Maybe v) (FindIx (KeyVal k (Maybe v)) p)
--         => Key k -> v -> p -> p
-- withOpt k v = ixInto (Proxy @(FindIx (KeyVal k (Maybe v)) p)) (Just v)


{-

type family FindIx a b :: IxPos where
  FindIx a Nil = NotFound
  FindIx a (a :*: b) = Here
  FindIx a (b :*: c) = IncrIx (FindIx a c)

class IxInto (p :: *) (a :: *) (n :: IxPos) | p n -> a where
  ixInto :: Proxy n -> a -> p -> p

instance IxInto (a :*: b) a Here where
  ixInto _ x' (_ :*: y) = x' :*: y
-- ixInto _ (x :*: _) = x

instance IxInto b c n => IxInto (a :*: b) c (There n) where
  -- ixInto p (_ :*: y) = ixInto (prevIx p) y
  ixInto p y' (x :*: y) = x :*: ixInto (prevIx p) y' y
    where
      prevIx :: Proxy (There n) -> Proxy n
      prevIx _ = Proxy

type Test = MergeAll
  [ KeyVal "lmao" String
  , KeyVal "the_key" (Maybe Int)
  ]

data Key s = Key
instance s ~ l => IsLabel l (Key s) where
  fromLabel = Key

exx :: Test
exx = mkTest "asd"

mkTest :: String -> Test
mkTest s = KeyVal s :*: KeyVal Nothing :*: Nil

lbl :: Key "the_key"
lbl = Key

exx' :: Test
exx' = exx & withOpt lbl _

-- haa :: Test -> Maybe Int
-- haa = getVal . ixIntoLabel #the_key

withOpt :: forall (k :: Symbol) v p .
           IxInto p (Maybe v) (FindIx (KeyVal k (Maybe v)) p)
        => Key k -> v -> p -> p
withOpt k v = ixInto (Proxy @(FindIx (KeyVal k (Maybe v)) p)) (Just v)

-- instance IxInto (Merge a b) c (There n) where
--   ixInto = _

-- lmfao :: (FindIx Int (String :*: Int :*: Nil) ~ There Here) => ()
-- lmfao = _

x :: Label "asd"
x = Label

y :: Label "qwe"
y = Label


rekk :: Record '[Tagged "asd" String, Tagged "qwe" Char]
rekk = x .=. "v1" .*. y .=. '2' .*. emptyRecord

uhm :: Record '[Tagged "asd" String, Tagged "qwe" Char] -> ()
uhm (Record xs) = undefined

-- type R xs = StripNull (Record xs)

-- type Filez = R
--   [ "file_ids" :> Maybe [Int]
--   , "file_hashes" :> Maybe [String]
--   ]

-- xlbs :: ExtensibleConstr (:&) xs (Field Identity) (k ':> a)
--      => Proxy k -> Setter (Record (kk :> v :: xs)) a
-- xlbs = xlb

-- mkFiles :: Maybe [Int] -> Maybe [String] -> Filez
-- mkFiles xs ys = StripNull $
--      #file_ids @= xs
--   <: #file_hashes @= ys
--   <: emptyRecord

-- withOpt :: Proxy s -> a -> Record xs -> Record xs
-- withOpt p x r = set (xlb p) r x

-- fidsOpt :: Lens' Files (Maybe [Int])
-- fidsOpt = xlb #file_ids

-- mkLL :: Int -> [Int] -> [String] -> LL
-- mkLL n xs ys =
--   HCons (Identity n) (HCons (Identity (file_ids @= xs <: _)) HNil)

-- asd :: LL -> Identity Int
-- asd ll = hindex ll membership
-}
