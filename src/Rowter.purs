module Rowter where

import Prelude

import Data.Codec (Codec', codec')
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Number as Number
import Data.Number.Format as Number.Format
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))

-- Codec

class HasJsonCodec a where
  jsonCodec :: CA.JsonCodec a

instance HasJsonCodec Int where
  jsonCodec = CA.int

instance HasJsonCodec Number where
  jsonCodec = CA.number

instance HasJsonCodec String where
  jsonCodec = CA.string

instance HasJsonCodec Boolean where
  jsonCodec = CA.boolean

instance HasJsonCodec Unit where
  jsonCodec = CA.null

instance HasJsonCodec a => HasJsonCodec (Array a) where
  jsonCodec = CA.array jsonCodec

class JsonRecord (rl :: RowList Type) (r :: Row Type) | rl -> r where
  jsonRecord :: Proxy rl -> CA.JPropCodec (Record r)

instance JsonRecord Nil () where
  jsonRecord _ = CA.record

instance
  ( IsSymbol l
  , HasJsonCodec a
  , JsonRecord tl r'
  , Row.Cons l a r' r
  , Row.Lacks l r'
  ) =>
  JsonRecord (Cons l a tl) r where
  jsonRecord _ = CA.recordProp (Proxy :: Proxy l) jsonCodec
    (jsonRecord (Proxy :: Proxy tl))

instance (RowToList r rl, JsonRecord rl r) => HasJsonCodec (Record r) where
  jsonCodec = CA.object "Record" (jsonRecord (Proxy :: Proxy rl))

type TextCodec a = Codec' (Either String) String a

class HasTextCodec a where
  textCodec :: TextCodec a

instance HasTextCodec String where
  textCodec = codec' Right identity

instance HasTextCodec Int where
  textCodec = codec'
    (\s -> note ("Invalid integer: " <> s) (Int.fromString s))
    show

instance HasTextCodec Number where
  textCodec = codec'
    ( \s ->
        if Regex.test numberPattern s then note (invalid s)
          (Number.fromString s)
        else Left (invalid s)
    )
    Number.Format.toString
    where
    invalid s = "Invalid number: " <> s

numberPattern :: Regex
numberPattern = unsafeRegex
  "^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$"
  noFlags

instance HasTextCodec Boolean where
  textCodec = codec'
    ( case _ of
        "true" -> Right true
        "false" -> Right false
        s -> Left ("Invalid boolean: " <> s)
    )
    show
