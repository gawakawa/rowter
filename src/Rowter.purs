module Rowter where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Array as Array
import Data.Codec (Codec', codec', decode, encode)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush, note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Number.Format as Number.Format
import Data.Reflectable (class Reflectable, reflectType)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol (class Append)
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Equality (class TypeEquals, from)
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

-- Response

newtype WithStatus :: Int -> Type -> Type
newtype WithStatus code a = WithStatus a

type Response =
  { status :: Int
  , headers :: Array (Tuple String String)
  , body :: String
  }

respond
  :: forall @label code a r_ r
   . IsSymbol label
  => Row.Cons label (WithStatus code a) r_ r
  => a
  -> Variant r
respond a = V.inj (Proxy :: Proxy label) (WithStatus a)

class EncodeResponse (rl :: RowList Type) (r :: Row Type) | rl -> r where
  encodeResponse :: Proxy rl -> Variant r -> Response

instance EncodeResponse Nil () where
  encodeResponse _ = V.case_

instance
  ( IsSymbol l
  , Reflectable code Int
  , HasJsonCodec a
  , EncodeResponse tl r'
  , Row.Cons l (WithStatus code a) r' r
  ) =>
  EncodeResponse (Cons l (WithStatus code a) tl) r where
  encodeResponse _ = V.on (Proxy :: Proxy l)
    ( \(WithStatus a) ->
        { status: reflectType (Proxy :: Proxy code)
        , headers: [ Tuple "Content-Type" "application/json" ]
        , body: stringify (encode jsonCodec a)
        }
    )
    (encodeResponse (Proxy :: Proxy tl))
else instance
  Fail (Text "A response must be written as WithStatus code body") =>
  EncodeResponse (Cons l a tl) () where
  encodeResponse _ = V.case_

toResponse
  :: forall r rl. RowToList r rl => EncodeResponse rl r => Variant r -> Response
toResponse = encodeResponse (Proxy :: Proxy rl)

-- Path

data Segments

foreign import data SNil :: Segments
foreign import data SLit :: Symbol -> Segments -> Segments
foreign import data SCapture :: Symbol -> Segments -> Segments

-- "/users/:id" is parsed into SLit "users" (SCapture "id" SNil)
class ParsePath (path :: Symbol) (segs :: Segments) | path -> segs

instance (Symbol.Cons h t path, ParseHead h t segs) => ParsePath path segs

class ParseHead (h :: Symbol) (t :: Symbol) (segs :: Segments) | h t -> segs

instance ParseRest t "" segs => ParseHead "/" t segs
else instance Fail (Text "A path must start with /") => ParseHead h t SNil

-- acc is the segment being read
class
  ParseRest (rest :: Symbol) (acc :: Symbol) (segs :: Segments)
  | rest acc -> segs

instance Emit acc SNil segs => ParseRest "" acc segs
else instance
  ( Symbol.Cons c rest' rest
  , ParseStep c rest' acc segs
  ) =>
  ParseRest rest acc segs

class
  ParseStep (c :: Symbol) (rest :: Symbol) (acc :: Symbol) (segs :: Segments)
  | c rest acc -> segs

instance
  ( ParseRest rest "" next
  , Emit acc next segs
  ) =>
  ParseStep "/" rest acc segs
else instance
  ( Append acc c acc'
  , ParseRest rest acc' segs
  ) =>
  ParseStep c rest acc segs

class
  Emit (acc :: Symbol) (next :: Segments) (out :: Segments)
  | acc next -> out

instance Emit "" next next
else instance
  ( Symbol.Cons h t acc
  , EmitSegment h t acc next out
  ) =>
  Emit acc next out

class
  EmitSegment
    (h :: Symbol)
    (t :: Symbol)
    (acc :: Symbol)
    (next :: Segments)
    (out :: Segments)
  | h t acc next -> out

instance EmitSegment ":" name acc next (SCapture name next)
else instance EmitSegment h t acc next (SLit acc next)

class MatchPath (segs :: Segments) (params :: Row Type) where
  matchPath :: Proxy segs -> Array String -> Maybe (Record params)

-- Only a path without captures may have an empty params
instance TypeEquals (Record params) (Record ()) => MatchPath SNil params where
  matchPath _ [] = Just (from {})
  matchPath _ _ = Nothing

instance
  ( IsSymbol lit
  , MatchPath rest params
  ) =>
  MatchPath (SLit lit rest) params where
  matchPath _ segs = case Array.uncons segs of
    Just { head, tail } | head == reflectSymbol (Proxy :: Proxy lit) ->
      matchPath (Proxy :: Proxy rest) tail
    _ -> Nothing

instance
  ( IsSymbol name
  , Row.Cons name a params' params
  , Row.Lacks name params'
  , HasTextCodec a
  , MatchPath rest params'
  ) =>
  MatchPath (SCapture name rest) params where
  matchPath _ segs = case Array.uncons segs of
    Just { head, tail } -> do
      a <- hush (decode textCodec head)
      rest <- matchPath (Proxy :: Proxy rest) tail
      pure (Record.insert (Proxy :: Proxy name) a rest)
    Nothing -> Nothing
