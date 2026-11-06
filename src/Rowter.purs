module Rowter where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
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
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as FO
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

-- Spec

data Endpoint
  (method :: Symbol)
  (path :: Symbol)
  (req :: Type)
  (res :: Row Type)

type GET = Endpoint "GET"
type POST = Endpoint "POST"
type PUT = Endpoint "PUT"
type PATCH = Endpoint "PATCH"
type DELETE = Endpoint "DELETE"

-- Router

type Request =
  { method :: String
  , path :: Array String
  , query :: Object String
  , body :: String
  }

-- The row of the "params" field of a request record
class ParamsOf (rl :: RowList Type) (params :: Row Type) | rl -> params

instance ParamsOf Nil ()
instance ParamsOf (Cons "params" (Record params) tl) params
else instance ParamsOf tl params => ParamsOf (Cons l a tl) params

class ParseQuery (rl :: RowList Type) (q :: Row Type) | rl -> q where
  parseQuery :: Proxy rl -> Object String -> Either String (Record q)

instance ParseQuery Nil () where
  parseQuery _ _ = Right {}

instance
  ( IsSymbol l
  , HasTextCodec a
  , ParseQuery tl q'
  , Row.Cons l a q' q
  , Row.Lacks l q'
  ) =>
  ParseQuery (Cons l a tl) q where
  parseQuery _ obj = do
    let key = reflectSymbol (Proxy :: Proxy l)
    raw <- note ("Missing query parameter: " <> key) (FO.lookup key obj)
    a <- lmap (\e -> "Invalid query parameter " <> key <> ": " <> e)
      (decode textCodec raw)
    rest <- parseQuery (Proxy :: Proxy tl) obj
    pure (Record.insert (Proxy :: Proxy l) a rest)

-- How one field of the request record is built; params is the already matched path
class ReqField (params :: Row Type) (l :: Symbol) (a :: Type) where
  reqField :: Proxy l -> Record params -> Request -> Either String a

instance ReqField params "params" (Record params) where
  reqField _ params _ = Right params
else instance
  ( RowToList q ql
  , ParseQuery ql q
  ) =>
  ReqField params "query" (Record q) where
  reqField _ _ request = parseQuery (Proxy :: Proxy ql) request.query
else instance HasJsonCodec a => ReqField params "body" a where
  reqField _ _ request = do
    json <- lmap (\e -> "Invalid JSON: " <> e) (jsonParser request.body)
    lmap (\e -> "Invalid body: " <> CA.printJsonDecodeError e)
      (decode jsonCodec json)
else instance
  Fail (Text "A request type may only have the fields params, query and body") =>
  ReqField params l a where
  reqField _ _ _ = Left ""

-- Builds the record a handler receives
class
  BuildReq (params :: Row Type) (rl :: RowList Type) (req :: Row Type)
  | rl -> req where
  buildReq :: Proxy rl -> Record params -> Request -> Either String (Record req)

instance BuildReq params Nil () where
  buildReq _ _ _ = Right {}

instance
  ( IsSymbol l
  , ReqField params l a
  , BuildReq params tl req'
  , Row.Cons l a req' req
  , Row.Lacks l req'
  ) =>
  BuildReq params (Cons l a tl) req where
  buildReq _ params request = do
    a <- reqField (Proxy :: Proxy l) params request
    rest <- buildReq (Proxy :: Proxy tl) params request
    pure (Record.insert (Proxy :: Proxy l) a rest)

class Route endpoint handler | endpoint -> handler where
  route :: Proxy endpoint -> handler -> Request -> Maybe (Aff Response)

instance
  ( IsSymbol method
  , ParsePath path segs
  , RowToList req rl
  , ParamsOf rl params
  , MatchPath segs params
  , BuildReq params rl req
  , RowToList res resRl
  , EncodeResponse resRl res
  ) =>
  Route (Endpoint method path (Record req) res)
    (Record req -> Aff (Variant res)) where
  route _ handler request
    | request.method == reflectSymbol (Proxy :: Proxy method) =
        matchPath (Proxy :: Proxy segs) request.path <#>
          \(params :: Record params) ->
            case buildReq (Proxy :: Proxy rl) params request of
              Left message -> pure (badRequest message)
              Right req -> toResponse <$> handler req
    | otherwise = Nothing

class Routes (rl :: RowList Type) (handlers :: Row Type) where
  routes :: Proxy rl -> Record handlers -> Request -> Maybe (Aff Response)

instance Routes Nil handlers where
  routes _ _ _ = Nothing

instance
  ( IsSymbol l
  , Row.Cons l handler handlers' handlers
  , Route endpoint handler
  , Routes tl handlers
  ) =>
  Routes (Cons l endpoint tl) handlers where
  routes _ handlers request =
    route (Proxy :: Proxy endpoint) (Record.get (Proxy :: Proxy l) handlers)
      request
      <|> routes (Proxy :: Proxy tl) handlers request

router
  :: forall @spec rl handlers
   . RowToList spec rl
  => Routes rl handlers
  => Record handlers
  -> Request
  -> Aff Response
router handlers request = case routes (Proxy :: Proxy rl) handlers request of
  Just response -> response
  Nothing -> pure notFound

notFound :: Response
notFound = { status: 404, headers: [], body: "Not Found" }

badRequest :: String -> Response
badRequest message = { status: 400, headers: [], body: message }
