module Test.Main where

import Prelude

import Data.Argonaut.Core (Json, caseJson, fromArray, fromObject, fromString, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (replicate)
import Data.Array.NonEmpty (cons')
import Data.Codec (decode, encode)
import Data.Codec.Argonaut (JsonDecodeError)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Number.Format (exponential, toString, toStringWith)
import Data.String (Pattern(..), Replacement(..), replaceAll, stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as FO
import Rowter (class HasJsonCodec, class HasTextCodec, Response, WithStatus, jsonCodec, respond, textCodec, toResponse)
import Test.QuickCheck (Result, arbitrary, (===))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

type User = { name :: String, age :: Int }
type ErrorBody = { message :: String }

type NotFoundErr r = (notFound :: WithStatus 404 ErrorBody | r)

taro :: User
taro = { name: "taro", age: 20 }

-- Json has no Show instance; Eq compares objects regardless of key order
newtype Structural = Structural Json

derive newtype instance Eq Structural

instance Show Structural where
  show (Structural json) = stringify json

parseStructural :: String -> Either String Structural
parseStructural = map Structural <<< jsonParser

assertJsonBody :: String -> Response -> Aff Unit
assertJsonBody expected response =
  Assert.equal (parseStructural expected) (parseStructural response.body)

genUser :: Gen User
genUser = do
  name <- arbitrary
  age <- arbitrary
  pure { name, age }

decodeJson :: forall a. HasJsonCodec a => Json -> Either JsonDecodeError a
decodeJson = decode jsonCodec

encodeJson :: forall a. HasJsonCodec a => a -> Json
encodeJson = encode jsonCodec

decodeText :: forall a. HasTextCodec a => String -> Either String a
decodeText = decode textCodec

encodeText :: forall a. HasTextCodec a => a -> String
encodeText = encode textCodec

-- Value side: decode (encode a) == Right a
jsonLaw :: forall a. HasJsonCodec a => Eq a => Show a => a -> Result
jsonLaw a = decodeJson (encodeJson a) === Right a

textLaw :: forall a. HasTextCodec a => Eq a => Show a => a -> Result
textLaw a = decodeText (encodeText a) === Right a

-- Representation side: s ≈ encode a, where s ≈ t iff decode s == decode t.
-- genRepr builds such an s from a.
genIntRepr :: Int -> Gen String
genIntRepr n = do
  let
    shown = show n
    { sign, digits } = case stripPrefix (Pattern "-") shown of
      Just rest -> { sign: "-", digits: rest }
      Nothing -> { sign: "", digits: shown }
  sign' <- if sign == "-" then pure "-" else elements (cons' "" [ "+" ])
  zeros <- chooseInt 0 3
  pure (sign' <> fromCharArray (replicate zeros '0') <> digits)

genNumberRepr :: Number -> Gen String
genNumberRepr x = do
  base <- elements (cons' (toString x) [ toStringWith (exponential 20) x ])
  plus <- if x >= 0.0 then elements (cons' "" [ "+" ]) else pure ""
  upper <- arbitrary
  pure
    ( plus <>
        if upper then replaceAll (Pattern "e") (Replacement "E") base else base
    )

-- Adds extra fields to every object
genJsonRepr :: Json -> Gen Json
genJsonRepr j = caseJson
  (\_ -> pure j)
  (\_ -> pure j)
  (\_ -> pure j)
  (\_ -> pure j)
  (\xs -> fromArray <$> traverse genJsonRepr xs)
  ( \o -> do
      o' <- traverse genJsonRepr o
      extra <- arrayOf do
        key <- arbitrary
        value <- fromString <$> arbitrary
        pure { key: "extra_" <> key, value }
      pure
        ( fromObject
            ( FO.fromFoldable (map (\e -> Tuple e.key e.value) extra) `FO.union`
                o'
            )
        )
  )
  j

jsonReprLaws
  :: forall a
   . HasJsonCodec a
  => Eq a
  => Show a
  => String
  -> Gen a
  -> TestSuite
jsonReprLaws name gen = do
  test (name <> ": decodes to the original value") do
    quickCheck do
      a <- gen
      s <- genJsonRepr (encodeJson a)
      pure (decodeJson s === Right a)
  test (name <> ": re-encoding does not change the decoded value") do
    quickCheck do
      a <- gen
      s <- genJsonRepr (encodeJson a)
      pure
        ( ( (decodeJson s :: Either JsonDecodeError a) >>=
              (decodeJson <<< encodeJson)
          ) === (decodeJson s :: Either JsonDecodeError a)
        )

textReprLaws
  :: forall a
   . HasTextCodec a
  => Eq a
  => Show a
  => String
  -> Gen a
  -> (a -> Gen String)
  -> TestSuite
textReprLaws name gen genRepr = do
  test (name <> ": decodes to the original value") do
    quickCheck do
      a <- gen
      s <- genRepr a
      pure (decodeText s === Right a)
  test (name <> ": re-encoding does not change the decoded value") do
    quickCheck do
      a <- gen
      s <- genRepr a
      pure
        ( ((decodeText s :: Either String a) >>= (decodeText <<< encodeText))
            === (decodeText s :: Either String a)
        )

main :: Effect Unit
main = runTest do
  suite "codec" do
    suite "value side" do
      test "json Int" do quickCheck (jsonLaw :: Int -> Result)
      test "json Number" do quickCheck (jsonLaw :: Number -> Result)
      test "json String" do quickCheck (jsonLaw :: String -> Result)
      test "json Boolean" do quickCheck (jsonLaw :: Boolean -> Result)
      test "json Array Int" do quickCheck (jsonLaw :: Array Int -> Result)
      test "json record" do quickCheck (jsonLaw <$> genUser)
      test "text Int" do quickCheck (textLaw :: Int -> Result)
      test "text Number" do quickCheck (textLaw :: Number -> Result)
      test "text String" do quickCheck (textLaw :: String -> Result)
      test "text Boolean" do quickCheck (textLaw :: Boolean -> Result)
    suite "representation side" do
      textReprLaws "text Int" arbitrary genIntRepr
      textReprLaws "text Number" arbitrary genNumberRepr
      jsonReprLaws "json record" genUser
      jsonReprLaws "json Array record" (arrayOf genUser)
    suite "decode failures" do
      test "text Int" do
        Assert.equal (Left "Invalid integer: abc")
          (decodeText "abc" :: Either String Int)
      test "text Number" do
        Assert.equal (Left "Invalid number: abc")
          (decodeText "abc" :: Either String Number)
      test "text Number with trailing garbage" do
        Assert.equal (Left "Invalid number:   1.2 ??")
          (decodeText "  1.2 ??" :: Either String Number)
      test "text Number Infinity" do
        Assert.equal (Left "Invalid number: Infinity")
          (decodeText "Infinity" :: Either String Number)
      test "text Boolean" do
        Assert.equal (Left "Invalid boolean: yes")
          (decodeText "yes" :: Either String Boolean)
      test "json Int from a string" do
        Assert.equal true
          (isLeft (decodeJson (fromString "1") :: Either JsonDecodeError Int))
  suite "response" do
    let
      ok = toResponse
        ( respond @"ok" taro
            :: Variant (ok :: WithStatus 200 User | NotFoundErr ())
        )
      notFound = toResponse
        ( respond @"notFound" { message: "no user" }
            :: Variant (ok :: WithStatus 200 User | NotFoundErr ())
        )
    test "status" do
      Assert.equal 200 ok.status
      Assert.equal 404 notFound.status
    test "body" do
      assertJsonBody """{"name":"taro","age":20}""" ok
      assertJsonBody """{"message":"no user"}""" notFound
    test "content type" do
      Assert.equal [ Tuple "Content-Type" "application/json" ] ok.headers
