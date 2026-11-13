# Design

## Scope

- The API is described by a type-level spec
- Response encoding is included
- Out for now: client derivation, HTTPurple compatibility

## Responses

- Responses, errors included, are rows, not ADTs
- The status is written in the spec as `WithStatus code a`, e.g. `WithStatus 200 User`

## Codecs

- Type class methods return bidirectional codecs
- `HasJsonCodec`: codec-argonaut `JsonCodec`, for request and response bodies
- `HasTextCodec`: purescript-codec, for path and query parameters
