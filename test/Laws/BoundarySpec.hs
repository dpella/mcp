{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Laws.BoundarySpec
Description : Property-based tests for Servant boundary instances
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides round-trip property tests for the FromHttpApiData/ToHttpApiData
instances on OAuth domain types. These instances form the boundary layer between
Servant's URL/query parameter parsing and our type-safe domain model.

== What This Tests

The round-trip property ensures that for any value @x@:

@
parseUrlPiece (toUrlPiece x) === Right x
@

This guarantees:

1. **Serialization is reversible**: No information is lost when converting to text
2. **Parsing is consistent**: The parser accepts what the serializer produces
3. **Type safety boundary**: Values that pass through HTTP remain valid

== Tested Types

=== Identity Newtypes

* 'ClientId' - OAuth client identifier
* 'AuthCodeId' - Authorization code identifier
* 'SessionId' - Session identifier (UUID format)
* 'AccessTokenId' - Access token identifier
* 'UserId' - User identifier
* 'RefreshTokenId' - Refresh token identifier

=== Value Newtypes

* 'RedirectUri' - OAuth redirect URI (https:// or http://localhost)
* 'Scope' - OAuth scope value (non-empty, no whitespace)
* 'CodeChallenge' - PKCE code challenge (base64url, 43-128 chars)
* 'CodeVerifier' - PKCE code verifier (unreserved chars, 43-128 chars)

=== ADTs

* 'CodeChallengeMethod' - PKCE challenge method (S256, Plain)
* 'GrantType' - OAuth grant type (authorization_code, refresh_token, client_credentials)
* 'ResponseType' - OAuth response type (code, token)
* 'ClientAuthMethod' - Client authentication method (none, client_secret_post, client_secret_basic)

== Usage

These tests are automatically included in the main test suite:

@
cabal test
@

To run only the boundary tests:

@
cabal test --test-option="-m" --test-option="Servant Boundary Round-trip"
@
-}
module Laws.BoundarySpec (spec) where

import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, (===))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- OAuth domain types (also imports orphan Arbitrary instances)
import Servant.OAuth2.IDP.Types (
    AccessTokenId,
    AuthCodeId,
    ClientAuthMethod,
    ClientId,
    CodeChallenge,
    CodeChallengeMethod,
    CodeVerifier,
    GrantType,
    OAuthState,
    RedirectUri,
    RefreshTokenId,
    ResourceIndicator,
    ResponseType,
    Scope,
    SessionId,
    UserId,
 )

-- | Main spec: round-trip property tests for all boundary instances
spec :: Spec
spec = describe "Servant Boundary Round-trip Laws" $ do
    describe "Identity Newtypes" $ do
        identityRoundTrip @ClientId
        identityRoundTrip @AuthCodeId
        identityRoundTrip @SessionId
        identityRoundTrip @AccessTokenId
        identityRoundTrip @UserId
        identityRoundTrip @RefreshTokenId

    describe "Value Newtypes" $ do
        identityRoundTrip @RedirectUri
        identityRoundTrip @Scope
        identityRoundTrip @CodeChallenge
        identityRoundTrip @CodeVerifier
        identityRoundTrip @OAuthState
        identityRoundTrip @ResourceIndicator

    describe "ADTs" $ do
        identityRoundTrip @CodeChallengeMethod
        identityRoundTrip @GrantType
        identityRoundTrip @ResponseType
        identityRoundTrip @ClientAuthMethod

{- | Generic round-trip property test for any type with FromHttpApiData/ToHttpApiData

This test verifies the fundamental boundary law:

@
parseUrlPiece . toUrlPiece = Right
@

The test is parameterized by a type application to fix the type variable.
The type name for test output is automatically derived using 'Typeable'.

Example usage:

@
identityRoundTrip @ClientId
@
-}
identityRoundTrip ::
    forall a.
    (Eq a, Show a, Arbitrary a, FromHttpApiData a, ToHttpApiData a, Typeable a) =>
    Spec
identityRoundTrip =
    prop (show (typeRep (Proxy @a)) ++ " round-trip") $ \(x :: a) ->
        parseUrlPiece (toUrlPiece x) === Right x
