{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Generators
Description : QuickCheck Arbitrary instances for MCP OAuth types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides QuickCheck Arbitrary instances for all OAuth domain types
used in property-based testing. These instances generate valid, well-formed
test data that respects the smart constructor validation rules.

== Design Principles

* Generate valid data by default (use smart constructors)
* Respect invariants enforced by validation rules
* Provide realistic test cases (e.g., valid URIs, proper UUID formats)

== Testing with These Generators

@
import Test.QuickCheck
import Generators ()  -- Import orphan instances

prop_example :: AuthCodeId -> Bool
prop_example authCodeId = not (T.null (unAuthCodeId authCodeId))

main :: IO ()
main = quickCheck prop_example
@
-}
module Generators () where

import Data.ByteArray qualified as BA
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Network.URI (URI, parseURI)
import Test.QuickCheck

-- OAuth domain types
import Servant.OAuth2.IDP.Auth.Backend (
    CredentialStore (..),
    HashedPassword,
    PlaintextPassword (..),
    Salt (..),
    Username (..),
    mkHashedPassword,
    mkPlaintextPassword,
    mkUsername,
 )
import Servant.OAuth2.IDP.Auth.Demo (AuthUser (..))
import Servant.OAuth2.IDP.Types (
    AccessTokenId (..),
    AuthCodeId (..),
    AuthorizationCode (..),
    ClientAuthMethod (..),
    ClientId (..),
    ClientInfo (..),
    CodeChallenge (..),
    CodeChallengeMethod (..),
    CodeVerifier (..),
    GrantType (..),
    OAuthState (..),
    PendingAuthorization (..),
    RedirectUri (..),
    RefreshTokenId (..),
    ResourceIndicator (..),
    ResponseType (..),
    Scope (..),
    SessionId (..),
    UserId (..),
    mkCodeChallenge,
    mkCodeVerifier,
    mkScope,
 )

-- ============================================================================
-- Identity Newtypes (non-empty text)
-- ============================================================================

instance Arbitrary AuthCodeId where
    arbitrary = AuthCodeId . T.pack . getNonEmpty <$> arbitrary
    shrink (AuthCodeId t) = [AuthCodeId (T.pack s) | s <- shrink (T.unpack t), not (null s)]

instance Arbitrary ClientId where
    arbitrary = ClientId . T.pack . getNonEmpty <$> arbitrary
    shrink (ClientId t) = [ClientId (T.pack s) | s <- shrink (T.unpack t), not (null s)]

-- SessionId requires UUID format: 8-4-4-4-12 hex pattern
instance Arbitrary SessionId where
    arbitrary = SessionId <$> genUUID
      where
        genUUID :: Gen Text
        genUUID = do
            p1 <- genHex 8
            p2 <- genHex 4
            p3 <- genHex 4
            p4 <- genHex 4
            p5 <- genHex 12
            pure $ T.intercalate "-" [p1, p2, p3, p4, p5]

        genHex :: Int -> Gen Text
        genHex n = T.pack <$> vectorOf n (elements "0123456789abcdef")

    shrink _ = [] -- Don't shrink UUIDs (they must maintain format)

instance Arbitrary UserId where
    arbitrary = UserId . T.pack . getNonEmpty <$> arbitrary
    shrink (UserId t) = [UserId (T.pack s) | s <- shrink (T.unpack t), not (null s)]

instance Arbitrary RefreshTokenId where
    arbitrary = RefreshTokenId . T.pack . getNonEmpty <$> arbitrary
    shrink (RefreshTokenId t) = [RefreshTokenId (T.pack s) | s <- shrink (T.unpack t), not (null s)]

instance Arbitrary AccessTokenId where
    arbitrary = AccessTokenId . T.pack . getNonEmpty <$> arbitrary
    shrink (AccessTokenId t) = [AccessTokenId (T.pack s) | s <- shrink (T.unpack t), not (null s)]

instance Arbitrary Username where
    arbitrary = do
        -- Generate valid usernames (alphanumeric + underscore/dot)
        let validChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_', '.']
        len <- chooseInt (1, 20) -- Reasonable username length
        username <- T.pack <$> vectorOf len (elements validChars)
        maybe arbitrary pure (mkUsername username) -- Retry if validation fails (shouldn't happen)
    shrink (Username t) = [u | s <- shrink (T.unpack t), not (null s), Just u <- [mkUsername (T.pack s)]]

-- ============================================================================
-- Value Newtypes
-- ============================================================================

-- RedirectUri: generate valid URIs (https:// or http://localhost)
instance Arbitrary RedirectUri where
    arbitrary = do
        scheme <- elements ["https", "http"]
        host <-
            if scheme == "http"
                then elements ["localhost", "127.0.0.1"]
                else genHostname
        port <- chooseInt (1024, 65535)
        path <- genPath
        let uriStr = scheme ++ "://" ++ host ++ ":" ++ show port ++ path
        case parseURI uriStr of
            Just uri -> pure (RedirectUri uri)
            Nothing -> arbitrary -- Retry if URI parsing fails
      where
        genHostname :: Gen String
        genHostname = do
            subdomain <- listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9']))
            domain <- elements ["example.com", "test.org", "app.io"]
            pure $ subdomain ++ "." ++ domain

        genPath :: Gen String
        genPath = do
            segments <- listOf (listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '_'])))
            pure $ concatMap ("/" ++) segments

    shrink _ = [] -- Don't shrink URIs (complex validation)

-- Scope: non-empty text without whitespace
instance Arbitrary Scope where
    arbitrary = do
        -- Generate valid scope values (alphanumeric + colon/dot)
        let validChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [':', '.', '-', '_']
        len <- chooseInt (1, 30)
        scopeText <- T.pack <$> vectorOf len (elements validChars)
        maybe arbitrary pure (mkScope scopeText) -- Retry if validation fails
    shrink (Scope t) = [s | str <- shrink (T.unpack t), not (null str), Just s <- [mkScope (T.pack str)]]

-- CodeChallenge: base64url charset, 43-128 chars
instance Arbitrary CodeChallenge where
    arbitrary = do
        len <- chooseInt (43, 128) -- PKCE spec: 43-128 characters
        let base64urlChars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '_']
        challengeText <- T.pack <$> vectorOf len (elements base64urlChars)
        maybe arbitrary pure (mkCodeChallenge challengeText) -- Retry if validation fails
    shrink _ = [] -- Don't shrink (must maintain length constraints)

-- CodeVerifier: unreserved chars per RFC 7636, 43-128 chars
instance Arbitrary CodeVerifier where
    arbitrary = do
        len <- chooseInt (43, 128) -- PKCE spec: 43-128 characters
        -- RFC 7636: unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
        let unreservedChars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '.', '_', '~']
        verifierText <- T.pack <$> vectorOf len (elements unreservedChars)
        maybe arbitrary pure (mkCodeVerifier verifierText) -- Retry if validation fails
    shrink _ = [] -- Don't shrink (must maintain length constraints)

-- OAuthState: opaque CSRF protection token (any non-empty text)
instance Arbitrary OAuthState where
    arbitrary = OAuthState . T.pack . getNonEmpty <$> arbitrary
    shrink (OAuthState t) = [OAuthState (T.pack s) | s <- shrink (T.unpack t), not (null s)]

-- ResourceIndicator: RFC 8707 resource indicator (any non-empty text, typically a URI)
instance Arbitrary ResourceIndicator where
    arbitrary = ResourceIndicator . T.pack . getNonEmpty <$> arbitrary
    shrink (ResourceIndicator t) = [ResourceIndicator (T.pack s) | s <- shrink (T.unpack t), not (null s)]

-- ============================================================================
-- ADTs (use arbitraryBoundedEnum)
-- ============================================================================

instance Arbitrary CodeChallengeMethod where
    arbitrary = elements [S256, Plain]

instance Arbitrary GrantType where
    arbitrary = elements [GrantAuthorizationCode, GrantRefreshToken, GrantClientCredentials]

instance Arbitrary ResponseType where
    arbitrary = elements [ResponseCode, ResponseToken]

instance Arbitrary ClientAuthMethod where
    arbitrary = elements [AuthNone, AuthClientSecretPost, AuthClientSecretBasic]

-- ============================================================================
-- UTCTime (within reasonable range)
-- ============================================================================

instance Arbitrary UTCTime where
    arbitrary = do
        -- Generate times within 10 years from 2020-01-01
        days <- chooseInt (0, 365 * 10)
        secs <- chooseInt (0, 86400) -- Seconds in a day
        let baseDay = fromGregorian 2020 1 1
        pure $ UTCTime (addDays (fromIntegral days) baseDay) (secondsToDiffTime (fromIntegral secs))
    shrink (UTCTime day time) =
        [UTCTime day' time | day' <- take 5 [addDays (-1) day, addDays 1 day]]

-- ============================================================================
-- Domain Entities
-- ============================================================================

instance (Arbitrary userId) => Arbitrary (AuthorizationCode userId) where
    arbitrary = do
        authCodeId <- arbitrary
        authClientId <- arbitrary
        authRedirectUri <- arbitrary
        authCodeChallenge <- arbitrary
        authCodeChallengeMethod <- arbitrary
        -- Scopes: generate 0-5 scopes
        authScopes <- Set.fromList <$> listOf arbitrary `suchThat` (\xs -> length xs <= 5)
        authUserId <- arbitrary
        authExpiry <- arbitrary
        pure AuthorizationCode{..}

instance Arbitrary ClientInfo where
    arbitrary = do
        clientName <- T.pack . getNonEmpty <$> arbitrary
        -- NonEmpty RedirectUris
        headUri <- arbitrary
        tailUris <- listOf arbitrary `suchThat` (\xs -> length xs <= 3)
        let clientRedirectUris = headUri :| tailUris
        -- Grant types: 1-3 grant types
        clientGrantTypes <- Set.fromList <$> listOf1 arbitrary `suchThat` (\xs -> length xs <= 3)
        -- Response types: 1-2 response types
        clientResponseTypes <- Set.fromList <$> listOf1 arbitrary `suchThat` (\xs -> length xs <= 2)
        clientAuthMethod <- arbitrary
        pure ClientInfo{..}

instance Arbitrary PendingAuthorization where
    arbitrary = do
        pendingClientId <- arbitrary
        pendingRedirectUri <- arbitrary
        pendingCodeChallenge <- arbitrary
        pendingCodeChallengeMethod <- arbitrary
        -- Optional scope
        pendingScope <- frequency [(1, pure Nothing), (3, Just . Set.fromList <$> listOf arbitrary `suchThat` (\xs -> length xs <= 5))]
        -- Optional state (OAuthState newtype)
        pendingState <- frequency [(1, pure Nothing), (3, Just . OAuthState . T.pack . getNonEmpty <$> arbitrary)]
        -- Optional resource URI
        pendingResource <- frequency [(1, pure Nothing), (2, Just <$> genResourceURI)]
        pendingCreatedAt <- arbitrary
        pure PendingAuthorization{..}
      where
        genResourceURI :: Gen URI
        genResourceURI = do
            RedirectUri uri <- arbitrary
            pure uri

instance Arbitrary AuthUser where
    arbitrary = do
        userUserId <- arbitrary
        userUserEmail <- frequency [(1, pure Nothing), (3, Just <$> genEmail)]
        userUserName <- frequency [(1, pure Nothing), (3, Just . T.pack . getNonEmpty <$> arbitrary)]
        pure AuthUser{..}
      where
        genEmail :: Gen Text
        genEmail = do
            local <- listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ ['.', '_']))
            domain <- elements ["example.com", "test.org", "mail.io"]
            pure $ T.pack (local ++ "@" ++ domain)

-- ============================================================================
-- Auth Backend Types
-- ============================================================================

-- PlaintextPassword: generate via mkPlaintextPassword (ScrubbedBytes)
instance Arbitrary PlaintextPassword where
    arbitrary = do
        password <- T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['!', '@', '#', '$', '%']))
        pure $ mkPlaintextPassword password
    shrink _ = [] -- Don't shrink passwords (sensitive data)

-- HashedPassword: generate via mkHashedPassword
instance Arbitrary HashedPassword where
    arbitrary = do
        salt <- arbitrary
        mkHashedPassword salt <$> arbitrary
    shrink _ = [] -- Don't shrink hashes

-- Salt: generate random bytes
instance Arbitrary Salt where
    arbitrary = do
        let saltChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
        saltText <- T.pack <$> vectorOf 32 (elements saltChars)
        pure $ Salt (BA.convert (TE.encodeUtf8 saltText))
    shrink _ = [] -- Don't shrink salts

-- CredentialStore: generate map of usernames to hashed passwords
instance Arbitrary CredentialStore where
    arbitrary = do
        salt <- arbitrary
        -- Generate 1-5 users
        users <- listOf1 arbitrary `suchThat` (\xs -> length xs <= 5)
        passwords <- vectorOf (length users) arbitrary
        let credentials = zip users passwords
        let storeCredentials = foldr (\(u, p) m -> Map.insert u (mkHashedPassword salt p) m) Map.empty credentials
        pure CredentialStore{storeCredentials, storeSalt = salt}
    shrink _ = [] -- Don't shrink credential stores (complex)
