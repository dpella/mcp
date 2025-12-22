{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Servant.OAuth2.IDP.Types
Description : OAuth 2.1 domain types for Servant IDP
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides type-safe OAuth 2.1 domain types with smart constructors
for validation. These types form the foundation for the OAuth typeclass
refactoring.
-}
module Servant.OAuth2.IDP.Types (
    -- * Identity Newtypes
    AuthCodeId,
    mkAuthCodeId,
    generateAuthCodeId,
    unAuthCodeId,
    ClientId,
    mkClientId,
    generateClientId,
    unClientId,
    SessionId,
    mkSessionId,
    generateSessionId,
    unSessionId,
    AccessTokenId,
    mkAccessTokenId,
    unAccessTokenId,
    RefreshTokenId,
    mkRefreshTokenId,
    generateRefreshTokenId,
    unRefreshTokenId,
    UserId,
    mkUserId,
    unUserId,

    -- * Value Newtypes
    RedirectUri,
    mkRedirectUri,
    unRedirectUri,
    Scope,
    mkScope,
    unScope,
    parseScopes,
    serializeScopeSet,
    Scopes (..),
    CodeChallenge,
    mkCodeChallenge,
    unCodeChallenge,
    CodeVerifier,
    mkCodeVerifier,
    unCodeVerifier,
    OAuthState (..),
    ResourceIndicator (..),
    ClientSecret,
    mkClientSecret,
    unClientSecret,
    ClientName,
    mkClientName,
    unClientName,
    AccessToken (..),
    TokenType (..),
    RefreshToken (..),
    TokenValidity,
    mkTokenValidity,
    unTokenValidity,

    -- * HTTP Response Newtypes
    RedirectTarget (..),
    SessionCookie (..),

    -- * ADTs
    CodeChallengeMethod (..),
    GrantType (..),
    ResponseType (..),
    ClientAuthMethod (..),
    OAuthGrantType (..),
    oauthGrantTypeToGrantType,
    LoginAction (..),

    -- * Domain Entities
    AuthorizationCode (..),
    ClientInfo (..),
    PendingAuthorization (..),

    -- * QuickCheck Helpers (monomorphic, no orphans)
    arbitraryUTCTime,
    shrinkUTCTime,
) where

import Control.Monad (forM_, guard, when)
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), secondsToDiffTime)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Network.URI (URI, parseURI, uriAuthority, uriRegName, uriScheme, uriToString)
import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, elements, frequency, getNonEmpty, listOf, listOf1, suchThat, vectorOf)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- -----------------------------------------------------------------------------
-- Identity Newtypes
-- -----------------------------------------------------------------------------

-- | Authorization code identifier
newtype AuthCodeId = AuthCodeId {unAuthCodeId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for AuthCodeId
mkAuthCodeId :: Text -> Maybe AuthCodeId
mkAuthCodeId t
    | T.null t = Nothing
    | otherwise = Just (AuthCodeId t)

instance FromHttpApiData AuthCodeId where
    parseUrlPiece t
        | T.null t = Left "AuthCodeId cannot be empty"
        | otherwise = Right (AuthCodeId t)

instance ToHttpApiData AuthCodeId where
    toUrlPiece = unAuthCodeId

-- | OAuth client identifier
newtype ClientId = ClientId {unClientId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for ClientId
mkClientId :: Text -> Maybe ClientId
mkClientId t
    | T.null t = Nothing
    | otherwise = Just (ClientId t)

instance FromHttpApiData ClientId where
    parseUrlPiece t
        | T.null t = Left "ClientId cannot be empty"
        | otherwise = Right (ClientId t)

instance ToHttpApiData ClientId where
    toUrlPiece = unClientId

-- | Session identifier for pending authorizations
newtype SessionId = SessionId {unSessionId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for SessionId (validates UUID format)
mkSessionId :: Text -> Maybe SessionId
mkSessionId t
    | isValidUUID t = Just (SessionId t)
    | otherwise = Nothing
  where
    -- Check for UUID format: 8-4-4-4-12 hex pattern
    isValidUUID uuid =
        let parts = T.splitOn "-" uuid
         in case parts of
                [p1, p2, p3, p4, p5]
                    | T.length p1 == 8
                        && T.length p2 == 4
                        && T.length p3 == 4
                        && T.length p4 == 4
                        && T.length p5 == 12
                        && all (T.all isHexDigit) parts ->
                        True
                _ -> False

instance FromHttpApiData SessionId where
    parseUrlPiece t = case mkSessionId t of
        Just sid -> Right sid
        Nothing -> Left "SessionId must be a valid UUID"

instance ToHttpApiData SessionId where
    toUrlPiece = unSessionId

-- | Access token identifier (JWT-generated)
newtype AccessTokenId = AccessTokenId {unAccessTokenId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for AccessTokenId
mkAccessTokenId :: Text -> Maybe AccessTokenId
mkAccessTokenId t
    | T.null t = Nothing
    | otherwise = Just (AccessTokenId t)

instance FromHttpApiData AccessTokenId where
    parseUrlPiece t
        | T.null t = Left "AccessTokenId cannot be empty"
        | otherwise = Right (AccessTokenId t)

instance ToHttpApiData AccessTokenId where
    toUrlPiece = unAccessTokenId

-- | Refresh token identifier
newtype RefreshTokenId = RefreshTokenId {unRefreshTokenId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for RefreshTokenId
mkRefreshTokenId :: Text -> Maybe RefreshTokenId
mkRefreshTokenId t
    | T.null t = Nothing
    | otherwise = Just (RefreshTokenId t)

instance FromHttpApiData RefreshTokenId where
    parseUrlPiece t
        | T.null t = Left "RefreshTokenId cannot be empty"
        | otherwise = Right (RefreshTokenId t)

instance ToHttpApiData RefreshTokenId where
    toUrlPiece = unRefreshTokenId

-- | User identifier
newtype UserId = UserId {unUserId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for UserId
mkUserId :: Text -> Maybe UserId
mkUserId t
    | T.null t = Nothing
    | otherwise = Just (UserId t)

instance FromHttpApiData UserId where
    parseUrlPiece t
        | T.null t = Left "UserId cannot be empty"
        | otherwise = Right (UserId t)

instance ToHttpApiData UserId where
    toUrlPiece = unUserId

-- -----------------------------------------------------------------------------
-- Crypto-Random ID Generators
-- -----------------------------------------------------------------------------

{- | Generate a cryptographically secure AuthCodeId with prefix
Uses 32 bytes (256 bits) of cryptographic randomness, base16-encoded.
-}
generateAuthCodeId :: Text -> IO AuthCodeId
generateAuthCodeId prefix = do
    randomBytes <- getRandomBytes 32 :: IO ByteString
    randomHex <- case TE.decodeUtf8' $ convertToBase Base16 randomBytes of
        Right t -> return t
        Left err -> error $ "generateAuthCodeId: Base16 encoding produced invalid UTF-8 (impossible): " ++ show err
    let idText = prefix <> randomHex
    case mkAuthCodeId idText of
        Just codeId -> return codeId
        Nothing -> error "generateAuthCodeId: crypto random generation produced empty text (impossible)"

{- | Generate a cryptographically secure ClientId with prefix
Uses 32 bytes (256 bits) of cryptographic randomness, base16-encoded.
-}
generateClientId :: Text -> IO ClientId
generateClientId prefix = do
    randomBytes <- getRandomBytes 32 :: IO ByteString
    randomHex <- case TE.decodeUtf8' $ convertToBase Base16 randomBytes of
        Right t -> return t
        Left err -> error $ "generateClientId: Base16 encoding produced invalid UTF-8 (impossible): " ++ show err
    let idText = prefix <> randomHex
    case mkClientId idText of
        Just clientId -> return clientId
        Nothing -> error "generateClientId: crypto random generation produced empty text (impossible)"

{- | Generate a cryptographically secure SessionId (UUID format)
Uses 16 bytes (128 bits) of cryptographic randomness, formatted as UUID v4.
Format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx where y is [8-9a-b].
-}
generateSessionId :: IO SessionId
generateSessionId = do
    randomBytes <- getRandomBytes 16 :: IO ByteString
    hex <- case TE.decodeUtf8' $ convertToBase Base16 randomBytes of
        Right t -> return t
        Left err -> error $ "generateSessionId: Base16 encoding produced invalid UTF-8 (impossible): " ++ show err
    let
        -- Format as UUID v4: 8-4-4-4-12
        part1 = T.take 8 hex
        part2 = T.take 4 $ T.drop 8 hex
        -- Set version to 4 (UUID v4)
        part3 = "4" <> T.take 3 (T.drop 13 hex)
        -- Set variant bits (10xx in binary, so first hex digit is 8-b)
        variantByte = T.take 1 $ T.drop 16 hex
        part4Variant = case T.unpack variantByte of
            [c]
                | c >= '0' && c <= '3' -> "8"
                | c >= '4' && c <= '7' -> "9"
                | c >= '8' && c <= 'b' -> "a"
                | otherwise -> "b"
            _ -> "8"
        part4 = part4Variant <> T.take 3 (T.drop 17 hex)
        part5 = T.take 12 $ T.drop 20 hex
        uuidText = T.intercalate "-" [part1, part2, part3, part4, part5]
    case mkSessionId uuidText of
        Just sessionId -> return sessionId
        Nothing -> error "generateSessionId: crypto random UUID generation produced invalid format (impossible)"

{- | Generate a cryptographically secure RefreshTokenId with prefix
Uses 32 bytes (256 bits) of cryptographic randomness, base16-encoded.
-}
generateRefreshTokenId :: Text -> IO RefreshTokenId
generateRefreshTokenId prefix = do
    randomBytes <- getRandomBytes 32 :: IO ByteString
    randomHex <- case TE.decodeUtf8' $ convertToBase Base16 randomBytes of
        Right t -> return t
        Left err -> error $ "generateRefreshTokenId: Base16 encoding produced invalid UTF-8 (impossible): " ++ show err
    let idText = prefix <> randomHex
    case mkRefreshTokenId idText of
        Just tokenId -> return tokenId
        Nothing -> error "generateRefreshTokenId: crypto random generation produced empty text (impossible)"

-- -----------------------------------------------------------------------------
-- Value Newtypes
-- -----------------------------------------------------------------------------

-- | OAuth redirect URI
newtype RedirectUri = RedirectUri {unRedirectUri :: URI}
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON RedirectUri where
    parseJSON = withText "RedirectUri" $ \t ->
        case mkRedirectUri t of
            Just uri -> pure uri
            Nothing -> fail $ "Invalid redirect URI: " ++ T.unpack t

instance ToJSON RedirectUri where
    toJSON (RedirectUri uri) = toJSON (show uri)

instance FromHttpApiData RedirectUri where
    parseUrlPiece t = case mkRedirectUri t of
        Just uri -> Right uri
        Nothing -> Left "Redirect URI must use https or http://localhost with exact hostname match"

instance ToHttpApiData RedirectUri where
    toUrlPiece (RedirectUri uri) = T.pack (uriToString id uri "")

{- | Parse IPv4 address "a.b.c.d" into octets (FR-051)
Parse as Integer first to prevent Word8 overflow bypass.
Example: "172.288.0.1" would wrap to (172, 32, 0, 1) without this check.
-}
parseIPv4 :: String -> Maybe (Word8, Word8, Word8, Word8)
parseIPv4 s = do
    -- Parse as Integer to detect overflow before conversion
    (a, b, c, d) <- case reads ("(" ++ map (\c -> if c == '.' then ',' else c) s ++ ")") of
        [((a', b', c', d'), "")] -> Just (a', b', c', d' :: Integer)
        _ -> Nothing
    -- Validate range [0-255] for each octet
    guard $ a >= 0 && a <= 255
    guard $ b >= 0 && b <= 255
    guard $ c >= 0 && c <= 255
    guard $ d >= 0 && d <= 255
    -- Safe to convert after validation
    return (fromInteger a, fromInteger b, fromInteger c, fromInteger d)

{- | Parse IPv6 address from bracketed notation "[addr]" (FR-051)
Extracts the address between brackets and validates basic IPv6 format.
Returns the hexadecimal segments if valid.
-}
parseIPv6 :: String -> Maybe [String]
parseIPv6 hostname = case hostname of
    ('[' : rest) -> case reverse rest of
        (']' : reversedAddr) -> do
            let addr = reverse reversedAddr
            -- Split by ':' and validate it looks like IPv6
            let segments = splitBy ':' addr
            -- IPv6 has at most 8 segments (can have :: for compression)
            guard $ not (null segments) && length segments <= 8
            -- Each segment should be hex digits (or empty for ::)
            guard $ all (\s -> null s || all isHexDigit s) segments
            Just segments
        _ -> Nothing
    _ -> Nothing
  where
    splitBy :: Char -> String -> [String]
    splitBy _ "" = [""]
    splitBy c (x : xs)
        | x == c = "" : splitBy c xs
        | otherwise = case splitBy c xs of
            (y : ys) -> (x : y) : ys
            [] -> [[x]]

{- | Check if IPv6 address is in private ranges (FR-051)
Blocks SSRF attacks to internal IPv6 infrastructure:
- fe80::/10 (link-local addresses, fe80:: to febf::)
- fc00::/7 (unique local addresses, fc00:: to fdff::)
-}
isPrivateIPv6 :: [String] -> Bool
isPrivateIPv6 [] = False
isPrivateIPv6 (firstSeg : _) =
    case firstSeg of
        -- fe80::/10 - link-local (first 10 bits are 1111111010)
        -- fe80 to febf in hex
        ('f' : 'e' : h1 : h2 : rest)
            | null rest || all isHexDigit rest ->
                let val = readHex [h1, h2]
                 in case val of
                        [(n, "")] -> n >= 0x80 && n <= 0xBF
                        _ -> False
        -- fc00::/7 - unique local (first 7 bits are 1111110)
        -- fc00 to fdff in hex
        ('f' : c : _) -> c == 'c' || c == 'd'
        _ -> False
  where
    readHex :: String -> [(Int, String)]
    readHex s = case reads ("0x" ++ s) of
        [(n, "")] -> [(n, "")]
        _ -> []

{- | Check if hostname is a private IP address (FR-051)
Blocks SSRF attacks to internal infrastructure:
- 10.0.0.0/8 (Class A private)
- 172.16.0.0/12 (Class B private)
- 192.168.0.0/16 (Class C private)
- 169.254.0.0/16 (link-local, cloud metadata)
- fe80::/10 (IPv6 link-local)
- fc00::/7 (IPv6 unique local)
-}
isPrivateIP :: String -> Bool
isPrivateIP hostname = case parseIPv4 hostname of
    Just (a, b, _c, _d) ->
        (a == 10) -- 10.0.0.0/8
            || (a == 172 && b >= 16 && b <= 31) -- 172.16.0.0/12
            || (a == 192 && b == 168) -- 192.168.0.0/16
            || (a == 169 && b == 254) -- 169.254.0.0/16
    Nothing -> maybe False isPrivateIPv6 (parseIPv6 hostname)

{- | Smart constructor for RedirectUri (validates https:// or http://localhost)
FR-050: Uses exact hostname matching to prevent SSRF bypass via substring tricks
FR-051: Blocks private IP ranges to prevent SSRF attacks
FR-051: Rejects decimal/hex/octal IP notation bypass vectors
-}
mkRedirectUri :: Text -> Maybe RedirectUri
mkRedirectUri t = do
    uri <- parseURI (T.unpack t)
    auth <- uriAuthority uri
    let hostname = uriRegName auth
        scheme = uriScheme uri

    -- FR-051: Reject decimal/hex/octal IP notation bypass vectors
    -- Only accept valid dotted-quad IPs (a.b.c.d) or domain names
    guard $ not (isNumericIPBypass hostname)

    case scheme of
        "https:" -> do
            -- FR-051: Reject malformed IPs (e.g., octets > 255)
            guard $ not (isMalformedIP hostname)
            -- FR-051: Block private IPs even on HTTPS
            guard $ not (isPrivateIP hostname)
            Just (RedirectUri uri)
        "http:" ->
            -- FR-050: Exact hostname match for localhost exemption
            if hostname `elem` ["localhost", "127.0.0.1", "[::1]"]
                then Just (RedirectUri uri)
                else Nothing
        _ -> Nothing
  where
    -- Check if hostname looks like an IP address but fails to parse
    -- (e.g., octets > 255 like 172.288.0.1)
    isMalformedIP :: String -> Bool
    isMalformedIP host =
        looksLikeIP host && isNothing (parseIPv4 host)
      where
        looksLikeIP h = case filter (== '.') h of
            "..." -> all (\c -> isDigit c || c == '.') h
            _ -> False

    -- Detect numeric IP bypass vectors (decimal, hex, octal)
    -- Returns True if hostname looks like a numeric bypass attempt
    isNumericIPBypass :: String -> Bool
    isNumericIPBypass host =
        -- All digits with no dots (decimal notation like 167772161)
        (all isDigit host && notElem '.' host)
            -- Starts with 0x or 0X (hex notation like 0xa000001)
            || case host of
                ('0' : 'x' : rest) -> not (null rest) && all isHexDigit rest
                ('0' : 'X' : rest) -> not (null rest) && all isHexDigit rest
                _ -> False
            -- Octal in dotted-quad (e.g., 012.0.0.1 where first octet starts with 0)
            || hasOctalOctet host

    -- Check if any octet in dotted-quad starts with 0 (octal notation)
    -- Only applies to strings that look like IP addresses (digits and dots only)
    hasOctalOctet :: String -> Bool
    hasOctalOctet host
        | '.' `elem` host && all (\c -> isDigit c || c == '.') host =
            let octets = splitOn '.' host
             in any startsWithZero octets
        | otherwise = False
      where
        startsWithZero ('0' : rest) = not (null rest) && all isDigit rest
        startsWithZero _ = False

        splitOn :: Char -> String -> [String]
        splitOn _ "" = [""]
        splitOn c (x : xs)
            | x == c = "" : splitOn c xs
            | otherwise = case splitOn c xs of
                (y : ys) -> (x : y) : ys
                [] -> [[x]]

-- | OAuth scope value
newtype Scope = Scope {unScope :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for Scope (non-empty, no whitespace)
mkScope :: Text -> Maybe Scope
mkScope t
    | T.null t = Nothing
    | T.any isSpace t = Nothing
    | otherwise = Just (Scope t)

instance FromHttpApiData Scope where
    parseUrlPiece t
        | T.null t = Left "Scope cannot be empty"
        | T.any isSpace t = Left "Scope cannot contain whitespace"
        | otherwise = Right (Scope t)

instance ToHttpApiData Scope where
    toUrlPiece = unScope

-- | QuickCheck Arbitrary instance for Scope (generates valid scope values)
instance Arbitrary Scope where
    arbitrary = do
        -- Generate valid scope values (alphanumeric + colon/dot)
        let validChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [':', '.', '-', '_']
        len <- chooseInt (1, 30)
        scopeText <- T.pack <$> vectorOf len (elements validChars)
        maybe arbitrary pure (mkScope scopeText) -- Retry if validation fails
    shrink scope = [s | str <- shrink (T.unpack (unScope scope)), not (null str), Just s <- [mkScope (T.pack str)]]

{- | Parse space-delimited scope list into Set of Scope values (RFC 6749 Section 3.3).
Empty string returns empty Set. Invalid scopes cause entire parse to fail.
Filters out empty strings from multiple consecutive spaces.
-}
parseScopes :: Text -> Maybe (Set Scope)
parseScopes t
    | T.null (T.strip t) = Just Set.empty
    | otherwise =
        let scopeTexts = filter (not . T.null) $ map T.strip $ T.splitOn " " t
            scopesMaybe = traverse mkScope scopeTexts
         in fmap Set.fromList scopesMaybe

{- | Serialize Set of Scope values to space-delimited string (RFC 6749 Section 3.3).
Empty Set returns empty string. Order is determined by Set's Ord instance.
-}
serializeScopeSet :: Set Scope -> Text
serializeScopeSet scopes
    | Set.null scopes = ""
    | otherwise = T.intercalate " " (map unScope (Set.toList scopes))

{- | Space-delimited scope list for HTTP API (RFC 6749 Section 3.3).
Wraps a Set of Scope values for use in Servant query parameters.
-}
newtype Scopes = Scopes {unScopes :: Set Scope}
    deriving stock (Eq, Ord, Show, Generic)

instance FromHttpApiData Scopes where
    parseUrlPiece t = case parseScopes t of
        Just scopes -> Right (Scopes scopes)
        Nothing -> Left "Invalid scope list"

instance ToHttpApiData Scopes where
    toUrlPiece (Scopes scopes) = serializeScopeSet scopes

instance ToJSON Scopes where
    toJSON = toJSON . toUrlPiece

instance FromJSON Scopes where
    parseJSON =
        withText "Scopes" $
            either (fail . T.unpack) pure . parseUrlPiece

-- -----------------------------------------------------------------------------
-- PKCE Types
-- -----------------------------------------------------------------------------

-- | PKCE code challenge
newtype CodeChallenge = CodeChallenge {unCodeChallenge :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData CodeChallenge where
    parseUrlPiece t
        | len < 43 || len > 128 = Left "CodeChallenge must be base64url (43-128 chars)"
        | not (T.all isBase64UrlChar t) = Left "CodeChallenge must be base64url (43-128 chars)"
        | otherwise = Right (CodeChallenge t)
      where
        len = T.length t
        isBase64UrlChar c =
            isAsciiUpper c
                || isAsciiLower c
                || isDigit c
                || c == '-'
                || c == '_'

instance ToHttpApiData CodeChallenge where
    toUrlPiece = unCodeChallenge

-- | PKCE code verifier
newtype CodeVerifier = CodeVerifier {unCodeVerifier :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData CodeVerifier where
    parseUrlPiece t
        | len < 43 || len > 128 = Left "CodeVerifier must contain unreserved chars (43-128 chars)"
        | not (T.all isUnreservedChar t) = Left "CodeVerifier must contain unreserved chars (43-128 chars)"
        | otherwise = Right (CodeVerifier t)
      where
        len = T.length t
        -- RFC 7636: unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
        isUnreservedChar c =
            isAsciiUpper c
                || isAsciiLower c
                || isDigit c
                || c == '-'
                || c == '.'
                || c == '_'
                || c == '~'

instance ToHttpApiData CodeVerifier where
    toUrlPiece = unCodeVerifier

-- | Smart constructor for CodeChallenge (base64url charset, 43-128 chars)
mkCodeChallenge :: Text -> Maybe CodeChallenge
mkCodeChallenge t
    | len < 43 || len > 128 = Nothing
    | not (T.all isBase64UrlChar t) = Nothing
    | otherwise = Just (CodeChallenge t)
  where
    len = T.length t
    isBase64UrlChar c =
        isAsciiUpper c
            || isAsciiLower c
            || isDigit c
            || c == '-'
            || c == '_'

-- | Smart constructor for CodeVerifier (unreserved chars per RFC 7636, 43-128 chars)
mkCodeVerifier :: Text -> Maybe CodeVerifier
mkCodeVerifier t
    | len < 43 || len > 128 = Nothing
    | not (T.all isUnreservedChar t) = Nothing
    | otherwise = Just (CodeVerifier t)
  where
    len = T.length t
    -- RFC 7636: unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
    isUnreservedChar c =
        isAsciiUpper c
            || isAsciiLower c
            || isDigit c
            || c == '-'
            || c == '.'
            || c == '_'
            || c == '~'

-- | OAuth state parameter (CSRF protection token per RFC 6749 Section 10.12)
newtype OAuthState = OAuthState {unOAuthState :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData OAuthState where
    parseUrlPiece t
        | T.null t = Left "OAuthState cannot be empty"
        | otherwise = Right (OAuthState t)

instance ToHttpApiData OAuthState where
    toUrlPiece = unOAuthState

-- | OAuth resource parameter (RFC 8707 Resource Indicators)
newtype ResourceIndicator = ResourceIndicator {unResourceIndicator :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData ResourceIndicator where
    parseUrlPiece t
        | T.null t = Left "ResourceIndicator cannot be empty"
        | otherwise = Right (ResourceIndicator t)

instance ToHttpApiData ResourceIndicator where
    toUrlPiece = unResourceIndicator

-- | OAuth client secret (FR-062)
newtype ClientSecret = ClientSecret {unClientSecret :: Text}
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for ClientSecret (allows empty for public clients)
mkClientSecret :: Text -> Maybe ClientSecret
mkClientSecret t = Just (ClientSecret t)

-- | OAuth client name (FR-062)
newtype ClientName = ClientName {unClientName :: Text}
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for ClientName (non-empty required)
mkClientName :: Text -> Maybe ClientName
mkClientName t
    | T.null t = Nothing
    | otherwise = Just (ClientName t)

-- | OAuth access token (FR-063)
newtype AccessToken = AccessToken {unAccessToken :: Text}
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | OAuth token type (FR-063, typically "Bearer")
newtype TokenType = TokenType {unTokenType :: Text}
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | OAuth refresh token (FR-063)
newtype RefreshToken = RefreshToken {unRefreshToken :: Text}
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

{- | Token validity duration (FR-004c)
Denotes what it IS (token validity duration), not field name
-}
newtype TokenValidity = TokenValidity {unTokenValidity :: NominalDiffTime}
    deriving stock (Eq, Show, Generic)

-- | Smart constructor for TokenValidity
mkTokenValidity :: NominalDiffTime -> TokenValidity
mkTokenValidity = TokenValidity

-- Custom ToJSON: outputs integer seconds for OAuth wire format compliance
instance ToJSON TokenValidity where
    toJSON (TokenValidity t) = toJSON (floor t :: Int)

-- -----------------------------------------------------------------------------
-- HTTP Response Newtypes
-- -----------------------------------------------------------------------------

-- | Semantic wrapper for HTTP redirect target (Location header)
newtype RedirectTarget = RedirectTarget {unRedirectTarget :: Text}
    deriving stock (Eq, Show)
    deriving newtype (ToHttpApiData)

-- | Semantic wrapper for HTTP session cookie (Set-Cookie header)
newtype SessionCookie = SessionCookie {unSessionCookie :: Text}
    deriving stock (Eq, Show)
    deriving newtype (ToHttpApiData)

-- -----------------------------------------------------------------------------
-- ADTs
-- -----------------------------------------------------------------------------

-- | PKCE code challenge method
data CodeChallengeMethod
    = S256
    | Plain
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON CodeChallengeMethod where
    parseJSON = withText "CodeChallengeMethod" $ \case
        "S256" -> pure S256
        "plain" -> pure Plain
        other -> fail $ "Invalid code_challenge_method: " ++ T.unpack other

instance ToJSON CodeChallengeMethod where
    toJSON S256 = toJSON ("S256" :: Text)
    toJSON Plain = toJSON ("plain" :: Text)

instance FromHttpApiData CodeChallengeMethod where
    parseUrlPiece = \case
        "S256" -> Right S256
        "plain" -> Right Plain
        other -> Left $ "Invalid code_challenge_method: " <> other

instance ToHttpApiData CodeChallengeMethod where
    toUrlPiece S256 = "S256"
    toUrlPiece Plain = "plain"

-- | OAuth grant type
data GrantType
    = GrantAuthorizationCode
    | GrantRefreshToken
    | GrantClientCredentials
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON GrantType where
    parseJSON = withText "GrantType" $ \case
        "authorization_code" -> pure GrantAuthorizationCode
        "refresh_token" -> pure GrantRefreshToken
        "client_credentials" -> pure GrantClientCredentials
        other -> fail $ "Invalid grant_type: " ++ T.unpack other

instance ToJSON GrantType where
    toJSON GrantAuthorizationCode = toJSON ("authorization_code" :: Text)
    toJSON GrantRefreshToken = toJSON ("refresh_token" :: Text)
    toJSON GrantClientCredentials = toJSON ("client_credentials" :: Text)

instance FromHttpApiData GrantType where
    parseUrlPiece = \case
        "authorization_code" -> Right GrantAuthorizationCode
        "refresh_token" -> Right GrantRefreshToken
        "client_credentials" -> Right GrantClientCredentials
        other -> Left $ "Invalid grant_type: " <> other

instance ToHttpApiData GrantType where
    toUrlPiece GrantAuthorizationCode = "authorization_code"
    toUrlPiece GrantRefreshToken = "refresh_token"
    toUrlPiece GrantClientCredentials = "client_credentials"

-- | OAuth response type
data ResponseType
    = ResponseCode
    | ResponseToken
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON ResponseType where
    parseJSON = withText "ResponseType" $ \case
        "code" -> pure ResponseCode
        "token" -> pure ResponseToken
        other -> fail $ "Invalid response_type: " ++ T.unpack other

instance ToJSON ResponseType where
    toJSON ResponseCode = toJSON ("code" :: Text)
    toJSON ResponseToken = toJSON ("token" :: Text)

instance FromHttpApiData ResponseType where
    parseUrlPiece = \case
        "code" -> Right ResponseCode
        "token" -> Right ResponseToken
        other -> Left $ "Invalid response_type: " <> other

instance ToHttpApiData ResponseType where
    toUrlPiece ResponseCode = "code"
    toUrlPiece ResponseToken = "token"

-- | Client authentication method
data ClientAuthMethod
    = AuthNone
    | AuthClientSecretPost
    | AuthClientSecretBasic
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON ClientAuthMethod where
    parseJSON = withText "ClientAuthMethod" $ \case
        "none" -> pure AuthNone
        "client_secret_post" -> pure AuthClientSecretPost
        "client_secret_basic" -> pure AuthClientSecretBasic
        other -> fail $ "Invalid token_endpoint_auth_method: " ++ T.unpack other

instance ToJSON ClientAuthMethod where
    toJSON AuthNone = toJSON ("none" :: Text)
    toJSON AuthClientSecretPost = toJSON ("client_secret_post" :: Text)
    toJSON AuthClientSecretBasic = toJSON ("client_secret_basic" :: Text)

instance FromHttpApiData ClientAuthMethod where
    parseUrlPiece = \case
        "none" -> Right AuthNone
        "client_secret_post" -> Right AuthClientSecretPost
        "client_secret_basic" -> Right AuthClientSecretBasic
        other -> Left $ "Invalid token_endpoint_auth_method: " <> other

instance ToHttpApiData ClientAuthMethod where
    toUrlPiece AuthNone = "none"
    toUrlPiece AuthClientSecretPost = "client_secret_post"
    toUrlPiece AuthClientSecretBasic = "client_secret_basic"

-- | OAuth grant types (MCP-specific subset, rest TBD)
data OAuthGrantType
    = -- | Authorization code flow for user-based scenarios
      OAuthAuthorizationCode
    | -- | Client credentials flow for application-to-application
      OAuthClientCredentials
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON OAuthGrantType where
    parseJSON = withText "OAuthGrantType" $ \case
        "authorization_code" -> pure OAuthAuthorizationCode
        "client_credentials" -> pure OAuthClientCredentials
        other -> fail $ "Invalid grant type: " ++ T.unpack other

instance ToJSON OAuthGrantType where
    toJSON OAuthAuthorizationCode = toJSON ("authorization_code" :: Text)
    toJSON OAuthClientCredentials = toJSON ("client_credentials" :: Text)

-- | Convert OAuthGrantType to GrantType for metadata endpoints
oauthGrantTypeToGrantType :: OAuthGrantType -> GrantType
oauthGrantTypeToGrantType OAuthAuthorizationCode = GrantAuthorizationCode
oauthGrantTypeToGrantType OAuthClientCredentials = GrantClientCredentials

-- | Login form action (approve or deny authorization)
data LoginAction
    = ActionApprove
    | ActionDeny
    deriving stock (Eq, Show, Generic)

instance FromHttpApiData LoginAction where
    parseUrlPiece "approve" = Right ActionApprove
    parseUrlPiece "deny" = Right ActionDeny
    parseUrlPiece x = Left ("Invalid action: " <> x)

instance ToHttpApiData LoginAction where
    toUrlPiece ActionApprove = "approve"
    toUrlPiece ActionDeny = "deny"

-- -----------------------------------------------------------------------------
-- Domain Entities
-- -----------------------------------------------------------------------------

-- | Authorization code with PKCE
data AuthorizationCode userId = AuthorizationCode
    { authCodeId :: AuthCodeId
    , authClientId :: ClientId
    , authRedirectUri :: RedirectUri
    , authCodeChallenge :: CodeChallenge
    , authCodeChallengeMethod :: CodeChallengeMethod
    , authScopes :: Set Scope
    , authUserId :: userId
    , authExpiry :: UTCTime
    }
    deriving stock (Eq, Show, Generic, Functor)

instance (FromJSON userId) => FromJSON (AuthorizationCode userId) where
    parseJSON = withObject "AuthorizationCode" $ \v -> do
        codeId <- v .: "auth_code_id"
        -- Validate AuthCodeId is non-empty
        when (T.null (unAuthCodeId codeId)) $
            fail "auth_code_id must not be empty"

        clientId <- v .: "auth_client_id"
        -- Validate ClientId is non-empty
        when (T.null (unClientId clientId)) $
            fail "auth_client_id must not be empty"

        redirectUri <- v .: "auth_redirect_uri"

        challengeText <- v .: "auth_code_challenge"
        challenge <- case mkCodeChallenge challengeText of
            Just c -> pure c
            Nothing -> fail "auth_code_challenge must be base64url (43-128 chars)"

        challengeMethod <- v .: "auth_code_challenge_method"

        scopesSet <- v .: "auth_scopes"
        -- Validate all scopes are valid
        forM_ (Set.toList scopesSet) $ \scope ->
            when (T.null (unScope scope) || T.any isSpace (unScope scope)) $
                fail "auth_scopes must contain valid scopes (non-empty, no whitespace)"

        userId <- v .: "auth_user_id"

        expiry <- v .: "auth_expiry"

        pure $ AuthorizationCode codeId clientId redirectUri challenge challengeMethod scopesSet userId expiry

instance (ToJSON userId) => ToJSON (AuthorizationCode userId) where
    toJSON AuthorizationCode{..} =
        object
            [ "auth_code_id" .= authCodeId
            , "auth_client_id" .= authClientId
            , "auth_redirect_uri" .= authRedirectUri
            , "auth_code_challenge" .= authCodeChallenge
            , "auth_code_challenge_method" .= authCodeChallengeMethod
            , "auth_scopes" .= authScopes
            , "auth_user_id" .= authUserId
            , "auth_expiry" .= authExpiry
            ]

-- | Registered OAuth client information
data ClientInfo = ClientInfo
    { clientName :: ClientName
    , clientRedirectUris :: NonEmpty RedirectUri
    , clientGrantTypes :: Set GrantType
    , clientResponseTypes :: Set ResponseType
    , clientAuthMethod :: ClientAuthMethod
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ClientInfo where
    parseJSON = withObject "ClientInfo" $ \v -> do
        nameText <- v .: "client_name"
        name <- case mkClientName nameText of
            Just n -> pure n
            Nothing -> fail "client_name must not be empty"

        uriList <- v .: "client_redirect_uris"
        uris <- case nonEmpty uriList of
            Nothing -> fail "client_redirect_uris must contain at least one URI"
            Just ne -> pure ne

        grantTypes <- v .: "client_grant_types"
        when (Set.null grantTypes) $
            fail "client_grant_types must not be empty"

        responseTypes <- v .: "client_response_types"
        when (Set.null responseTypes) $
            fail "client_response_types must not be empty"

        authMethod <- v .: "client_auth_method"

        pure $ ClientInfo name uris grantTypes responseTypes authMethod

instance ToJSON ClientInfo where
    toJSON ClientInfo{..} =
        object
            [ "client_name" .= unClientName clientName
            , "client_redirect_uris" .= clientRedirectUris
            , "client_grant_types" .= clientGrantTypes
            , "client_response_types" .= clientResponseTypes
            , "client_auth_method" .= clientAuthMethod
            ]

-- | Pending authorization awaiting user authentication
data PendingAuthorization = PendingAuthorization
    { pendingClientId :: ClientId
    , pendingRedirectUri :: RedirectUri
    , pendingCodeChallenge :: CodeChallenge
    , pendingCodeChallengeMethod :: CodeChallengeMethod
    , pendingScope :: Maybe (Set Scope)
    , pendingState :: Maybe OAuthState
    , pendingResource :: Maybe URI
    , pendingCreatedAt :: UTCTime
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON PendingAuthorization where
    parseJSON = withObject "PendingAuthorization" $ \v -> do
        clientId <- v .: "pending_client_id"
        -- Validate ClientId is non-empty
        when (T.null (unClientId clientId)) $
            fail "pending_client_id must not be empty"

        redirectUri <- v .: "pending_redirect_uri"

        challengeText <- v .: "pending_code_challenge"
        challenge <- case mkCodeChallenge challengeText of
            Just c -> pure c
            Nothing -> fail "pending_code_challenge must be base64url (43-128 chars)"

        challengeMethod <- v .: "pending_code_challenge_method"

        scopeMaybe <- v .:? "pending_scope"
        -- Validate scopes if present
        case scopeMaybe of
            Just scopesSet ->
                forM_ (Set.toList scopesSet) $ \scope ->
                    when (T.null (unScope scope) || T.any isSpace (unScope scope)) $
                        fail "pending_scope must contain valid scopes (non-empty, no whitespace)"
            Nothing -> pure ()

        state <- v .:? "pending_state"
        resource <- v .:? "pending_resource" >>= traverse parseURIText
        createdAt <- v .: "pending_created_at"

        pure $ PendingAuthorization clientId redirectUri challenge challengeMethod scopeMaybe state resource createdAt
      where
        parseURIText :: Text -> Parser URI
        parseURIText t = case parseURI (T.unpack t) of
            Just uri -> pure uri
            Nothing -> fail $ "Invalid URI: " ++ T.unpack t

instance ToJSON PendingAuthorization where
    toJSON PendingAuthorization{..} =
        object
            [ "pending_client_id" .= pendingClientId
            , "pending_redirect_uri" .= pendingRedirectUri
            , "pending_code_challenge" .= pendingCodeChallenge
            , "pending_code_challenge_method" .= pendingCodeChallengeMethod
            , "pending_scope" .= pendingScope
            , "pending_state" .= pendingState
            , "pending_resource" .= fmap (T.pack . show) pendingResource
            , "pending_created_at" .= pendingCreatedAt
            ]

-- ============================================================================
-- QuickCheck Arbitrary Instances
-- ============================================================================

{- |
These Arbitrary instances live in the type-defining module to:

1. Have access to constructors for generation (required)
2. Enable QuickCheck as library dependency (dead code elimination removes unused instances)
3. Allow tests to be library consumers using smart constructors only
-}

-- NO ORPHANS!!!!!!!!!!!!!!!!!!!!!!!!
--
-- ============================================================================
-- QuickCheck Helper Functions (monomorphic, no orphan instances)
-- ============================================================================

{- | Generate arbitrary UTCTime within reasonable range (monomorphic, no orphan)
Uses a 10-year range from 2020-01-01 to avoid extreme edge cases.
-}
arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = do
    days <- chooseInt (0, 365 * 10)
    secs <- chooseInt (0, 86400)
    let baseDay = fromGregorian 2020 1 1
    pure $ UTCTime (addDays (fromIntegral days) baseDay) (secondsToDiffTime (fromIntegral secs))

{- | Shrink UTCTime (monomorphic, no orphan)
Shrinks by adjusting the day forward/backward by one day.
-}
shrinkUTCTime :: UTCTime -> [UTCTime]
shrinkUTCTime (UTCTime day time) =
    [UTCTime day' time | day' <- take 5 [addDays (-1) day, addDays 1 day]]

-- ============================================================================
-- Identity Newtypes (non-empty text)
-- ============================================================================

{- HLINT ignore "Avoid partial function" -}
instance Arbitrary AuthCodeId where
    arbitrary = fromJust . mkAuthCodeId . T.pack . getNonEmpty <$> arbitrary
    shrink ac =
        [ fromJust (mkAuthCodeId (T.pack s)) -- Known-good: shrink preserves non-empty
        | s <- shrink (T.unpack (unAuthCodeId ac))
        , not (null s)
        ]

instance Arbitrary ClientId where
    arbitrary = fromJust . mkClientId . T.pack . getNonEmpty <$> arbitrary
    shrink cid =
        [ fromJust (mkClientId (T.pack s)) -- Known-good: shrink preserves non-empty
        | s <- shrink (T.unpack (unClientId cid))
        , not (null s)
        ]

-- SessionId requires UUID format: 8-4-4-4-12 hex pattern
instance Arbitrary SessionId where
    arbitrary = fromJust . mkSessionId <$> genUUID
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
    arbitrary = fromJust . mkUserId . T.pack . getNonEmpty <$> arbitrary
    shrink uid =
        [ fromJust (mkUserId (T.pack s)) -- Known-good: shrink preserves non-empty
        | s <- shrink (T.unpack (unUserId uid))
        , not (null s)
        ]

instance Arbitrary RefreshTokenId where
    arbitrary = fromJust . mkRefreshTokenId . T.pack . getNonEmpty <$> arbitrary
    shrink rt =
        [ fromJust (mkRefreshTokenId (T.pack s)) -- Known-good: shrink preserves non-empty
        | s <- shrink (T.unpack (unRefreshTokenId rt))
        , not (null s)
        ]

instance Arbitrary AccessTokenId where
    arbitrary = fromJust . mkAccessTokenId . T.pack . getNonEmpty <$> arbitrary
    shrink at =
        [ fromJust (mkAccessTokenId (T.pack s)) -- Known-good: shrink preserves non-empty
        | s <- shrink (T.unpack (unAccessTokenId at))
        , not (null s)
        ]

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
        let uriStr = T.pack $ scheme ++ "://" ++ host ++ ":" ++ show port ++ path
        maybe arbitrary pure (mkRedirectUri uriStr) -- Retry if URI parsing fails
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
        authExpiry <- arbitraryUTCTime
        pure AuthorizationCode{..}

instance Arbitrary ClientInfo where
    arbitrary = do
        clientNameText <- T.pack . getNonEmpty <$> arbitrary
        let clientName = case mkClientName clientNameText of
                Just cn -> cn
                Nothing -> error "Types.hs: generated invalid ClientName (should never happen)"
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
        pendingCreatedAt <- arbitraryUTCTime
        pure PendingAuthorization{..}
      where
        genResourceURI :: Gen URI
        genResourceURI = unRedirectUri <$> arbitrary
