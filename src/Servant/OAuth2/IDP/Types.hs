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
    AuthCodeId (..),
    mkAuthCodeId,
    ClientId (..),
    mkClientId,
    SessionId (..),
    mkSessionId,
    AccessTokenId (..),
    RefreshTokenId (..),
    mkRefreshTokenId,
    UserId (..),
    mkUserId,

    -- * Value Newtypes
    RedirectUri (..),
    mkRedirectUri,
    Scope (..),
    mkScope,
    parseScopes,
    serializeScopeSet,
    Scopes (..),
    CodeChallenge (..),
    mkCodeChallenge,
    CodeVerifier (..),
    mkCodeVerifier,
    OAuthState (..),
    ResourceIndicator (..),
    ClientSecret (..),
    mkClientSecret,
    ClientName (..),
    mkClientName,
    AccessToken (..),
    TokenType (..),
    RefreshToken (..),

    -- * HTTP Response Newtypes
    RedirectTarget (..),
    SessionCookie (..),

    -- * ADTs
    CodeChallengeMethod (..),
    GrantType (..),
    ResponseType (..),
    ClientAuthMethod (..),

    -- * Domain Entities
    AuthorizationCode (..),
    ClientInfo (..),
    PendingAuthorization (..),

    -- * Error Types
    AuthorizationError (..),
    authorizationErrorToResponse,
    ValidationError (..),
    validationErrorToResponse,
    OAuthErrorResponse (..),
) where

import Control.Monad (forM_, guard, when)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isSpace)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (Status, status400, status401, status403)
import Network.URI (URI, parseURI, uriAuthority, uriRegName, uriScheme, uriToString)
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

-- | Access token identifier (JWT-generated, no smart constructor needed)
newtype AccessTokenId = AccessTokenId {unAccessTokenId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

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
    hasOctalOctet :: String -> Bool
    hasOctalOctet host
        | '.' `elem` host =
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

-- | PKCE code challenge
newtype CodeChallenge = CodeChallenge {unCodeChallenge :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

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

instance FromHttpApiData CodeChallenge where
    parseUrlPiece t = case mkCodeChallenge t of
        Just cc -> Right cc
        Nothing -> Left "CodeChallenge must be base64url (43-128 chars)"

instance ToHttpApiData CodeChallenge where
    toUrlPiece = unCodeChallenge

-- | PKCE code verifier
newtype CodeVerifier = CodeVerifier {unCodeVerifier :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

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

instance FromHttpApiData CodeVerifier where
    parseUrlPiece t = case mkCodeVerifier t of
        Just cv -> Right cv
        Nothing -> Left "CodeVerifier must contain unreserved chars (43-128 chars)"

instance ToHttpApiData CodeVerifier where
    toUrlPiece = unCodeVerifier

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
    { clientName :: Text
    , clientRedirectUris :: NonEmpty RedirectUri
    , clientGrantTypes :: Set GrantType
    , clientResponseTypes :: Set ResponseType
    , clientAuthMethod :: ClientAuthMethod
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ClientInfo where
    parseJSON = withObject "ClientInfo" $ \v -> do
        name <- v .: "client_name"

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
            [ "client_name" .= clientName
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

-- -----------------------------------------------------------------------------
-- Error Types
-- -----------------------------------------------------------------------------

-- | OAuth 2.0 error response per RFC 6749 Section 5.2
data OAuthErrorResponse = OAuthErrorResponse
    { oauthErrorCode :: Text
    , oauthErrorDescription :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON OAuthErrorResponse where
    toJSON OAuthErrorResponse{..} =
        object $
            ("error" .= oauthErrorCode)
                : case oauthErrorDescription of
                    Just desc -> ["error_description" .= desc]
                    Nothing -> []

instance FromJSON OAuthErrorResponse where
    parseJSON = withObject "OAuthErrorResponse" $ \v ->
        OAuthErrorResponse
            <$> v .: "error"
            <*> v .:? "error_description"

{- | OAuth 2.0 authorization errors per RFC 6749 Section 4.1.2.1 and 5.2.
Fixed type (protocol-defined), NOT an associated type.
Safe to expose to clients in OAuth error response format.
-}
data AuthorizationError
    = -- | 400: Missing/invalid parameter
      InvalidRequest Text
    | -- | 401: Client authentication failed
      InvalidClient Text
    | -- | 400: Invalid authorization code/refresh token
      InvalidGrant Text
    | -- | 401: Client not authorized for grant type
      UnauthorizedClient Text
    | -- | 400: Grant type not supported
      UnsupportedGrantType Text
    | -- | 400: Invalid/unknown scope
      InvalidScope Text
    | -- | 403: Resource owner denied request
      AccessDenied Text
    | -- | 400: Authorization code expired
      ExpiredCode
    | -- | 400: Redirect URI doesn't match registered
      InvalidRedirectUri
    | -- | 400: Code verifier doesn't match challenge
      PKCEVerificationFailed
    deriving stock (Eq, Show, Generic)

{- | Map AuthorizationError to HTTP status and OAuth error response.
Per RFC 6749 Section 4.1.2.1 (authorization endpoint errors) and Section 5.2 (token endpoint errors).
-}
authorizationErrorToResponse :: AuthorizationError -> (Status, OAuthErrorResponse)
authorizationErrorToResponse = \case
    InvalidRequest msg -> (status400, OAuthErrorResponse "invalid_request" (Just msg))
    InvalidClient msg -> (status401, OAuthErrorResponse "invalid_client" (Just msg))
    InvalidGrant msg -> (status400, OAuthErrorResponse "invalid_grant" (Just msg))
    UnauthorizedClient msg -> (status401, OAuthErrorResponse "unauthorized_client" (Just msg))
    UnsupportedGrantType msg -> (status400, OAuthErrorResponse "unsupported_grant_type" (Just msg))
    InvalidScope msg -> (status400, OAuthErrorResponse "invalid_scope" (Just msg))
    AccessDenied msg -> (status403, OAuthErrorResponse "access_denied" (Just msg))
    ExpiredCode -> (status400, OAuthErrorResponse "invalid_grant" (Just "Authorization code has expired"))
    InvalidRedirectUri -> (status400, OAuthErrorResponse "invalid_request" (Just "Invalid redirect_uri"))
    PKCEVerificationFailed -> (status400, OAuthErrorResponse "invalid_grant" (Just "PKCE verification failed"))

{- | Semantic validation errors for OAuth handler logic.
Fixed type (not an associated type) - safe to expose to clients.
These are validation failures that pass parsing but violate business rules.
-}
data ValidationError
    = -- | redirect_uri doesn't match registered client
      RedirectUriMismatch ClientId RedirectUri
    | -- | response_type not supported
      UnsupportedResponseType Text
    | -- | client_id not found in registry
      ClientNotRegistered ClientId
    | -- | required scope not present
      MissingRequiredScope Scope
    | -- | state parameter validation failed
      InvalidStateParameter Text
    deriving stock (Eq, Show, Generic)

{- | Map ValidationError to HTTP 400 status with descriptive message.
All validation errors are semantic failures (not parse errors) and map to 400.
-}
validationErrorToResponse :: ValidationError -> (Status, Text)
validationErrorToResponse = \case
    RedirectUriMismatch clientId redirectUri ->
        ( status400
        , "redirect_uri does not match registered URIs for client_id: "
            <> unClientId clientId
            <> " (provided: "
            <> toUrlPiece redirectUri
            <> ")"
        )
    UnsupportedResponseType responseType ->
        (status400, "response_type not supported: " <> responseType)
    ClientNotRegistered clientId ->
        (status400, "client_id not registered: " <> unClientId clientId)
    MissingRequiredScope scope ->
        (status400, "Missing required scope: " <> unScope scope)
    InvalidStateParameter stateValue ->
        (status400, "Invalid state parameter: " <> stateValue)
