{- |
Module      : Servant.OAuth2.IDP.PKCE
Description : PKCE (Proof Key for Code Exchange) implementation per RFC 7636
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

PKCE implementation for OAuth 2.1 authorization code flow with PKCE extension.
All functions use domain newtypes from Servant.OAuth2.IDP.Types for type safety.

Per RFC 7636, PKCE prevents authorization code interception attacks by requiring
the client to prove possession of the code verifier that corresponds to the
code challenge sent during authorization.
-}
module Servant.OAuth2.IDP.PKCE (
    -- * Code Verifier Generation
    generateCodeVerifier,

    -- * Code Challenge Computation
    generateCodeChallenge,

    -- * Validation
    validateCodeVerifier,
) where

import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Random (getRandomBytes)
import Data.ByteArray (constEq, convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.Text.Encoding qualified as TE

import Servant.OAuth2.IDP.Types (CodeChallenge, CodeVerifier, mkCodeChallenge, mkCodeVerifier, unCodeChallenge, unCodeVerifier)

{- | Generate a cryptographically secure code verifier for PKCE.

Returns a 'CodeVerifier' with 256 bits of entropy (32 bytes),
base64url-encoded to 43 characters (no padding).

Per RFC 7636 Section 4.1:
- MUST have minimum entropy of 256 bits
- MUST be between 43-128 characters
- MUST use unreserved characters: [A-Z] / [a-z] / [0-9] / "-" / "." / "_" / "~"

Implementation uses cryptonite's 'getRandomBytes' for cryptographic randomness.
-}
generateCodeVerifier :: IO CodeVerifier
generateCodeVerifier = do
    bytes <- getRandomBytes 32 -- 32 bytes = 256 bits entropy
    -- Base64URL encoding always produces valid UTF-8, so decodeUtf8' cannot fail here
    case TE.decodeUtf8' $ B64URL.encodeUnpadded bytes of
        Right encoded -> case mkCodeVerifier encoded of
            Just cv -> pure cv
            Nothing -> error "Impossible: 32-byte base64url encoding produced invalid CodeVerifier"
        Left err -> error $ "Impossible: base64url encoding produced invalid UTF-8: " ++ show err

{- | Generate code challenge from verifier using SHA256 (S256 method).

Per RFC 7636 Section 4.2:
- code_challenge = BASE64URL(SHA256(ASCII(code_verifier)))

Takes a 'CodeVerifier' and returns the corresponding 'CodeChallenge'.
The transformation is deterministic (same verifier always produces same challenge).
-}
generateCodeChallenge :: CodeVerifier -> CodeChallenge
generateCodeChallenge verifier =
    let verifierBytes = TE.encodeUtf8 (unCodeVerifier verifier)
        challengeHash = hashWith SHA256 verifierBytes
        challengeBytes = convert challengeHash :: ByteString
        -- Base64URL encoding always produces valid UTF-8, so decodeUtf8' cannot fail here
        encoded = case TE.decodeUtf8' $ B64URL.encodeUnpadded challengeBytes of
            Right txt -> txt
            Left err -> error $ "Impossible: base64url encoding produced invalid UTF-8: " ++ show err
     in case mkCodeChallenge encoded of
            Just cc -> cc
            Nothing -> error "Impossible: SHA256 base64url encoding produced invalid CodeChallenge"

{- | Validate PKCE code verifier against challenge.

Returns 'True' if the verifier matches the challenge, 'False' otherwise.

Validation per RFC 7636 Section 4.6:
- Compute challenge from verifier: BASE64URL(SHA256(ASCII(code_verifier)))
- Compare with stored challenge using constant-time comparison

This function is used during token exchange to verify the client possesses
the original code verifier.
-}
validateCodeVerifier :: CodeVerifier -> CodeChallenge -> Bool
validateCodeVerifier verifier challenge =
    let computed = generateCodeChallenge verifier
        computedBytes = TE.encodeUtf8 $ unCodeChallenge computed
        challengeBytes = TE.encodeUtf8 $ unCodeChallenge challenge
     in constEq computedBytes challengeBytes
