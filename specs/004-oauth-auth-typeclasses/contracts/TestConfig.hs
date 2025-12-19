{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- NOTE: Implementation will require these extensions for FR-038 type witness patterns:
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}

{- | Conformance Test Interface Contract

This module specifies the interface for the polymorphic OAuth conformance test suite.
Implementations of OAuthStateStore and AuthBackend can use this interface to verify
their implementations against the same test suite used for the reference implementation.

__PARTIALLY SUPERSEDED (2025-12-17)__: Phase 14 removes the
@AuthBackendUserId m ~ OAuthUserId m@ type equality constraint from
'oauthConformanceSpec'. Only @AuthBackendUser m ~ OAuthUser m@ is required.
See plan.md Phase 14 for updated design.

== Usage

Third-party implementers provide a 'TestConfig' that wires up their implementation:

@
myPostgresTestConfig :: IO (TestConfig MyPostgresM)
myPostgresTestConfig = do
  pool <- createTestPool
  let makeApp = do
        env <- mkEnv pool
        let app = serveWithContext api ctx (hoistServer api (runMyM env) server)
        timeRef <- newIORef defaultTime
        return (app, \\dt -> modifyIORef timeRef (addUTCTime dt))
  return TestConfig
    { tcMakeApp = makeApp
    , tcRunM = runMyPostgresM pool
    , tcCredentials = TestCredentials "testuser" "testpass"
    }

spec :: Spec
spec = do
  config <- runIO myPostgresTestConfig
  oauthConformanceSpec config
@

== Design Principles

1. Type Coherence: The same type variable @m@ flows to both 'tcMakeApp' and
   the polymorphic spec, ensuring the test application and assertions use
   the same monad implementation.

2. Abstract Time Control: The 'advanceTime' callback abstracts over time
   advancement mechanism. Reference implementation uses TVar; PostgreSQL
   implementation might manipulate database timestamps.

3. HTTP-Based Setup: Helper combinators issue real HTTP requests for true
   black-box testing. No direct state manipulation.

4. Dual Isolation: Tests support both fresh-app-per-test (strict isolation)
   and shared-app-with-unique-IDs (performance optimization) strategies.

5. Type Witness Patterns (FR-038): Type-directed polymorphic test helpers
   MUST use @TypeApplications@ (@\@Type@) or @Proxy \@Type@ for type witnesses.
   NEVER use @undefined :: Type@. Example:

   @
   -- CORRECT: Use \@Type syntax
   httpApiRoundTrip \@ClientId

   -- CORRECT: Use Proxy \@Type
   httpApiRoundTrip (Proxy \@ClientId)

   -- PROHIBITED: Never use undefined
   httpApiRoundTrip (undefined :: ClientId)  -- DON'T DO THIS
   @
-}
module MCP.Server.OAuth.Test.Internal (
    -- * Test Configuration
    TestConfig (..),
    TestCredentials (..),

    -- * Conformance Suite
    oauthConformanceSpec,

    -- * Helper Combinators
    withRegisteredClient,
    withAuthorizedUser,
    withAccessToken,

    -- * Time Control
    advanceTestTime,

    -- * Utilities
    extractSessionCookie,
    extractCodeFromRedirect,
    generatePKCE,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.Wai (Application)
import Test.Hspec (Spec)
import Test.Hspec.Wai (WaiSession)

import Servant.OAuth2.IDP.Types (AccessTokenId, AuthCodeId, ClientId)

{- | Configuration for the polymorphic OAuth conformance test suite.

Implementations provide this record to run the conformance suite against
their OAuthStateStore and AuthBackend implementations.

== Type Parameter

The @m@ parameter is the implementation's monad stack. It must satisfy:

* 'OAuthStateStore' m
* 'AuthBackend' m
* 'MonadTime' m
* 'AuthBackendUser m ~ OAuthUser m' (Phase 8, FR-039)
* 'AuthBackendUserId m ~ OAuthUserId m' (Phase 8, FR-039)
* 'ToJWT (OAuthUser m)' (Phase 8, FR-040 - for token generation)

== Invariants

* 'tcMakeApp' MUST return a fresh Application with clean OAuth state
* 'tcCredentials' MUST be valid for the AuthBackend wired into the Application
* The @advanceTime@ callback returned by 'tcMakeApp' MUST affect the
  MonadTime instance used by the Application
-}
data TestConfig (m :: Type -> Type) = TestConfig
    { tcMakeApp :: IO (Application, NominalDiffTime -> IO ())
    {- ^ Create a WAI Application and time control handle.

    The Application should be wired up with:
    * Fresh OAuth state (no pre-existing clients, tokens, etc.)
    * The AuthBackend configured to accept 'tcCredentials'
    * A controllable MonadTime implementation

    The returned callback advances time by the given duration,
    affecting expiry checks in the Application's MonadTime.
    -}
    , tcRunM :: forall a. m a -> IO a
    {- ^ Natural transformation to execute the polymorphic monad.

    Used internally to verify typeclass law properties. The same @m@
    that parameterizes this TestConfig is used here.
    -}
    , tcCredentials :: TestCredentials
    {- ^ Valid credentials for OAuth login flow tests.

    The AuthBackend in the Application MUST accept these credentials.
    For the reference implementation, this is (demo, demo123).
    -}
    }

{- | Valid test credentials for OAuth login flow.

Used by helper combinators to complete the authorization flow.
Invalid credential tests use obviously wrong values (empty password,
nonexistent user like @__invalid_user__@).
-}
data TestCredentials = TestCredentials
    { tcUsername :: Text
    -- ^ Username that the AuthBackend accepts
    , tcPassword :: Text
    -- ^ Password that the AuthBackend accepts
    }
    deriving (Show, Eq)

{- | Polymorphic OAuth conformance test suite.

Runs a comprehensive suite of HTTP-level black-box tests covering:

* Client registration (POST /register)
* Authorization endpoint (GET /authorize)
* Login flow (POST /login)
* Token exchange (POST /token with authorization_code)
* Token refresh (POST /token with refresh_token)
* Error cases (invalid client, credentials, PKCE, malformed requests)
* Expiry behavior (auth codes, sessions, tokens)

== Test Isolation

Tests support two isolation strategies:

1. __Fresh Application per test__: Use hspec's @around@ to create a new
   Application for each @it@ block. Strict isolation, slightly slower.

2. __Shared Application with unique IDs__: Reuse Application across tests
   but use UUID-based client IDs to avoid collisions. Faster but requires
   careful test design.

The conformance suite uses strategy 1 for expiry tests (which modify time)
and strategy 2 for stateless tests (error cases, validation).

== Type Equality Constraints (Phase 8, FR-039)

The spec requires type equality between user types from both typeclasses,
ensuring handlers can flow user identity from authentication to token storage.
-}
oauthConformanceSpec ::
    forall m.
    ( OAuthStateStore m
    , AuthBackend m
    , MonadTime m
    , AuthBackendUser m ~ OAuthUser m -- Phase 8: type equality
    , AuthBackendUserId m ~ OAuthUserId m -- Phase 8: type equality
    , ToJWT (OAuthUser m) -- Phase 8: JWT at operation level
    ) =>
    TestConfig m ->
    Spec
oauthConformanceSpec = undefined -- Implementation in MCP.Server.OAuth.Test.Internal

{- | Register a test client and run an action with its ID.

Issues a real POST /register request within WaiSession.
Uses a UUID-based client name to avoid collisions when tests share state.

== Example

@
it "can register and authorize" $ do
  withRegisteredClient config $ \\clientId -> do
    -- clientId is now a valid registered client
    get ("/authorize?client_id=" <> unClientId clientId <> "&...")
      \`shouldRespondWith\` 200
@
-}
withRegisteredClient ::
    TestConfig m ->
    (ClientId -> WaiSession st a) ->
    WaiSession st a
withRegisteredClient = undefined

{- | Complete authorization flow up to obtaining an authorization code.

Performs the full OAuth authorization flow:
1. GET /authorize with PKCE challenge
2. Extract session cookie
3. POST /login with valid credentials and approval
4. Extract authorization code from redirect Location header

== Example

@
it "can exchange code for token" $ do
  withRegisteredClient config $ \\clientId -> do
    withAuthorizedUser config clientId $ \\code -> do
      post "/token" (tokenExchangeBody clientId code verifier)
        \`shouldRespondWith\` 200
@
-}
withAuthorizedUser ::
    TestConfig m ->
    ClientId ->
    (AuthCodeId -> WaiSession st a) ->
    WaiSession st a
withAuthorizedUser = undefined

{- | Complete full OAuth flow up to obtaining an access token.

Builds on 'withAuthorizedUser' to also exchange the code for tokens.
-}
withAccessToken ::
    TestConfig m ->
    ClientId ->
    (AccessTokenId -> WaiSession st a) ->
    WaiSession st a
withAccessToken = undefined

{- | Advance time in tests for expiry testing.

Wrapper around the advanceTime callback from 'tcMakeApp'.
Use within @liftIO@ in WaiSession context.

== Example

@
it "rejects expired auth codes" $ \\(app, advanceTime) -> do
  with (return app) $ do
    withAuthorizedUser config clientId $ \\code -> do
      liftIO $ advanceTestTime advanceTime (11 * 60)  -- 11 minutes
      post "/token" body \`shouldRespondWith\` 400
@
-}
advanceTestTime :: (NominalDiffTime -> IO ()) -> NominalDiffTime -> IO ()
advanceTestTime = id

-- | Extract session cookie from Set-Cookie header.
extractSessionCookie :: a -> Maybe Text
extractSessionCookie = undefined

-- | Extract authorization code from redirect Location header.
extractCodeFromRedirect :: Text -> Maybe Text
extractCodeFromRedirect = undefined

{- | Generate PKCE code verifier and challenge pair.

Returns (verifier, challenge) where challenge is S256 hash of verifier.
-}
generatePKCE :: IO (Text, Text)
generatePKCE = undefined
