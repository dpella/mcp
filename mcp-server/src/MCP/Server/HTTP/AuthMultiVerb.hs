{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module:      MCP.Server.HTTP.AuthMultiVerb
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Lets a 'MultiVerb' endpoint sit behind servant-auth's 'Auth' combinator.

'Auth' adds its @Set-Cookie@ headers to every endpoint below it, through the
'AddSetCookieApi' type family and the 'AddSetCookies' class. servant-auth-server
has no instances of them for 'MultiVerb', so an API with a 'MultiVerb' endpoint
under 'Auth' does not compile. This module gives those instances. Each response
of a 'MultiVerb' endpoint gets an optional @Set-Cookie@ header, and the handler
is written as before.

The same instances are proposed to servant-auth-server in
<https://github.com/haskell-servant/servant/pull/1922>. Remove this module when
a servant-auth-server release includes them.
-}
module MCP.Server.HTTP.AuthMultiVerb (
    WithSetCookie (..),
) where

import Data.Kind (Type)
import Data.SOP qualified as SOP
import Servant (Header, Proxy (..), Union)
import Servant.API.MultiVerb (
    AsUnion (..),
    MultiVerb,
    OptHeader,
    ResponseType,
    ResponseTypes,
    WithHeaders,
 )
import Servant.Auth.Server.Internal.AddSetCookie (
    AddSetCookieApi,
    AddSetCookies (..),
    Nat (..),
    SetCookieList (..),
 )
import Web.Cookie (SetCookie)

-- | Every response of a 'MultiVerb' endpoint gets an optional @Set-Cookie@
-- header. The handler gives the cookie together with its result: for an
-- endpoint with one response, as a pair; for an endpoint with several, as
-- 'WithSetCookie'.
type instance
    AddSetCookieApi (MultiVerb method cs as r) =
        MultiVerb method cs (AddSetCookieResponses as) (AddSetCookieMultiVerbResult as r)

-- | The responses of a 'MultiVerb' endpoint, each with an optional
-- @Set-Cookie@ header.
type family AddSetCookieResponses (as :: [Type]) :: [Type] where
    AddSetCookieResponses '[] = '[]
    AddSetCookieResponses (a ': as) =
        WithHeaders '[OptHeader (Header "Set-Cookie" SetCookie)] (ResponseType a, Maybe SetCookie) a
            ': AddSetCookieResponses as

-- | The handler result of a 'MultiVerb' endpoint with the cookie it sets.
-- The result of an endpoint with one response is paired with the cookie,
-- because servant converts such an endpoint's result with its own
-- 'AsUnion' instance.
type family AddSetCookieMultiVerbResult (as :: [Type]) r where
    AddSetCookieMultiVerbResult '[a] r = (r, Maybe SetCookie)
    AddSetCookieMultiVerbResult as r = WithSetCookie as r

-- | The result of a handler for a 'MultiVerb' endpoint with several
-- responses @as@, and the cookie to set on whichever response it gives.
data WithSetCookie (as :: [Type]) r = WithSetCookie (Maybe SetCookie) r

instance
    (bs ~ AddSetCookieResponses as, AsUnion as r, AddSetCookieUnion as) =>
    AsUnion bs (WithSetCookie as r)
    where
    toUnion (WithSetCookie cookie r) =
        addSetCookieUnion (Proxy :: Proxy as) cookie (toUnion @as r)
    fromUnion u =
        let (cookie, u') = dropSetCookieUnion (Proxy :: Proxy as) u
         in WithSetCookie cookie (fromUnion @as u')

-- | Moves a 'MultiVerb' result between the responses @as@ and the same
-- responses with an optional @Set-Cookie@ header.
class AddSetCookieUnion (as :: [Type]) where
    addSetCookieUnion ::
        Proxy as ->
        Maybe SetCookie ->
        Union (ResponseTypes as) ->
        Union (ResponseTypes (AddSetCookieResponses as))
    dropSetCookieUnion ::
        Proxy as ->
        Union (ResponseTypes (AddSetCookieResponses as)) ->
        (Maybe SetCookie, Union (ResponseTypes as))

instance AddSetCookieUnion '[] where
    addSetCookieUnion _ _ u = u
    dropSetCookieUnion _ u = (Nothing, u)

instance (AddSetCookieUnion as) => AddSetCookieUnion (a ': as) where
    addSetCookieUnion _ cookie (SOP.Z (SOP.I x)) = SOP.Z (SOP.I (x, cookie))
    addSetCookieUnion _ cookie (SOP.S rest) =
        SOP.S (addSetCookieUnion (Proxy :: Proxy as) cookie rest)
    dropSetCookieUnion _ (SOP.Z (SOP.I (x, cookie))) = (cookie, SOP.Z (SOP.I x))
    dropSetCookieUnion _ (SOP.S rest) = SOP.S <$> dropSetCookieUnion (Proxy :: Proxy as) rest

-- | A 'MultiVerb' endpoint with one response: the result paired with the cookie.
instance
    {-# OVERLAPPING #-}
    (AddSetCookies n (m old) (m new), Functor m) =>
    AddSetCookies ('S n) (m old) (m (new, Maybe SetCookie))
    where
    addSetCookies (mCookie `SetCookieCons` rest) oldVal =
        (,mCookie) <$> addSetCookies rest oldVal

-- | A 'MultiVerb' endpoint with several responses: the result with the cookie.
instance
    {-# OVERLAPPING #-}
    (AddSetCookies n (m old) (m new), Functor m) =>
    AddSetCookies ('S n) (m old) (m (WithSetCookie as new))
    where
    addSetCookies (mCookie `SetCookieCons` rest) oldVal =
        WithSetCookie mCookie <$> addSetCookies rest oldVal
