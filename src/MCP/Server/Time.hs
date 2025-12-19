{- |
Module      : MCP.Server.Time
Description : Time abstraction for testable time-dependent operations
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides a thin re-export layer for the @monad-time@ package,
which offers a 'MonadTime' typeclass for abstracting over time operations.

= Usage

The 'MonadTime' typeclass provides 'currentTime' and 'monotonicTime' methods
that work in any monad that implements them. This enables testing time-dependent
code by using a custom monad that provides controlled time values.

== Production use

In production code, use 'IO' which has a 'MonadTime' instance that reads
the system clock:

@
import MCP.Server.Time
import Data.Time (UTCTime)

checkExpiry :: MonadTime m => UTCTime -> m Bool
checkExpiry expiryTime = do
  now <- currentTime
  pure (now < expiryTime)
@

== Testing

In tests, you can implement a custom monad with a fixed or controlled time:

@
import MCP.Server.Time
import Data.Time (UTCTime)
import Control.Monad.Reader (Reader, ask, runReader)

newtype TestTime a = TestTime { runTestTime :: Reader (UTCTime, Double) a }
  deriving (Functor, Applicative, Monad, MonadReader (UTCTime, Double))

instance MonadTime TestTime where
  currentTime = fst <$> ask
  monotonicTime = snd <$> ask

-- Run tests with a specific time
runAtTime :: UTCTime -> Double -> TestTime a -> a
runAtTime t mono = flip runReader (t, mono) . runTestTime
@

= Re-exports

This module re-exports:

* 'MonadTime' typeclass from @Control.Monad.Time@
* 'currentTime' method (returns 'UTCTime')
* 'monotonicTime' method (returns monotonic time as 'Double')

The 'IO' instance is automatically available via the @monad-time@ package.
-}
module MCP.Server.Time (
    -- * Time typeclass
    MonadTime (..),
) where

import Control.Monad.Time (MonadTime (..))
