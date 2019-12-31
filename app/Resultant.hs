{-# LANGUAGE Rank2Types #-}

module Resultant
  ( Resultant(..)
  , result
  , runResultant
  ) where

import Control.Arrow (second)

import Data.Monoid ((<>))

import Control.Monad.Freer.Internal (Eff, Member, handleRelay, send)

-- | Resultant effects - send outputs to an effect environment.
data Resultant w r where
  Result :: w -> Resultant w ()

-- | Send a change to the attached environment.
result :: forall w effs. Member (Resultant w) effs => w -> Eff effs ()
result w = send (Result w)

-- | Handler for 'Resultant' effects.
runResultant :: forall w effs a. Monoid w => Eff (Resultant w ': effs) a -> Eff effs (a, w)
runResultant =
    handleRelay
    (\a -> pure (a, mempty))
    $ \(Result w) k -> second (w <>) <$> k ()

