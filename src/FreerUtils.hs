{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module FreerUtils (reinterpret4) where

import Control.Monad (when, unless)
import Control.Monad.Freer


-- | Like 'reinterpret', but encodes the @f@ effect in /four/ new effects
-- instead of just one.
reinterpret4
  :: forall f g h i j effs
   . (f ~> Eff (g ': h ': i ': j ': effs))
  -> Eff (f ': effs) ~> Eff (g ': h ': i ': j ': effs)
reinterpret4 = reinterpretN @[g, h, i, j]
{-# INLINE reinterpret4 #-}

-- | Like 'reinterpret', but encodes the @f@ effect in /five/ new effects
-- instead of just one.
reinterpret5
  :: forall f g h i j k effs
   . (f ~> Eff (g ': h ': i ': j ': k ': effs))
  -> Eff (f ': effs) ~> Eff (g ': h ': i ': j ': k ': effs)
reinterpret5 = reinterpretN @[g, h, i, j, k]
{-# INLINE reinterpret5 #-}
