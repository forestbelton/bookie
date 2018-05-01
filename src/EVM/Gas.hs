module EVM.Gas
    ( GasBucket(..)
    , GasCost(..)
    ) where

import EVM.Insn
import Data.Monoid

data GasBucket
    = GZero
    | GBase
    | GVeryLow
    | GLow
    | GMid
    | GHigh
    | GExtCode
    | GBalance

data GasCost
    = CostBucket GasBucket
    | CostImmediate Integer
    | CostAdd GasCost GasCost

instance Monoid GasCost where
    mempty        = CostImmediate 0
    a `mappend` b = CostAdd a b
