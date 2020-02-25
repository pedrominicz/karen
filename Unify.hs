module Unify where

import qualified Data.IntMap as IM

data UTerm
    = UVar Int
    | UTerm String [UTerm]
    deriving (Eq, Show)

type Subst = IM.IntMap UTerm
