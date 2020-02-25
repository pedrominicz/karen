module Solve where

import Syntax
import Unify

import qualified Data.Map as M

type Env = M.Map String UTerm

data State = State
    { goal   :: [Either Term Env]
    , env    :: Env
    , subst  :: Subst
    , next   :: Int
    , choice :: [Choice]
    }

type Choice = ([Either Term Env], Env, Subst)

step :: State -> State
step (State goal env subst next choice) = undefined
