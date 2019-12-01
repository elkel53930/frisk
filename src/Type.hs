{-# LANGUAGE QuasiQuotes #-}
module Type where

import Data.String.Interpolate
import qualified Data.Set as Set
import Data.Semigroup

data Event = A | B | C | D | E deriving (Show, Eq, Ord)
data State = P0 | P1 | P2 | P3 | P4
           | Q0 | Q1 | Q2 | Q3
           | Comp [State] deriving (Show, Eq, Ord)
type Generator a b = a -> [(b, a)]
type Queue a = [a]
type Hash a = Set.Set a
type Logs a b = [(b,a,a)] 

type Thread = Generator State Event
type UnfoldedThread = Generator States Event
type Unfolded = Set.Set (Event,States,States)
type States = [State]
type Filename = String
{-
thread_P :: Thread
thread_P P0 = zipWith (,) [A, B, C, E] [P1, P2, P3, P4]
thread_P p
    | p `elem` [P1, P2, P3, P4] = []
    | otherwise = undefined
-}
thread_P :: Thread
thread_P P0 = zipWith (,) [A, B, C, E] [P1, P2, P3, P4]
thread_P P1 = [(B,P2),(D,P0)]
thread_P P2 = [(C,P3),(D,P0)]
thread_P P3 = [(E,P4),(D,P0)]
thread_P P4 = [(D,P0),(D,P0)]
thread_P _  = undefined

thread_Q :: Thread
thread_Q Q0 = zipWith (,) [A, C, D] [Q1, Q2, Q3]
thread_Q q
    | q `elem` [Q1, Q2, Q3] = []
    | otherwise = undefined
