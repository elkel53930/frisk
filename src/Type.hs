{-# LANGUAGE QuasiQuotes #-}
module Type where

import Data.String.Interpolate
import qualified Data.Set as Set
import Data.Semigroup

data Event = A | B | C | D | E deriving (Show, Eq, Ord)
data State = P0 | P1 | P2 | P3 | P4
           | Q0 | Q1 | Q2 | Q3
           | Comp (State, State) deriving (Show, Eq, Ord)
type Queue a = [a]
type Hash a = Set.Set a
type Logs a b = [(b,a,a)] 

thread_P :: State -> [(Event, State)]
thread_P P0 = zipWith (,) [A, B, C, E] [P1, P2, P3, P4]
thread_P P1 = [(B,P2),(D,P0)]
thread_P P2 = [(C,P3),(D,P0)]
thread_P P3 = [(E,P4),(D,P0)]
thread_P P4 = [(D,P0),(D,P0)]
thread_P _  = undefined

thread_Q :: State -> [(Event, State)]
thread_Q Q0 = zipWith (,) [A, C, D] [Q1, Q2, Q3]
thread_Q q
    | q `elem` [Q1, Q2, Q3] = []
    | otherwise = undefined

thread_Comp :: (State -> [(Event, State)]) -> (State -> [(Event, State)]) -> State -> [(Event, State)]
thread_Comp p q s =
    map f $
        filter (\((x,_),(y,_)) -> x == y) candidates
    where
        Comp (state_p, state_q) = s
        trans_p = p state_p
        trans_q = q state_q
        candidates = [(x,y) | x <- trans_p, y <- trans_q]
        f ((e1,s1),(_,s2)) = (e1, Comp(s1,s2))