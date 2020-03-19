{-# LANGUAGE QuasiQuotes #-}
module Type where

import Data.String.Interpolate
import qualified Data.Set as Set
import Data.Semigroup

data Event = IN Int | MID Int | OUT Int deriving (Show, Eq, Ord)
data State = P0 | P1 Int
           | Q0 | Q1 Int
           | Comp (State, State) deriving (Show, Eq, Ord)
type Queue a = [a]
type Hash a = Set.Set a
type Logs a b = [(b,a,a)] 

range :: [Int]
range = [0..2]

thread_P :: State -> [(Event, State)]
thread_P P0 = map (\x -> (IN x,P1 x)) range
thread_P (P1 x) = [(MID x,P0)]
thread_P _  = undefined

thread_Q :: State -> [(Event, State)]
thread_Q Q0 = map (\x -> (MID x,Q1 x)) range
thread_Q (Q1 x) = [(OUT x,Q0)]
thread_Q _  = undefined

thread_Comp :: (State -> [(Event, State)]) -> (State -> [(Event, State)]) -> Set.Set Event -> State -> [(Event, State)]
thread_Comp p q es s =
    concat [ getSyncEvents trans_p trans_q es
           , getImdependentEvents trans_p trans_q s es
           ]
    where
        Comp (state_p, state_q) = s
        trans_p = p state_p
        trans_q = q state_q

getSyncEvents :: [(Event, State)] -> [(Event, State)] -> Set.Set Event -> [(Event, State)]
getSyncEvents ep eq es =
    map mk $ filter (\((x,_),(y,_)) -> x == y && Set.member x es ) candidates
    where
        candidates = [(x,y) | x <- ep, y <- eq]
        mk ((e1,s1),(_,s2)) = (e1, Comp(s1,s2))

getImdependentEvents :: [(Event, State)] -> [(Event, State)] -> State -> Set.Set Event -> [(Event, State)]
getImdependentEvents ep eq s es = (map mkp $ flt ep) ++ (map mkq $ flt eq)
    where
        Comp (state_p, state_q) = s
        mkp (e,s) = (e, Comp (s,state_q))
        mkq (e,s) = (e, Comp (state_p,s))
        flt = filter (\(e,_) -> Set.notMember e es)
