{-# LANGUAGE QuasiQuotes #-}
module Type where

import Data.String.Interpolate
import qualified Data.Set as Set
import Data.Semigroup

data Event = A | B | Tau
           | Hidden Event deriving (Show, Eq, Ord)
data State = P0 | P1 | P2 | P3 | P4
           | Q0 | Q1 | Q2
           | Scenario [Event]
           | Comp (State, State) deriving (Show, Eq, Ord)
type Queue a = [a]
type Hash a = Set.Set a
type Logs a b = [(b,a,a)] 
type Process = State -> [(Event, State)]
type Error = String

range :: [Int]
range = [0..2]

isHidden :: Event -> Bool
isHidden (Hidden _) = True
isHidden _ = False

hide :: (Event -> Bool) -> [Event] -> [Event]
hide pred es = map (\e -> if pred e then e else Hidden e) es

process_P :: Process
process_P P0 = [(Hidden Tau, P1), (Hidden Tau, P2)]
process_P P1 = [(A, P3)]
process_P P2 = [(B, P4)]
process_P P3 = []
process_P P4 = []
process_P _  = undefined

process_Q :: Process
process_Q Q0 = [(A,Q1),(B,Q2)]
process_Q Q1 = []
process_Q Q2 = []
process_Q _  = undefined

scenario :: Process
scenario (Scenario (e:es)) = [(e, Scenario es)]
scenario (Scenario []) = []

{-
    parameters
     thread1
     thread2
     interface (set of synchronous events)
-}
process_Comp :: Process -> Process -> Set.Set Event -> State -> [(Event, State)]
process_Comp p q es s =
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
