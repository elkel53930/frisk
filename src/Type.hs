{-# LANGUAGE QuasiQuotes #-}
module Type where

import Data.String.Interpolate
import qualified Data.Set as Set
import Data.Semigroup

data Mutex = Locked | Unlocked deriving (Show,Eq,Ord)
data SharedVars = SharedVars 
    {
        mtx0 :: Mutex,
        mtx1 :: Mutex
    } deriving (Show,Eq,Ord)

type Label = String
type Guard = SharedVars -> Bool
type Action = SharedVars -> SharedVars
data Trans = Trans {label :: Label, location :: Location, guard :: Guard, action :: Action} 
type Process = [(Location, [Trans])]
data State = State {locations :: [Location], step :: Int, sharedVars :: SharedVars}
type Queue = [State]
type Hash = Set.Set State
type Logs = [(Label,State,State)]
data Location = P0 | P1 | P2 | P3 | P4 | P5 | Q0 | Q1 | Q2 | Q3 | Q4 | Q5 deriving (Show,Eq,Ord)

instance Show Trans where
    show t = [i|Transition #{label t} : to #{location t})|]
instance Show State where
    show s = [i| #{locations s}\n#{dropWhile (/='{') . show $ sharedVars s}\nstep:#{step s}|]
instance Eq State where
    (State l1 _ s1) == (State l2 _ s2) = l1 == l2 && s1 == s2
instance Ord State where
    compare (State l1 _ s1) (State l2 _ s2) = compare l1 l2 <> compare s1 s2

proc :: Process
proc = 
    [ ( P0, [Trans "P locks 0"   P1 (\s -> mtx0 s == Unlocked) (\s -> s {mtx0 = Locked})])
    , ( P1, [Trans "P locks 1"   P2 (\s -> mtx1 s == Unlocked) (\s -> s {mtx1 = Locked})])
    , ( P2, [Trans "P unlocks 1" P3 (\_->True)                 (\s -> s {mtx1 = Unlocked})])
    , ( P3, [Trans "P unlocks 0" P0 (\_->True)                 (\s -> s {mtx0 = Unlocked})])
    , ( Q0, [Trans "Q locks 1"   Q1 (\s -> mtx1 s == Unlocked) (\s -> s {mtx1 = Locked})])
    , ( Q1, [Trans "Q locks 0"   Q2 (\s -> mtx0 s == Unlocked) (\s -> s {mtx0 = Locked})])
    , ( Q2, [Trans "Q unlocks 0" Q3 (\_->True)                 (\s -> s {mtx1 = Unlocked})])
    , ( Q3, [Trans "Q unlocks 1" Q0 (\_->True)                 (\s -> s {mtx0 = Unlocked})])
    ]

initState = State [P0,Q0] 0 $ SharedVars Unlocked Unlocked