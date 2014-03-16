{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameData where


import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Values


--------------------------------------------------------------------------------


mkReadsPrec :: String -> a -> (Int -> ReadS a)
mkReadsPrec s x = \_ str -> case stripPrefix s str of
    Nothing -> []
    Just str' -> [(x, str')]


--------------------------------------------------------------------------------


data Energy = E0 | E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8 | E9 | E10
    deriving (Show, Read, Eq, Ord, Enum)


instance Values Energy where
    allValues = [toEnum 0 ..]


--------------------------------------------------------------------------------


data Player = P1 | P2
    deriving (Show, Read, Eq, Ord, Enum)


instance Values Player where
    allValues = [toEnum 0 ..]


--------------------------------------------------------------------------------


data Bot = Bot Player Energy
    deriving (Show, Read, Eq, Ord)


instance Values Bot where
    allValues = do
        c <- allValues
        l <- allValues
        return $ Bot c l


--------------------------------------------------------------------------------


data LandMine = LandMine
    deriving (Show, Read, Eq, Ord, Enum)


instance Values LandMine where
    allValues = [toEnum 0 ..]


--------------------------------------------------------------------------------


data Dir = N | NE | E | SE | S | SW | W | NW
    deriving (Show, Read, Eq, Ord, Enum)


instance Values Dir where
    allValues = [toEnum 0 ..]


--------------------------------------------------------------------------------


newtype Bullet = Bullet Dir
    deriving (Show, Read, Eq, Ord, Enum)


instance Values Bullet where
    allValues = [toEnum 0 ..]


--------------------------------------------------------------------------------


newtype Missile = Missile Dir
    deriving (Show, Read, Eq, Ord, Enum)


instance Values Missile where
    allValues = [toEnum 0 ..]


--------------------------------------------------------------------------------


data Cell = Cell {
    _bot :: Maybe Bot,
    _landMines :: [LandMine],
    _bullets :: [Bullet],
    _missiles :: [Missile]
} deriving (Show, Read, Eq, Ord)


emptyCell :: Cell
emptyCell = Cell {
    _bot = Nothing,
    _landMines = [],
    _bullets = [],
    _missiles = [] }


--------------------------------------------------------------------------------


data EmpDuration = EmpTwoRounds | EmpOneRound
    deriving (Show, Read, Eq, Ord, Enum)


instance Values EmpDuration where
    allValues = [toEnum 0 ..]


--------------------------------------------------------------------------------


newtype Emp = Emp EmpDuration
    deriving (Show, Read, Eq, Ord, Enum)


instance Values Emp where
    allValues = [toEnum 0 ..]


--------------------------------------------------------------------------------


data Coords = Coords Int Int
    deriving (Show, Read, Eq, Ord)


--------------------------------------------------------------------------------


newtype Arena = Arena { unArena :: Map Coords Cell }
    deriving (Show, Read, Eq, Ord)


--------------------------------------------------------------------------------

























