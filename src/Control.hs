{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Controller (
    CommandLine,
    runCommandLine,
    RandomIO,
    runRandomIO,
) where


import Control.Monad.Random (MonadRandom, getRandomR)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy(..))
import Game
import GameData
import Text.Read (readMaybe)
import Values


--------------------------------------------------------------------------------


pick :: (MonadRandom m) => [a] -> m a
pick xs = case length xs of
    0 -> error "pick: empty list"
    n -> do
        idx <- getRandomR (0, n - 1)
        return $ xs !! idx


getPlayer'sBot :: Player -> Arena -> Bot
getPlayer'sBot p arena = let
    bots = map fst $ gatherAll (Proxy :: Proxy Bot) arena
    err = error "getPlayer'sBot: Could not find player's bot."
    in case bots of
        [b0@(Bot p0 _), b1@(Bot p1 _)] -> case p == p0 of
            True -> b0
            False -> case p == p1 of
                True -> b1
                False -> err
        _ -> err


--------------------------------------------------------------------------------


parseCommand :: String -> Command
parseCommand str = case str of
    "0" -> DoNothing
    "P" -> FireEmp
    'B' : ' ' : rest -> case readMaybe rest of
        Nothing -> DoNothing
        Just dir -> FireBullet dir
    'M' : ' ' : rest -> case readMaybe rest of
        Nothing -> DoNothing
        Just dir -> FireMissile dir
    'L' : ' ' : rest -> case readMaybe rest of
        Nothing -> DoNothing
        Just dir -> DropLandMine dir
    _ -> case readMaybe str of
        Nothing -> DoNothing
        Just dir -> Move dir


cellChar :: Cell -> Char
cellChar cell = let
    bot = case _bot cell of
        Nothing -> Nothing
        Just (Bot _ e) -> Just $ case e of
            E0 -> '0'
            E1 -> '1'
            E2 -> '2'
            E3 -> '3'
            E4 -> '4'
            E5 -> '5'
            E6 -> '6'
            E7 -> '7'
            E8 -> '8'
            E9 -> '9'
            E10 -> '+'
        --Just (Bot p _) -> Just $ case p of
            --P1 -> '1'
            --P2 -> '2'
    bullet = case _bullets cell of
        [] -> Nothing
        _ -> Just 'B'
    missile = case _missiles cell of
        [] -> Nothing
        _ -> Just 'M'
    landMine = case _landMines cell of
        [] -> Nothing
        _ -> Just 'L'
    f = maybeToList
    in case f bot ++ f bullet ++ f missile ++ f landMine of
        "" -> '.'
        [c] -> c
        _ -> '?'


showArena :: Arena -> String
showArena arena = let
    rows = [0 .. 9]
    cols = [0 .. 9]
    showRow row = let
        f col = let
            coords = Coords col row
            cell = getCell coords arena
            in cellChar cell
        in map f cols
    in unlines $ map showRow rows


--------------------------------------------------------------------------------


newtype CommandLine a = CommandLine { runCommandLine :: IO a }
    deriving (Monad, MonadRandom, MonadIO)


instance MonadBattleBots CommandLine where
    tellArena arena = liftIO $ do
        putStrLn $ showArena arena
    getCommand p _ = liftIO $ do
        putStr $ show p ++ "> "
        fmap parseCommand getLine


--------------------------------------------------------------------------------


newtype RandomIO a = RandomIO { runRandomIO :: IO a }
    deriving (Monad, MonadRandom, MonadIO)


instance MonadBattleBots RandomIO where
    tellArena arena = liftIO $ do
        putStrLn $ showArena arena
        putStrLn "PRESS ENTER"
        _ <- getLine
        return ()
    getCommand p arena = liftIO $ do
        dir <- pick allValues
        let bot = getPlayer'sBot p arena
            coords = getBotCoords bot arena
            coords' = moveDir coords dir
            cmds = [
                DoNothing,
                Move dir,
                FireBullet dir,
                FireMissile dir,
                DropLandMine dir,
                FireEmp ]
            cmds' = case coords' of
                Right _ -> cmds
                Left _ -> flip filter cmds $ flip notElem [
                    Move dir,
                    FireBullet dir,
                    DropLandMine dir ]
        cmd <- pick cmds'
        putStrLn $ show p ++ " - " ++ show cmd
        return cmd




























