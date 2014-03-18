{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control (
    CommandLineControl,
    runCommandLineControl,

    RandomControl,
    runRandomControl,

    Arg,
    Program(..),
    ProgramControl,
    runProgramControl,
) where


import Control.Monad (when)
import Control.Monad.Random (MonadRandom, getRandomR)
import Control.Monad.State (MonadState, StateT, evalStateT, gets)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (isSpace)
import Data.Maybe (maybeToList, listToMaybe)
import Data.Proxy (Proxy(..))
import Game
import GameData
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
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


data EmptyCell = EmptyCell


transformCell
    :: (EmptyCell -> a)
    -> (Bot -> a)
    -> (Bullet -> a)
    -> (Missile -> a)
    -> (LandMine -> a)
    -> ([a] -> a)
    -> Cell
    -> a
transformCell fe fbot fb fm fl fxs cell = let
    bot = fmap fbot $ _bot cell
    bullet = fmap fb $ listToMaybe $ _bullets cell
    missile = fmap fm $ listToMaybe $ _missiles cell
    landMine = fmap fl $ listToMaybe $ _landMines cell
    g = maybeToList
    in case g bot ++ g bullet ++ g missile ++ g landMine of
        [] -> fe EmptyCell
        [x] -> x
        xs -> fxs xs


gridify :: Arena -> [[Cell]]
gridify arena = let
    rows = [0 .. 9]
    cols = [0 .. 9]
    showRow row = let
        f col = let
            coords = Coords col row
            in getCell coords arena
        in map f cols
    in map showRow rows


showArena :: Maybe Char -> Maybe Player -> Arena -> String
showArena mDenseChar mPlayer arena = trim $ gridChars ++ fullInfo
    where
        grid = gridify arena
        gridChars = unlines $ map (map cellToChar) grid
        playerToChar p = case mPlayer of
            Nothing -> case p of
                P1 -> '1'
                P2 -> '2'
            Just p' -> case p == p' of
                True -> 'Y'
                False -> 'X'
        cellToChar = let
            fe _ = '.'
            fbot (Bot p _) = playerToChar p
            fb _ = 'B'
            fm _ = 'M'
            fl _ = 'L'
            fxs xs = case mDenseChar of
                Nothing -> head xs
                Just c -> c
            in transformCell fe fbot fb fm fl fxs
        showCoords (Coords x y) = show x ++ " " ++ show y
        fullInfo = let
            bots = putPlayer'sBotFirst mPlayer $ gatherAll (Proxy :: Proxy Bot) arena
            bullets = gatherAll (Proxy :: Proxy Bullet) arena
            missiles = gatherAll (Proxy :: Proxy Missile) arena
            landMines = gatherAll (Proxy :: Proxy LandMine) arena
            fbot _ (Bot p e) = playerToChar p : " " ++ show (fromEnum e)
            fb coords (Bullet dir) = "B " ++ showCoords coords ++ " " ++ show dir
            fm coords (Missile dir) = "M " ++ showCoords coords ++ " " ++ show dir
            fl coords LandMine = "L " ++ showCoords coords
            g = map . uncurry . flip
            in unlines $ g fbot bots ++ g fb bullets ++ g fm missiles ++ g fl landMines


putPlayer'sBotFirst :: Maybe Player -> [(Bot, Coords)] -> [(Bot, Coords)]
putPlayer'sBotFirst mPlayer bots = case mPlayer of
    Nothing -> bots
    Just p -> let
        owned (Bot p' _, _) = p == p'
        bs = filter owned bots
        bs' = filter (not . owned) bots
        in bs ++ bs'


--------------------------------------------------------------------------------


newtype CommandLineControl a = CommandLineControl { runCommandLineControl :: IO a }
    deriving (Monad, MonadRandom, MonadIO)


instance MonadBattleBots CommandLineControl where
    tellBout boutSt = let
        arena = _arena boutSt
        in liftIO $ do
            putStrLn $ showArena (Just '?') Nothing arena
    getCommand p _ = liftIO $ do
        putStr $ show p ++ "> "
        fmap parseCommand getLine


--------------------------------------------------------------------------------


newtype RandomControl a = RandomControl { runRandomControl :: IO a }
    deriving (Monad, MonadRandom, MonadIO)


instance MonadBattleBots RandomControl where
    tellBout boutSt = let
        arena = _arena boutSt
        in liftIO $ do
            putStrLn $ showArena (Just '?') Nothing arena
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


--------------------------------------------------------------------------------


type Arg = String
data Program = Program FilePath [Arg]
type Programs = (Program, Program)


newtype ProgramControl a = ExecutableIO { unProgramControl :: StateT Programs IO a }
    deriving (Monad, MonadRandom, MonadIO, MonadState Programs)


runProgramControl :: Program -> Program -> ProgramControl a -> IO a
runProgramControl prog1 prog2 = let
    st = (prog1, prog2)
    in flip evalStateT st . unProgramControl


instance MonadBattleBots ProgramControl where
    tellBout boutSt = let
        arena = _arena boutSt
        time = _time boutSt
        emp = _emp boutSt
        in liftIO $ do
            putStrLn $ "Time: " ++ show time
            putStrLn $ "Emp: " ++ show emp
            putStrLn $ showArena (Just '?') Nothing arena
    getCommand p arena = do
        prog <- case p of
            P1 -> gets fst
            P2 -> gets snd
        let msg = showArena Nothing (Just p) arena ++ "\n"
            Program exe args = prog
            runProg = liftIO $ readProcessWithExitCode exe (args ++ [msg]) ""
        (exitCode, outStr, _) <- runProg
        let trimmedOutStr = trim outStr
            command = case exitCode of
                ExitSuccess -> parseCommand trimmedOutStr
                ExitFailure _ -> UnknownCommand
        liftIO $ do
            putStrLn ""
            putStrLn $ show p ++ ": " ++ trimmedOutStr
            putStrLn $ show p ++ ": " ++ show command
            when (p == P2) $ do
                putStr "\nPRESS ENTER"
                _ <- getLine
                return ()
        return command


trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


















