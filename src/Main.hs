module Main (
    main
) where


import Control (Program(..), runProgramControl)
import Game (runMatch)
import System.Environment (getArgs)


--------------------------------------------------------------------------------


main :: IO ()
main = do
    args <- getArgs
    let mProgs = parsePrograms args
    case mProgs of
        Nothing -> do
            putStrLn "Usage: runmatch --prog1 prog1 [prog1-args] --prog2 [prog2-args]"
        Just (prog1, prog2) -> do
            mWinner <- runProgramControl prog1 prog2 $ runMatch 5
            print mWinner


parsePrograms :: [String] -> Maybe (Program, Program)
parsePrograms args = case args of
    [] -> Nothing
    "--prog1" : rest -> case parseProgram1 rest of
        Nothing -> Nothing
        Just (prog1, rest') -> case parseProgram2 rest' of
            Nothing -> Nothing
            Just prog2 -> Just (prog1, prog2)


parseProgram1 :: [String] -> Maybe (Program, [String])
parseProgram1 args = case args of
    [] -> Nothing
    "--prog2" : _ -> Nothing
    exe : rest -> let
        (progArgs, rest') = span (/= "--prog2") rest
        prog = Program exe progArgs
        in Just (prog, rest')


parseProgram2 :: [String] -> Maybe Program
parseProgram2 args = case args of
    [] -> Nothing
    exe : rest -> let
        progArgs = rest
        prog = Program exe progArgs
        in Just prog





