module Main (
    main
) where


import Control (Executable, runExecutableIO)
import Game (runMatch)
import System.Environment (getArgs)


--------------------------------------------------------------------------------


main :: IO ()
main = do
    [exe1, exe2] <- getArgs
    mWinner <- runExecutableIO exe1 exe2 $ runMatch 5
    print mWinner



