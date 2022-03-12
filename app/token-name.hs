module Main
    ( main
    ) where

import Data.String        (IsString (..))
import System.Environment (getArgs)
import Utils       (unsafeTokenNameToHex)

main :: IO ()
main = do
    [tn'] <- getArgs
    let tn = fromString tn'
    putStrLn $ unsafeTokenNameToHex tn
