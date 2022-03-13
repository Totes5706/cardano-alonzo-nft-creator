module Main
    ( main
    ) where

import Control.Exception    (throwIO)
import Data.String          (IsString (..))
import System.Environment   (getArgs)
import Token.OnChain (nftPolicy)
import Utils         (unsafeReadTxOutRef, writeMintingPolicy)

main :: IO ()
main = do
    [file, oref', tn'] <- getArgs
    let oref = unsafeReadTxOutRef oref'
        tn   = fromString tn'
        p    = nftPolicy oref tn
    e <- writeMintingPolicy file p
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> return ()
