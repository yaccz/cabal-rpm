module Main where

import Test.Hspec
import Data.String.Utils
import Setup
import System.Environment
import System.Exit
import System.Process

main = do
    os <- getEnv "OSTYPE"
    runTests $ read os

runTests :: OS -> IO ()
runTests os = hspec $
    describe "Command detect-os" $
        it "detects os" $ do
            (ec, out, err) <- readProcessWithExitCode
                "cblrpm" ["detect-os"] ""
            let (e_ec, e_out, e_err) = expects os

            ec `shouldBe` e_ec
            out `shouldBe` e_out
            err `shouldBe` e_err

            where
                expects SUSE   = (ExitSuccess, show SUSE   ++ "\n", "")
                expects Fedora = (ExitSuccess, show Fedora ++ "\n", "")
                expects _      = (ExitFailure 1, "", detectOSFailureWarning)
