import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (forM_)
import qualified System.Directory
import qualified System.Info

import qualified Shakefiles.ElmFormat.IntegrationTests
import qualified Shakefiles.Haskell
import qualified Shakefiles.Shellcheck
import qualified Shakefiles.Dependencies
import qualified Shakefiles.Signature
import Shakefiles.Extra


main :: IO ()
main = do
    shakefiles <- getDirectoryFilesIO "" [ "Shakefile/src//*.hs" ]
    shakefilesHash <- getHashedShakeVersion shakefiles
    shakeArgs shakeOptions{
      shakeChange = ChangeModtimeAndDigest,
      shakeColor = True,
      shakeVersion = shakefilesHash
    } rules


rules :: Rules ()
rules = do

    StdoutTrim gitDescribe <- liftIO $ cmd "git" [ "describe", "--abbrev=8", "--match", "[0-9]*", "--always" ]
    StdoutTrim gitSha <- liftIO $ cmd "git" [ "describe", "--always", "--match", "NOT A TAG", "--dirty" ]

    let elmFormat = "_build" </> "elm-format" <.> exe

    shellcheck <- Shakefiles.Dependencies.rules

    want [ "test" ]

    phony "test" $ need
        [ "unit-tests"
        , "integration-tests"
        , "shellcheck"
        ]

    phony "build" $ need [ "elm-format" ]
    phony "elm-format" $ need [ elmFormat ]
    phony "unit-tests" $ need
        [ "_build/cabal/elm-format-lib/test.ok"
        , "_build/cabal/elm-format-test-lib/test.ok"
        , "_build/cabal/elm-format/test.ok"
        ]
    phony "profile" $ need [ "_build/tests/test-files/prof.ok" ]
    phony "dist" $ need [ "dist-elm-format" ]
    phonyPrefix "publish-" $ \version ->
        need [ "elm-format-publish-" ++ version ]

    phony "clean" $ do
        removeFilesAfter "dist-newstyle" [ "//*" ]
        removeFilesAfter "_build" [ "//*" ]
        removeFilesAfter ".shake" [ "//*" ]
        removeFilesAfter ".shake.cache" [ "//*" ]
        removeFilesAfter "."
            [ "_input.elm"
            , "_input2.elm"
            , "formatted.elm"
            , "formatted.json"
            , "_stdout.txt"
            ]

    Shakefiles.Signature.rules


    --
    -- build
    --

    "generated/Build_elm_format.hs" %> \out -> do
        alwaysRerun
        writeFileChanged out $ unlines
            [ "module Build_elm_format where"
            , ""
            , "gitDescribe :: String"
            , "gitDescribe = " ++ show (gitDescribe :: String)
            ]

    Shakefiles.Haskell.cabalProject "avh4-lib"
        [ "avh4-lib/avh4-lib.cabal" ]
        [ "avh4-lib/src//*.hs" ]
        []
        [ "avh4-lib/test//*.hs" ]
        []

    Shakefiles.Haskell.cabalProject "elm-format-markdown"
        [ "elm-format-markdown/elm-format-markdown.cabal" ]
        [ "elm-format-markdown/src//*.hs"
        , "elm-format-markdown//*.hs"
        ]
        [] [] []

    Shakefiles.Haskell.cabalProject "elm-format-lib"
        [ "elm-format-lib/elm-format-lib.cabal" ]
        [ "elm-format-lib/src//*.hs" ]
        [ "avh4-lib"
        , "elm-format-markdown"
        ]
        [ "elm-format-lib/test//*.hs" ]
        [ "elm-format-test-lib" ]

    Shakefiles.Haskell.cabalProject "elm-format-test-lib"
        [ "elm-format-test-lib/elm-format-test-lib.cabal" ]
        [ "elm-format-test-lib/src//*.hs" ]
        [ "avh4-lib" ]
        [ "elm-format-test-lib/test//*.hs" ]
        []

    Shakefiles.Haskell.cabalProject "elm-format"
        [ "elm-format.cabal"
        , "generated/Build_elm_format.hs"
        ]
        [ "src//*.hs" ]
        [ "avh4-lib"
        , "elm-format-lib"
        ]
        [ "tests//*.hs"
        , "tests//*.stdout"
        , "tests//*.stderr"
        ]
        [ "elm-format-test-lib" ]

    Shakefiles.Haskell.executable elmFormat "elm-format" gitDescribe

    Shakefiles.ElmFormat.IntegrationTests.rules gitSha elmFormat

    Shakefiles.Shellcheck.rules shellcheck
