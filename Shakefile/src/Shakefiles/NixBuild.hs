module Shakefiles.NixBuild where

import Development.Shake
import Development.Shake.FilePath

rules :: Rules ()
rules = do
    phony "nix:all" $ need
        [ "_build/nix-build/out/default/dist.linux-x86_64/bin/elm-format"
        , "_build/nix-build/out/default/dist.linux-aarch64/bin/elm-format"
        ]

    "_build/files_list/git/ls-files.txt" %> \out -> do
        alwaysRerun
        Stdout stdout <- cmd "git" "ls-files"
        writeFileChanged out stdout

    "_build/nix-build/out/default/*/bin/elm-format" %> \out -> do
        -- TODO: this is not quite correct, as untracked files that are not gitignored can also affect the nix-build output
        -- TODO: this also is overly broad and includes many files that aren't needed by the nix-build
        allProjectFiles <- readFileLines "_build/files_list/git/ls-files.txt"
        need allProjectFiles

        let (_:_:_:_:attr:rest) = splitDirectories out
        let gcroot = "_build/nix-build/gcroots/default" </> attr

        cmd_ "nix-build" "-A" attr "-o" gcroot
        copyFileChanged (gcroot </> foldl (</>) "" rest) out
