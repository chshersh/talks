{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Free2 where

import Data.Foldable (traverse_)
import Data.Text (Text)
import System.Directory (getCurrentDirectory, getDirectoryContents)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

data Cmd
    = Echo [Text]
    | Ls

runCommand :: Cmd -> IO ()
runCommand cmd = case cmd of
    Echo args -> TextIO.putStrLn $ Text.unwords args
    Ls -> do
        curDir <- getCurrentDirectory
        dirs <- getDirectoryContents curDir
        TextIO.putStrLn $ Text.unlines $ map Text.pack dirs

newtype Script = Script [Cmd]

runScript :: Script -> IO ()
runScript (Script commands) = traverse_ runCommand commands

runScriptDryRun :: Script -> [Text]
runScriptDryRun (Script commands) = map fmtCommand commands

dryRun :: Script -> IO ()
dryRun = TextIO.putStr . Text.unlines . runScriptDryRun

fmtCommand :: Cmd -> Text
fmtCommand cmd = case cmd of
    Echo args -> Text.unwords $ "echo" : args
    Ls        -> "ls -1a"

exampleScript :: Script
exampleScript = Script
    [ Echo ["pancake", "honey"]
    , Ls
    , Echo ["Is", "this", "free", "monad?"]
    ]

optimiseEchoes :: Script -> Script
optimiseEchoes (Script commands) = Script (optimise commands)
  where
    optimise :: [Cmd] -> [Cmd]
    optimise [] = []
    optimise (Echo xs : Echo ys : cmds) =
        optimise (Echo (xs ++ ys) : cmds)
    optimise (cmd : cmds) = cmd : optimise cmds

{- Run:

λ: dryRun echoChamber
echo You're the best!
echo Amazing!

λ: dryRun $ optimiseEchoes echoChamber
echo You're the best! Amazing!

-}
echoChamber :: Script
echoChamber = Script
    [ Echo ["You're", "the", "best!"]
    , Echo ["Amazing!"]
    ]
