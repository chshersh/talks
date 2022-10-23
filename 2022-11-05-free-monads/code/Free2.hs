{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Free2 where

import System.Directory (getDirectoryContents, getCurrentDirectory)
import Data.Text (Text)
import Data.Foldable (traverse_)

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

fmtCommand :: Cmd -> Text
fmtCommand cmd = case cmd of
    Echo args -> Text.unwords $ "echo" : args
    Ls -> "ls -1a"

exampleScript :: Script
exampleScript = Script
    [ Echo ["pancake", "honey"]
    , Ls
    , Echo ["Is", "this", "free", "monad?"]
    ]
