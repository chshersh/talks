{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Free2 where

import Data.Text (Text)
import System.Directory (getCurrentDirectory, getDirectoryContents)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

data Cmd a
    = Echo [Text] (Cmd a)
    | Ls ([Text] -> Cmd a)
    | Finish a

runCommand :: Cmd a -> IO a
runCommand cmd = case cmd of
    Echo args next -> do
        TextIO.putStrLn $ Text.unwords args
        runCommand next
    Ls getNext -> do
        curDir <- getCurrentDirectory
        dirs <- getDirectoryContents curDir
        let dirNames = map Text.pack dirs
        runCommand $ getNext dirNames
    Finish a -> pure a

dryRunCommand :: Show a => Cmd a -> [Text]
dryRunCommand cmd = case cmd of
    Echo args next -> Text.unwords ("echo" : args) : dryRunCommand next
    Ls next        -> "ls -1a" : dryRunCommand (next ["dry-run-test-dir"])
    Finish a       -> ["Returning: " <> Text.pack (show a)]

dryRun :: Script -> IO ()
dryRun = TextIO.putStr . Text.unlines . runScriptDryRun

exampleScript :: Cmd Int
exampleScript
    = Echo ["pancake", "honey"]
    $ Ls
    $ \dirs -> Echo dirs
    $ Echo ["Is", "this", "free", "monad?"]
    $ Finish (length dirs)
