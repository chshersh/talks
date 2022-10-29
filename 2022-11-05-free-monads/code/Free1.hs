{-# LANGUAGE OverloadedStrings #-}

module Free1 where

import Data.Foldable (traverse_)
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

data Cmd
    = Echo [Text]

runCommand :: Cmd -> IO ()
runCommand cmd = case cmd of
    Echo args -> TextIO.putStrLn $ Text.unwords args

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

exampleScript :: Script
exampleScript = Script
    [ Echo ["pancake", "honey"]
    , Echo ["Is", "this", "free", "monad?"]
    ]
