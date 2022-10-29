{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Free2 where

import Control.Monad.Writer (MonadWriter (..), Writer, execWriter)
import Data.Kind (Type)
import Data.Text (Text)
import System.Directory (getCurrentDirectory, getDirectoryContents)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

type Free :: (Type -> Type) -> Type -> Type
data Free f a
    = Pure a
    | Roll (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap f (Pure a)     = Pure (f a)
    fmap f (Roll ffree) = Roll (fmap (fmap f) ffree)

instance Functor f => Applicative (Free f) where
    pure :: a -> Free f a
    pure = Pure

    (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    Pure f <*> free = f <$> free
    Roll ff <*> fa  = Roll (fmap (<*> fa) ff)

instance Functor f => Monad (Free f) where
    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    Pure x >>= f     = f x
    Roll ffree >>= f = Roll (fmap (>>= f) ffree)

liftF :: (Functor f) => f a -> Free f a
liftF fa = Roll (fmap Pure fa)

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree run free = case free of
    Pure a  -> pure a
    Roll fa -> do
        freeA <- run fa
        foldFree run freeA

data Cmd next
    = Echo [Text] next
    | Ls ([Text] -> next)
    deriving stock (Functor)

type Script a = Free Cmd a

echo :: [Text] -> Script ()
echo args = liftF (Echo args ())

ls :: Script [Text]
ls = liftF (Ls id)

------------------------------------------------------------------
-- Example script
------------------------------------------------------------------

exampleScript :: Script Int
exampleScript = do
    echo ["pancake", "honey"]
    dirs <- ls
    echo dirs
    echo ["Is", "this", "free", "monad?"]
    pure $ length dirs

------------------------------------------------------------------
-- IO Interpreter
------------------------------------------------------------------

runCommand :: Cmd next -> IO next
runCommand cmd = case cmd of
    Echo args next -> do
        TextIO.putStrLn $ Text.unwords args
        pure next
    Ls getNext -> do
        curDir <- getCurrentDirectory
        dirs <- getDirectoryContents curDir
        let dirNames = map Text.pack dirs
        pure $ getNext dirNames

runScript :: Script a -> IO a
runScript = foldFree runCommand

------------------------------------------------------------------
-- Pure Interpreter
------------------------------------------------------------------

--- !!! It's better to use State + DList for efficiency
fmtCommand :: Cmd a -> Writer [Text] a
fmtCommand cmd = case cmd of
    Echo args next -> do
        tell [Text.unwords ("echo" : args)]
        pure next
    Ls next -> do
        tell ["ls -1a"]
        pure $ next ["dry-run-test-dir"]

dryRunCommand :: Script a -> [Text]
dryRunCommand = execWriter . foldFree fmtCommand

dryRun :: Script a -> IO ()
dryRun = TextIO.putStr . Text.unlines . dryRunCommand

------------------------------------------------------------------
-- Alternative implementation of dryRun using pure Free Monad fold
------------------------------------------------------------------

freeFoldr
    :: forall f acc cur a
    .  (forall next . f next -> (cur, next))
    -- ^ Extract next value from the @f@ type and transform the existing
    -> (cur -> acc -> acc)
    -- ^ Fold current value with accumulator
    -> (a -> acc)
    -- ^ Map the final 'Pure' to the accumulator
    -> Free f a
    -> acc
freeFoldr split step fromPure = go
  where
    go :: Free f a -> acc
    go free = case free of
        Pure a     -> fromPure a
        Roll ffree ->
            let (cur, next) = split ffree
            in step cur (go next)

dryRunCommand2 :: Script a -> [Text]
dryRunCommand2 = freeFoldr split (:) (const [])
  where
    split :: Cmd a -> (Text, a)
    split cmd = case cmd of
        Echo args next -> (Text.unwords ("echo" : args), next)
        Ls next        -> ("ls -1a", next ["dry-run-test-dir"])

dryRun2 :: Script a -> IO ()
dryRun2 = TextIO.putStr . Text.unlines . dryRunCommand2

