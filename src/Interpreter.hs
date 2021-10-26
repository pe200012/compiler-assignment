{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter
    ( Machine(..)
    , ProgramT(..)
    , runProgramIO
    , cleanRun
    , effectful
    ) where

import           Control.Lens                   ( _1
                                                , _2
                                                , over
                                                , to
                                                , use
                                                , view
                                                )
import           Control.Lens.TH                ( makeLenses )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.State.Lazy       ( StateT
                                                , execStateT
                                                , gets
                                                , modify
                                                )
import           Control.Monad.Writer.Lazy      ( Writer
                                                , tell
                                                )
import           Stream                         ( Stream
                                                , createStream
                                                , current
                                                , fromList
                                                , getLeft
                                                , getRight
                                                , setCurrent
                                                )
import           Token                          ( Token(..) )

newtype Machine = Machine { unMachine :: Stream Int }

data Running = Running
    { _statements :: Stream (Maybe Token)
    , _cells      :: Stream Int
    }

makeLenses ''Running

newtype ProgramT m = ProgramT { runProgramT :: [Token] -> m Machine }

runProgramIO :: MonadIO m => ProgramT m -> [Token] -> m Machine
runProgramIO = runProgramT

cleanRun :: Stream Char -> ProgramT (Writer [Char])
cleanRun input = ProgramT
    $ \ts -> Machine . view (_2 . cells) <$> execStateT loop (fromEnum <$> input, Running (fromList ts) (createStream 0 (const (const 0))))
  where
    loop :: StateT (Stream Int, Running) (Writer [Char]) ()
    loop = use (_2 . statements . to current) >>= \case
        Nothing -> pure ()
        Just t  -> do
            case t of
                Incr      -> modify (over (_2 . cells) (setCurrent . succ =<< current))
                Decr      -> modify (over (_2 . cells) (setCurrent . pred =<< current))
                MoveLeft  -> modify (over (_2 . cells) getLeft)
                MoveRight -> modify (over (_2 . cells) getRight)
                Print     -> tell . pure . toEnum =<< use (_2 . cells . to current)
                Read      -> do
                    modify . over (_2 . cells) . setCurrent =<< gets (view (_1 . to current))
                    modify (over _1 getRight)
                JumpForward -> use (_2 . cells . to current) >>= \case
                    0 -> forward 0
                    _ -> pure ()
                JumpBackward -> use (_2 . cells . to current) >>= \n -> if n /= 0 then backward 0 else pure ()
            moveRight
            loop
    moveRight = modify (over (_2 . statements) getRight)
    moveLeft  = modify (over (_2 . statements) getLeft)
    forward :: Int -> StateT (Stream Int, Running) (Writer [Char]) ()
    forward n = use (_2 . statements . to current) >>= \case
        Nothing          -> pure ()
        Just JumpForward -> moveRight >> forward (n + 1)
        Just JumpBackward | n == 1    -> pure ()
                          | otherwise -> moveRight >> forward (n - 1)
        _ -> moveRight >> forward n
    backward :: Int -> StateT (Stream Int, Running) (Writer [Char]) ()
    backward n = use (_2 . statements . to current) >>= \case
        Nothing           -> pure ()
        Just JumpBackward -> moveLeft >> backward (n + 1)
        Just JumpForward | n == 1    -> pure ()
                         | otherwise -> moveLeft >> backward (n - 1)
        _ -> moveLeft >> backward n


effectful :: ProgramT IO
effectful = ProgramT $ \ts -> Machine . view cells <$> execStateT loop (Running (fromList ts) (createStream 0 (const (const 0))))
  where
    loop :: StateT Running IO ()
    loop = use (statements . to current) >>= \case
        Nothing -> pure ()
        Just t  -> do
            case t of
                Incr        -> modify (over cells (setCurrent . succ =<< current))
                Decr        -> modify (over cells (setCurrent . pred =<< current))
                MoveLeft    -> modify (over cells getLeft)
                MoveRight   -> modify (over cells getRight)
                Print       -> liftIO . putChar . toEnum =<< use (cells . to current)
                Read        -> modify . over cells . setCurrent . fromEnum =<< liftIO getChar
                JumpForward -> use (cells . to current) >>= \case
                    0 -> forward 0
                    _ -> pure ()
                JumpBackward -> use (cells . to current) >>= \n -> if n /= 0 then backward 0 else pure ()
            moveRight
            loop
    moveRight = modify (over statements getRight)
    moveLeft  = modify (over statements getLeft)
    forward :: Int -> StateT Running IO ()
    forward n = use (statements . to current) >>= \case
        Nothing          -> pure ()
        Just JumpForward -> moveRight >> forward (n + 1)
        Just JumpBackward | n == 1    -> pure ()
                          | otherwise -> moveRight >> forward (n - 1)
        _ -> moveRight >> forward n
    backward :: Int -> StateT Running IO ()
    backward n = use (statements . to current) >>= \case
        Nothing           -> pure ()
        Just JumpBackward -> moveLeft >> backward (n + 1)
        Just JumpForward | n == 1    -> pure ()
                         | otherwise -> moveLeft >> backward (n - 1)
        _ -> moveLeft >> backward n

