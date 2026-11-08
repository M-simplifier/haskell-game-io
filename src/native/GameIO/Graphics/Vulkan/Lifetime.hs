{-# LANGUAGE PatternSynonyms #-}

module GameIO.Graphics.Vulkan.Lifetime
  ( CleanupGate
  , newCleanupGate
  , cleanupAllowed
  , requireCleanupKnown
  , releaseWhenAllowed
  , requireCompletion
  ) where

import Control.Exception (mask_, throwIO, try)
import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GameIO.Graphics.Vulkan.Raw

data CleanupGate = CleanupGate !(IORef Bool) !(IO ())

newCleanupGate :: IO () -> IO CleanupGate
newCleanupGate retainWindow = CleanupGate <$> newIORef True <*> pure retainWindow

cleanupAllowed :: CleanupGate -> IO Bool
cleanupAllowed (CleanupGate allowed _) = readIORef allowed

requireCleanupKnown :: CleanupGate -> IO ()
requireCleanupKnown gate = do
  allowed <- cleanupAllowed gate
  unless allowed (throwIO VulkanCompletionUnconfirmed)

releaseWhenAllowed :: CleanupGate -> IO () -> IO ()
releaseWhenAllowed gate release = mask_ $ do
  allowed <- cleanupAllowed gate
  when allowed release

-- The fallback must independently establish completion of the same work.
-- Queue idle can prove a queue submission, but cannot prove a WSI fence.
requireCompletion :: CleanupGate -> IO () -> IO () -> IO ()
requireCompletion (CleanupGate allowed retainWindow) wait fallback = mask_ $ do
  completed <- try wait
  case completed of
    Right () -> pure ()
    Left lost@(VulkanCallFailed _ VK_ERROR_DEVICE_LOST) -> throwIO lost
    Left _ -> do
      confirmed <- try fallback
      case confirmed of
        Right () -> pure ()
        Left lost@(VulkanCallFailed _ VK_ERROR_DEVICE_LOST) -> throwIO lost
        Left unconfirmed -> do
          writeIORef allowed False
          retainWindow
          throwIO (unconfirmed :: VulkanError)
