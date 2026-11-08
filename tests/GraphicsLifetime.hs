{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Exception (finally, throwIO, try)
import Control.Monad (unless)
import Data.IORef
import GameIO.Graphics.Vulkan.Lifetime
import GameIO.Graphics.Vulkan.Raw (VulkanError(..), pattern VK_ERROR_DEVICE_LOST)

main :: IO ()
main = do
  check "direct proof" (pure ()) (fail "fallback must not run") True Nothing
  check "separate drain succeeds" incomplete (pure ()) True Nothing
  check "unknown completion retains owner" incomplete incomplete False (Just uncertain)
  check "lost device permits release" (throwIO lost) (fail "fallback must not run") True (Just lost)
  checkNoRetryAfterUnconfirmedUpload incomplete
  putStrLn "graphics lifetime: direct proof, fallback, retention, device loss and retry rejection passed"
  where
    uncertain = VulkanCallFailed "injected wait" (-1)
    incomplete = throwIO uncertain
    lost = VulkanCallFailed "injected wait" VK_ERROR_DEVICE_LOST

check :: String -> IO () -> IO () -> Bool -> Maybe VulkanError -> IO ()
check label wait fallback expectedRelease expectedError = do
  retained <- newIORef False
  destroyed <- newIORef False
  gate <- newCleanupGate (writeIORef retained True)
  outcome <- try $ requireCompletion gate wait fallback
    `finally` releaseWhenAllowed gate (writeIORef destroyed True)
  didDestroy <- readIORef destroyed
  didRetain <- readIORef retained
  let actualError = either Just (const Nothing) outcome
  unless (didDestroy == expectedRelease && didRetain /= expectedRelease && actualError == expectedError) $
    fail ("lifetime test failed: " ++ label)

checkNoRetryAfterUnconfirmedUpload :: IO () -> IO ()
checkNoRetryAfterUnconfirmedUpload incomplete = do
  gate <- newCleanupGate (pure ())
  started <- newIORef False
  _ <- try (requireCompletion gate incomplete incomplete) :: IO (Either VulkanError ())
  retried <- try (requireCleanupKnown gate >> writeIORef started True)
  reused <- readIORef started
  unless (retried == Left VulkanCompletionUnconfirmed && not reused) $
    fail "operation admission allowed a retry after an unconfirmed upload"
