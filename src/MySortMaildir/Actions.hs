{-# OPTIONS -fno-warn-overlapping-patterns #-}
--------------------------------------------------------------------------------
-- | 
-- Module      : MySortMaildir.Actions
-- Note        : Contains the function to apply the actions defined in Common.hs
-- 
-- 
-- 
--------------------------------------------------------------------------------

module MySortMaildir.Actions
  ( applyAction
  ) where

import           System.Directory
import           System.FilePath
import           Control.Monad

import           MySortMaildir.Common

--------------------------------------------------------------------------------
-- Takes
--      a mail and
--      a action
-- and aplies the action to the mail
applyAction :: Mail -> Action -> IO ()
applyAction m (MoveTo p) = let
    sPath          = splitPath (file m)
    dSPath         = drop (length sPath - 2) sPath
    targetDir      = p </> head dSPath
    targetFile     = targetDir </> head (tail dSPath)
    mySafeCopy s d = do
      -- TODO: Improve!
      exD <- doesFileExist d
      if exD
        then putStrLn "Destination file already exists"
        else do
          copyFile s d
          exS <- doesFileExist d
          when exS (removeFile s)
  in do
    -- create mailbox if needed
    exD <- doesDirectoryExist p
    unless exD
           (mapM_ (\c -> createDirectoryIfMissing True (p </> c))
                  ["new","cur","tmp"])
    -- copy the file
    mySafeCopy (file m) targetFile
    putStrLn "\b...done"
applyAction m (GenAction a) = a m
applyAction m RemAction = removeFile (file m)
applyAction m (PrintAction f) = mapM_ putStrLn $ f m
applyAction m _         = error "applyAction: action not implemented yet!"
