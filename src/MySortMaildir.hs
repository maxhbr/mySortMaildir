--------------------------------------------------------------------------------
-- |
-- Module      : MySortMaildir
-- Note        :
--
--
--
--------------------------------------------------------------------------------
module MySortMaildir
  ( runMySortMaildir
  ) where

import           System.Directory
import           System.FilePath
import           System.FilePath.Posix
import           System.Posix.Files
import           Control.Monad
import           Data.Char

import           Common
import           Config
import           MyGetMails

--------------------------------------------------------------------------------
--  runMySortMaildir

runMySortMaildir :: IO ()
runMySortMaildir = do
    line >> putStrLn "Start"
    mapM_ (\cfg -> do
      line >> putStrLn ("work on: " ++ inbox cfg ++ " ...")
      (curMails,newMails) <- getMails (inbox cfg)
      putStrLn $ "   " ++ show (length curMails) ++ " current mails found"
      putStrLn $ "   " ++ show (length newMails) ++ " new mails found"
      mapM_ (applyRules $ rules cfg) (curMails ++ newMails)
      ) cfgs
    line >> putStrLn "Done"
  where
    line = putStrLn $ replicate 60 '='

applyRules :: [Rule] -> Mail -> IO()
applyRules [] m = return ()
  -- putStrLn $ "no rule found (From: " ++ from m ++ ")"
applyRules (r:rs) m = if rule r m
  then do
    putStr $ "apply rule " ++ name r ++ " ... "
    applyAction m (action r)
  else applyRules rs m

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
    putStrLn "done"
applyAction m (GenAction a) = a m
