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
import           Data.List
import           Data.Char

import           Common
import           Config
import           MyParseMail

--------------------------------------------------------------------------------
--  runMySortMaildir

runMySortMaildir :: IO ()
runMySortMaildir = do
    line >> putStrLn "Start"
    mapM_ (\cfg -> do
      line >> putStrLn ("work on: " ++ inbox cfg ++ " ...")
      curMails <- getMails (inbox cfg) "cur"
      putStrLn $ "   " ++ show (length curMails) ++ " current mails found"
      newMails <- getMails (inbox cfg) "new"
      putStrLn $ "   " ++ show (length newMails) ++ " new mails found"
      mapM_ (applyRules $ rules cfg) (curMails ++ newMails)
      ) cfgs
    line >> putStrLn "Done"
  where
    line = putStrLn $ replicate 60 '='

    getMails :: FilePath -> FilePath -> IO [Mail]
    getMails inb cur = let
      filterDots  = filter (\p -> not $ "." `isPrefixOf` p)
      filterFiles = filterM (\p -> doesFileExist (inb </> cur </> p))
      getMail :: FilePath -> IO Mail
      getMail p = let
          filePath = inb </> cur </> p
        in do
          rawContent <- readFile filePath
          return $ parseMail (emptyM {file = filePath}) (lines rawContent)
      in do
        ex <- doesDirectoryExist (inb </> cur)
        if ex
          then do
            allfilespre <- getDirectoryContents (inb </> cur)
            files <- filterFiles (filterDots allfilespre)
            mapM getMail files
          else error "INBOX not found"

    applyRules :: [Rule] -> Mail -> IO()
    applyRules [] m = return ()
      -- putStrLn $ "no rule found (From: " ++ from m ++ ")"
    applyRules (r:rs) m =let 
        sPath          = splitPath (file m)
        dSPath         = drop (length sPath - 2) sPath
        targetDir      = target r </> head dSPath
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
      in if rule r m
        then do
          putStr $ "apply rule " ++ name r ++ " ... "
          -- create mailbox if needed
          exD <- doesDirectoryExist (target r)
          unless exD
                 (mapM_ (\c -> createDirectoryIfMissing True (target r </> c))
                        ["new","cur","tmp"])
          -- copy the file
          mySafeCopy (file m) targetFile
          putStrLn "done"
        else applyRules rs                                                       m
