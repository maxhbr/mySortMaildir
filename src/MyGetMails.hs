module MyGetMails
  ( getMails
  ) where
import           System.Directory
import           System.FilePath
import           System.FilePath.Posix
import           System.Posix.Files
import           Control.Monad

import           Common
import           MyParseMail

getMails :: FilePath -> IO ([Mail],[Mail])
getMails inb = do
  cur <- getMails' inb "cur"
  new <- getMails' inb "new"
  return (cur,new)

getMails' :: FilePath -> FilePath -> IO [Mail]
getMails' inb cur = let
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
