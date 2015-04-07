--------------------------------------------------------------------------------
-- |
-- Module      : MySortMaildir.GetMails
-- Note        :
--
--
--
--------------------------------------------------------------------------------

module MySortMaildir.GetMails
  ( getMails
  ) where

import           System.Directory
import           System.FilePath
import           Control.Monad
import           Data.List

import           MySortMaildir.Common
import           MySortMaildir.GetMails.ParseMail

--------------------------------------------------------------------------------
--  Functions to get all mails (in a parsed form)

getMails :: FilePath -> IO ([Mail],[Mail])
getMails inb = let
    getMails' cur = let
        filterDots  = filter (\p -> not $ "." `isPrefixOf` p)
        filterFiles = filterM (\p -> doesFileExist (inb </> cur </> p))
        getMail :: FilePath -> IO Mail
        getMail p = let
            filePath = inb </> cur </> p
          in do
            rawCtn <- readFile filePath
            return $ parseMail (emptyMail { file = filePath
                                          , rawContent = rawCtn
                                          , isNew = cur == "new" })
                               (lines rawCtn)
      in do
        ex <- doesDirectoryExist (inb </> cur)
        if ex
          then do
            allfilespre <- getDirectoryContents (inb </> cur)
            files <- filterFiles (filterDots allfilespre)
            mapM getMail files
          else error "INBOX not found"
  in do
    cur <- getMails' "cur"
    new <- getMails' "new"
    return (cur,new)
