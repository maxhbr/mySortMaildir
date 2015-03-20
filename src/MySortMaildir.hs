{-# LANGUAGE CPP #-}
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
  , Config (..)
  , Mail (..), subject, from, to, cc
  , Action (..)
  , Rule (..)
  -- useful functions for creating rules:
  , isAnyInfix
  ) where

import           System.Directory
import           System.FilePath
import           System.FilePath.Posix
import           System.Posix.Files
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map as M

--------------------------------------------------------------------------------
--  Main function to call

runMySortMaildir :: [Config] -> IO ()
runMySortMaildir cfgs = do
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

--------------------------------------------------------------------------------
--  Data definitions

data Config = C { inbox :: FilePath
                , rules :: [Rule]
                }

data Mail = M { file :: FilePath
              , rawContent :: String
              , content :: String
              , allHeaders :: (M.Map String String)
              } deriving (Eq,Show)
emptyMail = M { file = ""
              , rawContent = ""
              , content = ""
              , allHeaders = M.empty }
myLookup k m = case M.lookup k (allHeaders m) of
  Nothing -> ""
  Just v -> map toLower v
subject = myLookup "Subject"
from = myLookup "From"
to = words . myLookup "To"
cc = words . myLookup "Cc"

data Action = MoveTo FilePath | GenAction (Mail -> IO())

data Rule = R { name :: String
              , rule :: Mail -> Bool
              , action :: Action
              }

--------------------------------------------------------------------------------
--  Useful functions for creating rules

isAnyInfix :: String -> Mail -> Bool
isAnyInfix needle m = any (needle `isInfixOf`) (to m)
                    || any (needle `isInfixOf`) (cc m)
                    || needle `isInfixOf` from m

--------------------------------------------------------------------------------
--  Functions to get all mails (in a parsed form)

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
        rawCtn <- readFile filePath
        return $ parseMail (emptyMail { file = filePath
                                      , rawContent = rawCtn })
                           (lines rawCtn)
  in do
    ex <- doesDirectoryExist (inb </> cur)
    if ex
      then do
        allfilespre <- getDirectoryContents (inb </> cur)
        files <- filterFiles (filterDots allfilespre)
        mapM getMail files
      else error "INBOX not found"

--------------------------------------------------------------------------------
--  Functions to parse the a mail

parseMail :: Mail -> [String] -> Mail
parseMail m ls =  parseMail' m ls ""

parseMail' :: Mail -> [String] -> String -> Mail
parseMail' m []      r = parseMail'' m r
#if 0
parseMail' m ([]:ls) r = parseMail'' (m { content = unlines ls }) r
#else
parseMail' m ([]:ls) r = parseMail'' m r
#endif
parseMail' m (l:ls)  r | " " `isPrefixOf` l = parseMail' m ls (r++l)
                       | otherwise          = parseMail' (parseMail'' m r) ls l

#if 0
parseMail'' :: Mail -> String -> Mail
parseMail'' m r | "From:"    `isPrefixOf` r = m { from    = remKW  r }
                | "Subject:" `isPrefixOf` r = m { subject = remKW  r }
                | "To:"      `isPrefixOf` r = m { to      = remKWl r }
                | "Cc:"      `isPrefixOf` r = m { cc      = remKWl r }
                | otherwise                 = m
  where
    remKW  = unwords . tail . splitOn ":" . map toLower
    remKWl = words . remKW
#else
parseMail'' :: Mail -> String -> Mail
parseMail'' m r = let
    mySplit :: String -> (String,String)
    mySplit s = mySplit' "" s
    mySplit' :: String -> String -> (String,String)
    mySplit' r []       = (r,"")
    mySplit' r (':':ss) = (r,ss)
    mySplit' r (s:ss)   = mySplit' (r++[s]) ss

    kv = mySplit r
  in m { allHeaders = (M.insert (fst kv) (snd kv) (allHeaders m)) }
#endif

--------------------------------------------------------------------------------
--  Functions to apply the rules

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
