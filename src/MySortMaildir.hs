{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      : MySortMaildir
-- Note        : A small programm to sort maildirs
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
  , isAnyInfix, isAllAnywhere, isOneAnywhere
  ) where

import           System.Directory
import           System.FilePath
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map as M

-- encodings:
import qualified Codec.Binary.Base64.String as Base64
import qualified Data.String.UTF8 as UTF8

import           Debug.Trace

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

data Mail = M { file :: FilePath
              , rawContent :: String
              , content :: String
              , allHeaders :: M.Map String String
              } deriving (Eq,Show)
emptyMail :: Mail
emptyMail = M { file = ""
              , rawContent = ""
              , content = ""
              , allHeaders = M.empty }
myLookup :: String -> Mail -> String
myLookup k m = case M.lookup k (allHeaders m) of
  Nothing -> ""
  Just v -> map toLower v
subject :: Mail -> String
subject = myLookup "subject"
from :: Mail -> String
from = myLookup "from"
to :: Mail -> [String]
to = words . myLookup "to"
cc :: Mail -> [String]
cc = words . myLookup "cc"

data Action = MoveTo FilePath           -- move file to new path
            | RemAction                 -- remove file
            | GenAction (Mail -> IO())  -- generic action

data Rule = R { name :: String
              , rule :: Mail -> Bool
              , action :: Action
              }
instance Eq Rule where 
  r1 == r2 = name r1 == name r2
instance Show Rule where 
  show = show . name

data Config = C { inbox :: FilePath
                , rules :: [Rule]
                } deriving (Eq,Show)

--------------------------------------------------------------------------------
--  Useful functions for creating rules

isAnyInfix :: String -> Mail -> Bool
isAnyInfix needle m = any (needle `isInfixOf`) (to m)
                    || any (needle `isInfixOf`) (cc m)
                    || needle `isInfixOf` from m

isAllAnywhere :: String -> String -> Bool
isAllAnywhere ndls s = and [ndl `isInfixOf` s | ndl <- words ndls]
isOneAnywhere :: String -> String -> Bool
isOneAnywhere ndls s = or [ndl `isInfixOf` s | ndl <- words ndls]

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
  in do
    cur <- getMails' "cur"
    new <- getMails' "new"
    return (cur,new)

--------------------------------------------------------------------------------
--  Functions to parse the a mail

--------------------------------------------------------------------------------
-- Takes 
--      a mail and
--      a list of lines
-- and adds the found information in the lines to the mail
parseMail :: Mail -> [String] -> Mail
parseMail m ls = let
    parseMail' :: Mail -> [String] -> String -> Mail
    parseMail' m' []      r = parseMail'' m' r
    parseMail' m' ([]:ls) r = parseMail'' (m' { content = unlines ls }) r
    parseMail' m' (l:ls') r | " " `isPrefixOf` l = parseMail' m' ls' (r++l)
                            | otherwise          = parseMail' (parseMail'' m' r) ls' l

    parseMail'' :: Mail -> String -> Mail
    parseMail'' m' l = let
        mySplit :: String -> (String,String)
        mySplit = mySplit' ""
        mySplit' :: String -> String -> (String,String)
        mySplit' r (':':ss) = (r,parseItem ss)
        mySplit' r (s:ss)   = mySplit' (r++[toLower s]) ss
        mySplit' r []       = (r,"")
      in m' { allHeaders = uncurry M.insert (mySplit l) (allHeaders m') }
  in parseMail' m ls ""

-------------------------------------------------------------------------------
-- takes a string containing encoded parts of the form 
--      "=?" charset "?" encoding "?" encoded-text "?="
-- and returs a string, where these parts are reencoded
parseItem :: String -> String
parseItem = let 
    parseItem' r ('=':('?':ss)) =
        parseItem' (r ++ parseItem (reencode (take 3 (spted ss))))
                   (parseItem                (last   (spted ss)))
      where
        -----------------------------------------------------------------------
        -- Splits a string of the form 
        --      "=?charset?encoding?encoded-text?=rest"
        -- into
        --      ["charset","encoding","encoded-text","rest"]
        spted ss' = let 
            spt1 r' ('?':ss'')       = r' : spt2 "" ss''
            spt1 r' (s:ss'')         = spt1 (r' ++ [s]) ss''
            spt1 _ []                = error "Encing problem (1)"
            spt2 r' ('?':ss'')       = r' : spt3 "" ss''
            spt2 r' (s:ss'')         = spt2 (r' ++ [s]) ss''
            spt2 _ []                = error "Encing problem (2)"
            spt3 r' ('?':('=':ss'')) = [r',ss'']
            spt3 r' (s:ss'')         = spt3 (r' ++ [s]) ss''
            spt3 r' []               = [r',""]
          in spt1 "" ss'
    parseItem' r (s':ss)        = parseItem' (r ++ [s']) ss
    parseItem' r []             = r
  in parseItem' ""

-------------------------------------------------------------------------------
-- Takes a list 
--      ["charset","encoding","stringToReencode"]
-- and returns a reencoded version of "stringToReencode"
reencode :: [String] -> String
reencode (charset:(encoding:[s])) = let 
    reencode' s'  = case map toLower charset of
      "utf-8"        -> s' -- TODO: UTF8.toString $ UTF8.fromRep s'
      "iso-8859-1"   -> s' -- TODO
      "iso-8859-15"  -> s' -- TODO
      "us-ascii"     -> s' -- TODO
      "windows-1252" -> s' -- TODO
      _              -> trace ("unknown charset: " ++ charset) s'
  in reencode' $ case map toLower encoding of
    "b" -> Base64.decode s
    "q" -> s -- TODO: imap_8bit encoding
    _   -> trace ("unknown encoding: " ++ encoding) s
reencode ss = trace "reencode: Input list has not the correct length" $ unwords ss

--------------------------------------------------------------------------------
--  Functions to apply the rules

--------------------------------------------------------------------------------
-- Takes
--      a list of rules and
--      a mail
-- and applies the first rule, whose condition is satisfied
applyRules :: [Rule] -> Mail -> IO()
applyRules [] _ = return ()
  -- putStrLn $ "no rule found (From: " ++ from m ++ ")"
applyRules (r:rs) m = if rule r m
  then do
    putStr $ "apply rule " ++ show r ++ " ... "
    applyAction m (action r)
  else applyRules rs m

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
    putStrLn "done"
applyAction m (GenAction a) = a m
applyAction m RemAction = removeFile (file m)
