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
parseMail' m ([]:_) r = parseMail'' m r
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
parseMail'' m l = let
    mySplit :: String -> (String,String)
    mySplit = mySplit' ""
    mySplit' :: String -> String -> (String,String)
    mySplit' r (':':ss) = (r,parseItem ss)
    mySplit' r (s:ss)   = mySplit' (r++[toLower s]) ss
    mySplit' r []       = (r,"")
  in m { allHeaders = uncurry M.insert (mySplit l) (allHeaders m) }
#endif

-- encoded-word = "=?" charset "?" encoding "?" encoded-text "?="
--      "Subject: =?UTF-8?B?".base64_encode($subject)."?="
--      "Subject: =?UTF-8?Q?".imap_8bit($subject)."?="
parseItem :: String -> String
parseItem = parseItem' ""
  where
    parseItem' r ('=':('?':ss)) = parseItem' (r ++ parseItem reencoded)
                                             (last spted)
      where
        spted      = spt1 "" ss
          where
            spt1 r' ('?':ss')       = r' : spt2 "" ss'
            spt1 r' (s:ss')         = spt1 (r' ++ [s]) ss'
            spt1 _ []               = error "Encing problem (1)"
            spt2 r' ('?':ss')       = r' : spt3 "" ss'
            spt2 r' (s:ss')         = spt2 (r' ++ [s]) ss'
            spt2 _ []               = error "Encing problem (2)"
            spt3 r' ('?':('=':ss')) = [r',ss']
            spt3 r' (s:ss')         = spt3 (r' ++ [s]) ss'
            spt3 r' []              = [r',""]
        charset    = head spted
        encoding   = head $ tail spted
        raw        = head $ tail $ tail spted
        reencoded' = case encoding of
          "B" -> Base64.decode raw
          "Q" -> raw -- imap_8bit encoding
          _   -> raw
        reencoded = case charset of
          "UTF-8"      -> reencoded' -- UTF8.toString $ UTF8.fromRep reencoded'
          "iso-8859-1" -> reencoded'
          "us-ascii"   -> reencoded'
          _            -> reencoded'
    parseItem' r (s':ss)        = parseItem' (r ++ [s']) ss
    parseItem' r []             = r

--------------------------------------------------------------------------------
--  Functions to apply the rules

applyRules :: [Rule] -> Mail -> IO()
applyRules [] _ = return ()
  -- putStrLn $ "no rule found (From: " ++ from m ++ ")"
applyRules (r:rs) m = if rule r m
  then do
    putStr $ "apply rule " ++ show r ++ " ... "
    applyAction m (action r)
  else applyRules rs m

applyAction :: Mail -> Action -> IO ()
applyAction _ _ = putStr "+"
-- applyAction m (MoveTo p) = let 
--     sPath          = splitPath (file m)
--     dSPath         = drop (length sPath - 2) sPath
--     targetDir      = p </> head dSPath
--     targetFile     = targetDir </> head (tail dSPath)
--     mySafeCopy s d = do 
--       -- TODO: Improve!
--       exD <- doesFileExist d
--       if exD
--         then putStrLn "Destination file already exists"
--         else do
--           copyFile s d
--           exS <- doesFileExist d
--           when exS (removeFile s)
--   in do
--     -- create mailbox if needed
--     exD <- doesDirectoryExist p
--     unless exD
--            (mapM_ (\c -> createDirectoryIfMissing True (p </> c))
--                   ["new","cur","tmp"])
--     -- copy the file
--     mySafeCopy (file m) targetFile
--     putStrLn "done"
-- applyAction m (GenAction a) = a m
-- applyAction m RemAction = removeFile (file m)
