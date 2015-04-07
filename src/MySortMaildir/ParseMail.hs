module MySortMaildir.ParseMail 
  ( parseMail
  ) where

import           Data.Char
import           Data.List
import qualified Data.Map as M

-- encodings:
import qualified Codec.Binary.Base64.String as Base64
import qualified Data.String.UTF8 as UTF8

import           Debug.Trace

import           MySortMaildir.Common

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


