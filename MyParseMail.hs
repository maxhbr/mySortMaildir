{-# LANGUAGE CPP #-}
module MyParseMail
  ( parseMail
  ) where

import           Data.List
import           Data.List.Split
import           Data.Char

import           Common

--------------------------------------------------------------------------------
parseMail :: Mail -> [String] -> Mail
parseMail m ls =  parseMail' m ls ""

--------------------------------------------------------------------------------
parseMail' :: Mail -> [String] -> String -> Mail
parseMail' m []     r = parseMail'' m r
#if 0
parseMail' m ([]:ls) r = parseMail'' (m { content = unlines ls }) r
#else
parseMail' m ([]:ls) r = parseMail'' m r
#endif
parseMail' m (l:ls) r | " " `isPrefixOf` l = parseMail' m ls (r++l)
                      | otherwise          = parseMail' (parseMail'' m r) ls l

--------------------------------------------------------------------------------
parseMail'' :: Mail -> String -> Mail
parseMail'' m r | "From:"    `isPrefixOf` r = m { from    = remKW  r }
                | "Subject:" `isPrefixOf` r = m { subject = remKW  r }
                | "To:"      `isPrefixOf` r = m { to      = remKWl r }
                | "Cc:"      `isPrefixOf` r = m { cc      = remKWl r }
                | otherwise                 = m
  where
    remKW  = unwords . tail . splitOn ":" . map toLower
    remKWl = words . remKW
