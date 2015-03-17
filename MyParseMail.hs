module MyParseMail
  ( parseMail
  ) where

import           Data.List
import           Data.Char

import           Common

parseMail :: Mail -> [String] -> String -> Mail
parseMail m []     r = parseMail' m r
parseMail m ([]:ls) r = parseMail' (m { content = unlines ls }) r
parseMail m (l:ls) r | " " `isPrefixOf` l = parseMail m ls (r++l)
                     | otherwise          = parseMail (parseMail' m r) ls l
parseMail' :: Mail -> String -> Mail
parseMail' m r | "From:"    `isPrefixOf` r = m { from    = remKW  r }
               | "Subject:" `isPrefixOf` r = m { subject = remKW  r }
               | "To:"      `isPrefixOf` r = m { to      = remKWl r }
               | "Cc:"      `isPrefixOf` r = m { cc      = remKWl r }
               | otherwise                 = m
  where
    remKW  = unwords . tail . words . map toLower
    remKWl = tail . words . map toLower