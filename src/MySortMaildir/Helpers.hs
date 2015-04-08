--------------------------------------------------------------------------------
-- | 
-- Module      : MySortMaildir.Helpers
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------
module MySortMaildir.Helpers
  where

import           System.FilePath
import           Data.List

import           MySortMaildir.Common

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

basicListRule :: (String, String) -> Rule
basicListRule (mInfx,mBx) = R { name   = tail $ takeExtension mBx
                              , rule   = isAnyInfix mInfx
                              , action = MoveTo mBx }

-- apply rules only to new mails
onlyNew :: Rule -> Rule
onlyNew r = r { rule = \ m -> isNew m && rule r m}
onlyNews :: [Rule] -> [Rule]
onlyNews = map onlyNew

-- apply rules only to current mails
onlyCur :: Rule -> Rule
onlyCur r = r { rule = \ m -> not (isNew m) && rule r m}
onlyCurs :: [Rule] -> [Rule]
onlyCurs = map onlyCur

