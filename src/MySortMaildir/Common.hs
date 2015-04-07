module MySortMaildir.Common
  ( Config (..)
  , Mail (..), emptyMail, subject, from, to, cc, getAge
  , Action (..)
  , Rule (..)
  ) where

import           Data.Char
import qualified Data.Map as M

--------------------------------------------------------------------------------
--  Data definitions

data Mail = M { file :: FilePath
              , rawContent :: String
              , content :: String
              , allHeaders :: M.Map String String
              , isNew :: Bool
              } deriving (Eq,Show)
emptyMail :: Mail
emptyMail = M { file = ""
              , rawContent = ""
              , content = ""
              , allHeaders = M.empty
              , isNew = False }
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
getAge :: Mail -> Int
getAge = undefined

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

