module Common
  where

import           Data.List

data Config = C { inbox :: FilePath
                , rules :: [Rule]
                }
data Mail = M { file :: FilePath
              , content :: String
              , subject :: String
              , from :: String
              , to :: [String]
              , cc :: [String]
              } deriving (Eq,Show)
data Action = MoveTo FilePath | GenAction (Mail -> IO())
data Rule = R { name :: String
              , rule :: Mail -> Bool
              , action :: Action
              }

emptyM = M { file = ""
           , content = ""
           , subject = ""
           , from = ""
           , to = []
           , cc = [] }

--------------------------------------------------------------------------------
--  common functions

isAnyInfix :: String -> Mail -> Bool
isAnyInfix needle m = any (needle `isInfixOf`) (to m)
                    || any (needle `isInfixOf`) (cc m)
                    || needle `isInfixOf` from m
