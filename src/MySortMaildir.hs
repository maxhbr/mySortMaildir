{-# LANGUAGE CPP #-}
{-# OPTIONS -fwarn-unused-imports #-}
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
  , module X
  ) where

import           MySortMaildir.Common
import           MySortMaildir.Colors as X
import           MySortMaildir.Common as X hiding (emptyMail)
import           MySortMaildir.Helpers as X
import           MySortMaildir.GetMails
import           MySortMaildir.Actions

--------------------------------------------------------------------------------
--  Main function to call
-- Takes
--      a list of configs
-- and runs them all
runMySortMaildir :: [Config] -> IO ()
runMySortMaildir cfgs = let 
    ----------------------------------------------------------------------------
    -- Takes
    --      a list of rules and
    --      a mail
    -- and applies the action of the first rule, whose condition is satisfied
    applyRules :: [Rule] -> Mail -> IO()
    applyRules [] _ = return ()
      -- putStrLn $ "no rule found (From: " ++ from m ++ ")"
    applyRules (r:rs) m = if rule r m
      then do
        greenPrint $ "apply rule " ++ show r ++ " ... "
        applyAction m (action r)
      else applyRules rs m
  in do
    line >> cyanPrint "Start"
    mapM_ (\cfg -> do
      line >> putStrLn ("work on: " ++ inbox cfg ++ " ...")
      (curMails,newMails) <- getMails (inbox cfg)
      putStrLn $ "   " ++ show (length curMails) ++ " current mails found"
      putStrLn $ "   " ++ show (length newMails) ++ " new mails found"
      mapM_ (applyRules $ rules cfg) (curMails ++ newMails)) cfgs
    line >> cyanPrint "Done"
  where
    line = bluePrint $ replicate 60 '='
