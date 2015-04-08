--------------------------------------------------------------------------------
-- |
-- Module      : MySortMaildir.Colors
-- Note        :
--
-- Stolen from: https://github.com/schell/steeloverseer/blob/master/src/ANSIColors.hs
--
--------------------------------------------------------------------------------
module MySortMaildir.Colors
  where

data ANSIColor = ANSIBlack
               | ANSIRed
               | ANSIGreen
               | ANSIYellow
               | ANSIBlue
               | ANSIMagenta
               | ANSICyan
               | ANSIWhite
               | ANSINone
    deriving (Ord, Eq)

instance Show ANSIColor where
    show ANSINone = "\27[0m"
    show c = let
        colorNum = length $ takeWhile (/= c) [ ANSIBlack
                                             , ANSIRed
                                             , ANSIGreen
                                             , ANSIYellow
                                             , ANSIBlue
                                             , ANSIMagenta
                                             , ANSICyan
                                             , ANSIWhite ]
      in "\27[" ++ show (30 + colorNum c) ++ "m"

--------------------------------------------------------------------------------
--  String coloring
colorString :: ANSIColor -> String -> String
colorString c s = show c ++ s ++ show ANSINone

redString :: String -> String
redString = colorString ANSIRed
greenString :: String -> String
greenString = colorString ANSIGreen
yellowString :: String -> String
yellowString = colorString ANSIYellow
blueString :: String -> String
blueString = colorString ANSIBlue
magentaString :: String -> String
magentaString = colorString ANSIMagenta
cyanString :: String -> String
cyanString = colorString ANSICyan
whiteString :: String -> String
whiteString = colorString ANSIWhite

--------------------------------------------------------------------------------
--  Print colored strings
colorPrint :: ANSIColor -> String -> IO ()
colorPrint c = putStrLn . colorString c

redPrint :: String -> IO ()
redPrint = colorPrint ANSIRed
greenPrint :: String -> IO ()
greenPrint = colorPrint ANSIGreen
yellowPrint :: String -> IO ()
yellowPrint = colorPrint ANSIYellow
bluePrint :: String -> IO ()
bluePrint = colorPrint ANSIBlue
magentaPrint :: String -> IO ()
magentaPrint = colorPrint ANSIMagenta
cyanPrint :: String -> IO ()
cyanPrint = colorPrint ANSICyan
whitePrint :: String -> IO ()
whitePrint = colorPrint ANSIWhite
