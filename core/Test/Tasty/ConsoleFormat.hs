module Test.Tasty.ConsoleFormat
  ( ConsoleFormat(..)
  , failFormat
  , infoFailFormat
  , infoOkFormat
  , okFormat
  , skippedFormat
  )
where

import System.Console.ANSI

data ConsoleFormat = ConsoleFormat
  { consoleIntensity :: ConsoleIntensity
  , colorIntensity   :: ColorIntensity
  , color            :: Color
  }

failFormat, infoFailFormat, infoOkFormat, okFormat, skippedFormat :: ConsoleFormat
failFormat     = ConsoleFormat BoldIntensity   Vivid Red
infoFailFormat = ConsoleFormat NormalIntensity Dull  Red
infoOkFormat   = ConsoleFormat NormalIntensity Dull  White
okFormat       = ConsoleFormat NormalIntensity Dull  Green
skippedFormat  = ConsoleFormat NormalIntensity Dull  Magenta
