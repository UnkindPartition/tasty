-- | Console output format
--
-- These console formats tasty uses when reporing.
-- They are exported so custom providers with custom report printing
-- can re-use the tasty output formats
module Test.Tasty.ConsoleFormat
  ( ConsoleFormat(..)
  , ConsoleFormatPrinter
  , failFormat
  , infoFailFormat
  , infoOkFormat
  , okFormat
  , skippedFormat
  )
where

import System.Console.ANSI

-- | Console output format
data ConsoleFormat = ConsoleFormat
  { consoleIntensity :: ConsoleIntensity
  , colorIntensity   :: ColorIntensity
  , color            :: Color
  }

-- | Type of console format printer functions
type ConsoleFormatPrinter
  =  ConsoleFormat -- ^ selected console format
  -> IO ()         -- ^ action to be executed with active console format
  -> IO ()

-- | Format used to display failures
failFormat :: ConsoleFormat
failFormat = ConsoleFormat BoldIntensity   Vivid Red

-- | Format used to display additional information on failures
infoFailFormat :: ConsoleFormat
infoFailFormat = ConsoleFormat NormalIntensity Dull  Red

-- | Format used to display sucesses
okFormat :: ConsoleFormat
okFormat = ConsoleFormat NormalIntensity Dull  Green

-- | Format used to display additional information on sucesses
infoOkFormat :: ConsoleFormat
infoOkFormat = ConsoleFormat NormalIntensity Dull  White

-- | Format used to display skipped tests
skippedFormat :: ConsoleFormat
skippedFormat = ConsoleFormat NormalIntensity Dull  Magenta
