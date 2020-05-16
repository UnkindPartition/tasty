-- | This module can be used by providers to perform colorful/formatted
-- output and possibly re-use tasty's own output formats.
module Test.Tasty.Providers.ConsoleFormat
  ( ResultDetailsPrinter(..)
  , ConsoleFormat(..)
  , ConsoleFormatPrinter
  , noResultDetails
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

-- | Noop result details printer. The default for most providers.
noResultDetails :: ResultDetailsPrinter
noResultDetails = ResultDetailsPrinter . const . const $ return ()

-- | An action that prints additional information about a test using
-- colors/formatting; see 'Test.Tasty.Providers.testFailedDetails' and
-- 'Test.Tasty.Runners.resultDetailsPrinter'.
--
-- As input, this action is provided with the current indentation level and
-- a 'ConsoleFormatPrinter', which tells it how perform output.
--
-- This is a newtype to allow a 'Show' instance.
newtype ResultDetailsPrinter = ResultDetailsPrinter
  (Int -> ConsoleFormatPrinter -> IO ())

instance Show ResultDetailsPrinter where
  show _printer = "ResultDetailsPrinter"

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
