-- | This module can be used by providers to perform colorful/formatted
-- output and possibly re-use tasty's own output formats.
--
-- @since 1.3.1
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
  , ConsoleFormatType(..)
  , stdFormat
  , getFormat
  )
where

import System.Console.ANSI
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Console output format
--
-- @since 1.3.1
data ConsoleFormat = ConsoleFormat
  { consoleIntensity :: ConsoleIntensity
  , colorIntensity   :: ColorIntensity
  , color            :: Color
  }

-- | Type of console format printer functions
--
-- @since 1.3.1
type ConsoleFormatPrinter
  =  ConsoleFormat -- ^ selected console format
  -> IO ()         -- ^ action to be executed with active console format
  -> IO ()

-- | Noop result details printer. The default for most providers.
--
-- @since 1.3.1
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
--
-- @since 1.3.1
newtype ResultDetailsPrinter = ResultDetailsPrinter
  (Int -> ConsoleFormatPrinter -> IO ())

instance Show ResultDetailsPrinter where
  show _printer = "ResultDetailsPrinter"

-- | Format used to display failures
--
-- @since 1.3.1
failFormat :: ConsoleFormat
failFormat = ConsoleFormat BoldIntensity   Vivid Red

-- | Format used to display additional information on failures
--
-- @since 1.3.1
infoFailFormat :: ConsoleFormat
infoFailFormat = ConsoleFormat NormalIntensity Dull  Red

-- | Format used to display sucesses
--
-- @since 1.3.1
okFormat :: ConsoleFormat
okFormat = ConsoleFormat NormalIntensity Dull  Green

-- | Format used to display additional information on sucesses
--
-- @since 1.3.1
infoOkFormat :: ConsoleFormat
infoOkFormat = ConsoleFormat NormalIntensity Dull  White

-- | Format used to display skipped tests
--
-- @since 1.3.1
skippedFormat :: ConsoleFormat
skippedFormat = ConsoleFormat NormalIntensity Dull  Magenta

-- | Enumeration of supported 'ConsoleFormat's
--
-- @since 1.5.1
data ConsoleFormatType
  = FormatFail
  | FormatInfoFail
  | FormatOk
  | FormatInfoOk
  | FormatSkipped

-- | Default 'ConsoleFormat's
--
-- @since 1.5.1
stdFormat :: ConsoleFormatType -> ConsoleFormat
stdFormat FormatFail = failFormat
stdFormat FormatInfoFail = infoFailFormat
stdFormat FormatOk = okFormat
stdFormat FormatInfoOk = infoOkFormat
stdFormat FormatSkipped = skippedFormat

-- | If the appropriate environment variable has been set,
-- and can be parsed, return the 'ConsoleFormat' that it describes,
-- otherwise use the standard format ('stdFormat').
--
-- An environment variable consists of three words that
-- describe the 'ConsoleIntensity', 'ColorIntensity', and 'Color'
-- of the format. Here is the definition of the standard formats:
--
-- > TASTY_FORMAT_FAIL="BoldIntensity Vivid Red"
-- > TASTY_FORMAT_INFO_FAIL="NormalIntensity Dull Red"
-- > TASTY_FORMAT_OK="NormalIntensity Dull Green"
-- > TASTY_FORMAT_INFO_OK="NormalIntensity Dull White"
-- > TASTY_FORMAT_SKIPPED="NormalIntensity Dull Magenta"
--
-- @since 1.5.1
getFormat :: ConsoleFormatType -> IO ConsoleFormat
getFormat t = do
  mpal <- lookupEnv (formatName t)
  case mpal of
    Nothing -> return (stdFormat t)
    Just pal ->
      case parseFormatVal pal of
        Just fmt -> return fmt
        Nothing -> return (stdFormat t)
  where
    formatName :: ConsoleFormatType -> String
    formatName FormatFail = "TASTY_FORMAT_FAIL"
    formatName FormatInfoFail = "TASTY_FORMAT_INFO_FAIL"
    formatName FormatOk = "TASTY_FORMAT_OK"
    formatName FormatInfoOk = "TASTY_FORMAT_INFO_OK"
    formatName FormatSkipped = "TASTY_FORMAT_SKIPPED"

    parseFormatVal :: String -> Maybe ConsoleFormat
    parseFormatVal str =
      case words str of
        [w1, w2, w3] ->
          ConsoleFormat <$> readMaybe w1 <*> readMaybe w2 <*> readMaybe w3
        _ -> Nothing
