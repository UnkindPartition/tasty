-- | Note: this module is re-exported as a whole from "Test.Tasty.Runners"
module Test.Tasty.Runners.Utils where

import Control.Exception
import Control.DeepSeq
import Control.Applicative
import Text.Printf

-- | Printing exceptions or other messages is tricky — in the process we
-- can get new exceptions!
--
-- See e.g. https://github.com/feuerbach/tasty/issues/25
formatMessage :: String -> IO String
formatMessage msg = go 3 msg
  where
    -- to avoid infinite recursion, we introduce the recursion limit
    go :: Int -> String -> IO String
    go 0        _ = return "exceptions keep throwing other exceptions!"
    go recLimit msg = do
      mbStr <- try $ evaluate $ force msg
      case mbStr of
        Right str -> return str
        Left e' -> printf "message threw an exception: %s" <$> go (recLimit-1) (show (e' :: SomeException))
