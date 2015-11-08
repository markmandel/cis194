module LogAnalysis where

import           Data.Char
import           Log

parseMessage :: String -> LogMessage
parseMessage str@(prefix:_:rhs@(timestamp:_:rest))
  | prefix == 'I' = LogMessage Info (digitToInt timestamp) rest
  | prefix == 'E' = parseError rhs
  | prefix == 'W' = LogMessage Warning (digitToInt timestamp) rest
  | otherwise = Unknown str

-- Just parse the errors.
-- Example: E 70 3 Way too many pickles
parseError :: String -> LogMessage
parseError (severity:_:timestamp:rest) = LogMessage (Error (digitToInt severity)) (digitToInt timestamp) rest