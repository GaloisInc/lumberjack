{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Reader
import           Data.Functor.Contravariant
import           Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Control.Monad.Catch as X
import           Lumberjack
import           System.IO ( stderr )

----------------------------------------------------------------------
-- Base example:

instance HasLog T.Text IO where
  -- The base IO monad does not have direct "storage" ability in the
  -- monad itself, so it can really only support basic/default
  -- operations which preclude some of the ancillary techniques such
  -- as adding tags automatically.  Lumberjack provides some default
  -- functions to support logging directly in the IO monad if this is
  -- desired.
  getLogAction    = return defaultGetIOLogAction

exampleTextLoggingInIO :: IO ()
exampleTextLoggingInIO = do

  -- This function represents the main code that logging output should
  -- be generated from.  Here's an example of generating a log message:

  writeLogM $ T.pack "This is a logged text message in base IO"

  -- In situations where the current monad doesn't provide the log
  -- action, it's possible to provide that directly:

  let myLogAction = LogAction TIO.putStrLn
  writeLog myLogAction $ T.pack "This is another text message, logged in IO with a custom action"


----------------------------------------------------------------------
-- Example 2: Logging strings using a contramapped converter

instance HasLog [Char] IO where
  -- The defaultGetIOLogAction logs Text, but if the code needed to
  -- log Strings, the contramap functionality can be used to simplify
  -- the adaptation of the existing logger to a new input type.
  getLogAction = return $ T.pack >$< defaultGetIOLogAction

exampleStringLoggingInIO :: IO ()
exampleStringLoggingInIO = do
  writeLogM ("This is a logged string message in base IO" :: String)


----------------------------------------------------------------------
-- Example 3: Storing the LogAction in a local monad stack

type ReaderEnv = LogAction MyMonad T.Text

newtype MyMonad a = MyMonad { runMyMonad :: ReaderT ReaderEnv IO a }
  deriving ( Applicative, Functor, Monad, MonadReader ReaderEnv, MonadIO )

instance HasLog T.Text MyMonad where
  getLogAction = ask

instance LoggingMonad T.Text MyMonad where
  adjustLogAction a = local a

exampleStringLoggingInMyMonad :: MyMonad ()
exampleStringLoggingInMyMonad = do
  writeLogM $ T.pack "This is a logged string message in MyMonad"
  adjustLogAction (contramap (("LOG> " :: T.Text) <>)) $ do
    writeLogM $ T.pack "The logger message can be adjusted"


----------------------------------------------------------------------
-- Example 4: Logging information-rich message objects.  Lumberjack
-- helpfully provides a common rich message object.  Other message
-- objects can be defined and logged, but the Lumberjack LogMessage
-- attempts to provide a useful set of functionality so that a custom
-- msg type is frequently unnecessary.

type ReaderEnv2 = LogAction MyMonad2 LogMessage

newtype MyMonad2 a = MyMonad2 { runMyMonad2 :: ReaderT ReaderEnv2 IO a }
  deriving ( Applicative, Functor, Monad, MonadReader ReaderEnv2
           , X.MonadThrow, X.MonadCatch, MonadIO )

instance HasLog LogMessage MyMonad2 where
  getLogAction = ask

instance LoggingMonad LogMessage MyMonad2 where
  adjustLogAction a = local a

-- The above is sufficient to log LogMessage objects, but for
-- convenience, Text can be logged directly as well, using the
-- conversion builtin here.
instance HasLog T.Text MyMonad2 where
  getLogAction = asks $ contramap textToLogMessage
    where
      textToLogMessage t = msgWith { logText = t, logLevel = Info }


exampleStringLoggingInMyMonad2 :: MyMonad2 ()
exampleStringLoggingInMyMonad2 = do

  -- As noted above, this function represents the main body of code.
  -- The logging messages would be interspersed in this code at
  -- appropriate locations to generate the various logged information.

  writeLogM $ msgWith { logText = "This is a logged string message in MyMonad" }

  -- withLogTag is a helper to set the logTags field for subsequently logged messages
  withLogTag "loc" "inner" $ do
    writeLogM $ msgWith { logText = "doing stuff..." }
    withLogTag "style" "(deep)" $ do

      -- Tags accumulate and are applied to all messages logged.
      writeLogM $ msgWith { logText = "deep thinking",
                            logLevel = Info
                          }

      -- There's also a HasLog for simple messages in this monad
      writeLogM $ ("Text messages can be logged as well" :: T.Text)

    -- Calls to other functions can be logged on entry and exit by
    -- simply using this wrapper.  Note also that this is outside of
    -- the inner withLogTag context, so only the outer tags are
    -- applied, but the context for those tags extends to the logging
    -- from the functions being called.
    logFunctionCallM "invoking subFunction" $ subFunction

  -- Helpers can be used to log various types of information.  Here is
  -- an indication of progress being made by the code.
  logProgressM "making good progress"

  writeLogM $ msgWith { logText = "Done now", logLevel = Warning }


subFunction :: (WithLog LogMessage m, Monad m) => m ()
subFunction =
  -- An example of a monadic function called that can perform logging
  -- with minimal constraints on the current Monad type.
  writeLogM $ msgWith { logText = "subFunction executing" }


----------------------------------------------------------------------

main = do
  exampleTextLoggingInIO
  exampleStringLoggingInIO

  -- The monad stack can just use the regular IO logging action
  -- because the monad stack has MonadIO.
  runReaderT (runMyMonad exampleStringLoggingInMyMonad) defaultGetIOLogAction

  -- Or something different could be configured... without changing
  -- the target code doing the logging
  -- (e.g. exampleStringLoggingInMyMonad).
  runReaderT (runMyMonad exampleStringLoggingInMyMonad) $
    LogAction $ liftIO . \m -> do putStr "LOGMSG << "
                                  TIO.putStr m
                                  putStrLn " >>"

  -- Richer messages allow for more detailed information.  Of
  -- particular interest, the target code identifies the information
  -- relative to the code (like the severity of the message) but the
  -- handler sets the time of log and performs the conversion from the
  -- LogMessage to the Text that can be output by the base logger used.
  let richStderrLogger = addLogActionTime $
                         cvtLogMessageToANSITermText >$< defaultGetIOLogAction
  writeLogM ("** Example of rich message logging" :: String)
  runReaderT (runMyMonad2 exampleStringLoggingInMyMonad2) richStderrLogger


  -- Sometimes it's convenient to send log output to multiple sources.
  -- In this example, warnings and above are logged to the console,
  -- but all messages are logged to a file (without ANSI terminal
  -- color codes).  Again, note that the target code containing the
  -- logging code does not change, only the logger configuration here.
  --
  -- Note that the `cvtLogMessage...` functions are provided by
  -- Lumberjack for a standard method of formatting the LogMessage
  -- supported by Lumberjack.  It's possible to write entirely
  -- different formatting functions for the LogMessage and use those
  -- instead.
  --
  -- It's also a good idea to use the `safeLogAction` wrapper to
  -- ensure that exceptions generated by the Logger simply cause log
  -- messages to be discarded rather than causing failure of the
  -- entire application.
  let consoleLogger = logFilter (\m -> Warning <= logLevel m ) $
                      cvtLogMessageToANSITermText >$<
                      defaultGetIOLogAction
      fileLogger = safeLogAction $
                   addLogActionTime $
                   cvtLogMessageToPlainText >$<
                   LogAction (liftIO . TIO.appendFile "./example.log" . flip (<>) "\n")
      failingLogger = safeLogAction $  -- remove this and the app will exit prematurely
                      addLogActionTime $
                      cvtLogMessageToPlainText >$<
                      LogAction (liftIO . TIO.appendFile "/bogus/location/to/log/to" . flip (<>) "\n")
  writeLogM ("** Example of rich message logging to multiple outputs (see ./example.log)" :: String)
  runReaderT (runMyMonad2 exampleStringLoggingInMyMonad2) $
    consoleLogger <> failingLogger <> fileLogger

  putStrLn "end of example"
