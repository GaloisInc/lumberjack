{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-------------------------------------------
-- |
-- Module      : Lumberjack
-- Copyright   : (c) Galois Inc. 2020
-- Maintainer  : kquick@galois.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module defines a general logging facility that can be used to
-- output log messages to various targets.
--
-- The 'LogAction' is the fundamental operation that decides how to
-- log a provided message.
--
-- Code wishing to output a logged message simply uses the LogAction
-- object:
--
-- > writeLog action msg
--
-- For convenience, the LogAction can be stored in the local operating
-- monad context, from which it can be retrieved (and modified).  A
-- monad which can supply a LogAction is a member of the HasLog class,
-- and the 'writeLogM' function will automatically retrieve the
-- LogAction from the monad and write to it:
--
-- > writeLogM msg
--
-- LogActions can be combined via Semigroup operations (<>) and the
-- resulting LogAction will perform both actions with each message.
-- The Monoidal mempty LogAction simply does nothing.  For example,
-- logging to both a file and stdout can be done by @logToFile <>
-- logToStdout@.
--
-- LogActions are also Contravariant (and Divisible and Decidable) to
-- allow easy conversion of a LogAction for the base message type into
-- a LogAction for a different message type (or types) that can be
-- converted to (and combined into) the base message type.
-------------------------------------------

module Lumberjack
  ( -- * Interface for Logging
    LogAction(..)
  , HasLog(..)
  , LoggingMonad(..)
  , writeLogM
    -- * Logging Utilities
    --
    -- The following utility functions can be used to adjust or wrap
    -- LogActions to provide additional functionality.
  , safeLogAction
  , logFilter
    -- * LogMessage rich logging type
    -- $richMsgType
  , Severity(..)
  , LogType(..)
  , LogMessage(..)
  , msgWith
  , WithLog
  , withLogTag
  , addLogActionTime
    -- ** Output formatting for LogMessage
    -- $richMsgFormatting
  , cvtLogMessageToPlainText
  , cvtLogMessageToANSITermText
    -- * Helpers and convenience functions
    -- $helpers
  , (|#)
  , logFunctionCall, logFunctionCallM
  , logProgress, logProgressM
  , tshow
  , defaultGetIOLogAction
  )
where

import           Control.Monad (when)
import qualified Control.Monad.Catch as X
import           Control.Monad.IO.Class
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Monoid hiding ( (<>) )
import           Data.Semigroup
import           Data.Text ( Text, pack, empty )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP_Term
import qualified Prettyprinter.Render.Text as PP_Text
import           Data.Time.Clock ( UTCTime(..), getCurrentTime, diffUTCTime )
import           Data.Time.Format ( defaultTimeLocale, formatTime )
import           Data.Void
import           System.IO ( stderr )

import           Prelude


-- ----------------------------------------------------------------------

-- * Interface for Logging

-- | The LogAction holds the ability to log a message of type @msg@
-- (the second parameter) via a monad @m@ (the first parameter).
--
-- LogActions are semigroup and monoid combineable, which results in
-- both LogActions being taken (or no action in the case of mempty),
-- and contravariant to allow the msg to be modified via function
-- prior to being logged (as well as Divisible and Decidable).
newtype LogAction m msg = LogAction { writeLog :: msg -> m () }

instance Applicative m => Semigroup (LogAction m a) where
  LogAction a1 <> LogAction a2 = LogAction $ \a -> a1 a *> a2 a

instance Applicative m => Monoid (LogAction m a) where
  mappend = (<>)
  mempty = LogAction $ \_ -> pure ()

instance Contravariant (LogAction m) where
  contramap f (LogAction a) = LogAction $ a . f

instance (Applicative m) => Divisible (LogAction m) where
  conquer = LogAction $ \_ -> pure ()
  divide splitf lLog rLog = LogAction $ \i ->
    let (l, r) = splitf i
        ll = writeLog lLog l
        rl = writeLog rLog r
    in ll *> rl

instance (Applicative m) => Decidable (LogAction m) where
  lose f = LogAction $ \a -> absurd (f a)
  choose split l r = LogAction $ either (writeLog l) (writeLog r) . split


-- | Any monad which will support retrieving a LogAction from the
-- Monad's environment should support the 'HasLog' class.
class Monad m => HasLog msg m where
  getLogAction :: m (LogAction m msg)


-- | This type is a Constraint that should be applied to any client
-- function that will perform logging in a monad context.  The @msg@
-- is the type of message that will be logged, and the @m@ is the
-- monad under which the logging is performed.
--
-- /Note/ that 'WithLog' historically had additional functionality over 'HasLog'
-- but no longer and is therefore deprecated in favor of 'HasLog'.
type WithLog msg m = ({- X.MonadCatch m, -} HasLog msg m)
{-# DEPRECATED WithLog "Use HasLog instead" #-}


-- | An instance of the 'LoggingMonad' class can be defined for the
-- base monadic logging action to allow adjusting that logging action.
-- This class can only be instantiated (and only needs to be
-- instantiated) for the base message type; all other message types
-- will use contramapping to convert their message type to the
-- 'LoggingMonad' base message type.
class (Monad m, HasLog msg m) => LoggingMonad msg m where
  adjustLogAction :: (forall k. LogAction k msg -> LogAction k msg) -> m a -> m a


-- | This obtains the 'LogAction' from the current monad's environment
-- to use for outputting the log message.  Most code will use this function.
writeLogM :: HasLog msg m => msg -> m ()
writeLogM m = getLogAction >>= flip writeLog m


----------------------------------------------------------------------
-- * Logging Utilities


-- | Ensures that the LogAction does not fail if the logging operation
-- itself throws an exception (the exception is ignored).
safeLogAction :: X.MonadCatch m => LogAction m msg -> LogAction m msg
safeLogAction a = LogAction $ \m -> X.catch (writeLog a m) (\(_ex :: X.SomeException) -> return ())


-- | The logFilter can be used on a LogAction to determine which
-- messages the LogAction should be invoked for (only those for which
-- the filter function returns True).
logFilter :: Applicative m => (msg -> Bool) -> LogAction m msg -> LogAction m msg
logFilter f (LogAction l) = LogAction $ \m -> when (f m) (l m)



----------------------------------------------------------------------
-- * LogMessage rich logging type

-- $richMsgType
--
-- This is an enhanced message type for the LogAction, containing
-- various auxiliary information associated with the log message.
-- While "Lumberjack" can be used with other message types, this
-- message type should provide support for most of the common logging
-- auxiliary data and can therefore be used "out of the box".
--
-- /Note/ the 'LogMessage' type is a convenience type; lumberjack can be used
-- with any message type desired.


-- | The Severity indicates the relative importance of the logging
-- message.  This can be useful for filtering log messages.
data Severity = Debug | Info | Warning | Error deriving (Ord, Eq, Show)


-- | The LogType indicates what type of message this is.  These are
-- printed on the log line and can be used for filtering different
-- types of log messages.
data LogType = Progress | FuncEntry | FuncExit | MiscLog | UserOp
  deriving (Eq, Show)


-- | Each logged output is described by a LogMessage object.
data LogMessage = LogMessage { logType :: LogType
                             , logLevel :: Severity
                             , logTime :: UTCTime
                             , logTags :: [(Text, Text)]
                             , logText :: Text
                             }

instance Semigroup LogMessage where
  a <> b = LogMessage { logType = if logType a == MiscLog then logType b else logType a
                      , logLevel = max (logLevel a) (logLevel b)
                      , logTime = max (logTime a) (logTime b)
                      , logTags = logTags a <> logTags b
                      , logText = case (T.null (logText a), T.null (logText b)) of
                                  (False, False) -> logText a <> "; " <> logText b
                                  (True, False) -> logText b
                                  _ -> logText a
                      }

instance Monoid LogMessage where
  mempty = LogMessage MiscLog Debug (UTCTime (toEnum 0) (toEnum 0)) [] empty
  mappend = (<>)


-- | Helper routine to return an empty LogMessage, whose fields can
-- then be updated.
msgWith :: LogMessage
msgWith = mempty


-- | This operator is a convenient infix operator for logging a Text
-- message.  This is especially useful when used in conjunction with
-- the @OverloadedStrings@ language pragma:
--
--   >>> warning|# "This is your last warning"
--   >>> error|# "Failure has occurred"
--
-- The '|#' will convert the 'Text' to a 'LogMessage', and the user's function
-- passed as a first argument can convert the 'LogMessage' into the preferred
-- logging type for that application.  If the application continued to use
-- 'LogMessage' then it could simply provide the following definition:
--
-- > warning msg = msg { logLevel = Warning }

(|#) :: (LogMessage -> a) -> Text -> a
o |# t = o (msgWith { logText = t })
infixr 0 |#


-- | Add the current timestamp to the LogMessage being logged
addLogActionTime :: MonadIO m => LogAction m LogMessage -> LogAction m LogMessage
addLogActionTime a = LogAction $ \m -> do t <- liftIO getCurrentTime
                                          writeLog a $ m <> mempty { logTime = t }


-- | Log messages can have any number of key/value tags applied to
-- them.  This function establishes a new key/value tag pair that will
-- be in effect for the monadic operation passed as the third
-- argument.
-- withLogTag tname tval op = local (adjustLogAction $ addLogTag tname tval) op
withLogTag :: (LoggingMonad LogMessage m) => Text -> Text -> m a -> m a
withLogTag tname tval op =
    let tagmsg = mempty { logTags = [(tname, tval)] }
    in (adjustLogAction $ contramap (tagmsg <>)) op


-- ----------------------------------------------------------------------
-- * Output formatting for LogMessage

-- $richMsgFormatting
--
-- When the 'LogMessage' logging type is used, "Lumberjack" provides a
-- standard set of output formatting functions.  The output uses the
-- prettyprinter package to generate 'Prettyprinter.Doc' output with
-- annotations specifying the type of markup to be applied to various
-- portions of the output.
--
-- There are multiple rendering functions that can be supplied as
-- contramap converters to the base 'LogAction'.  One rendering
-- function outputs a log message in plain text, while the other uses
-- the prettyprinter-ansi-terminal package to generate various ANSI
-- highlighting and color codes for writing enhanced output to a TTY.


-- | Normal LogMessage formatting uses prettyprinter output with a
-- 'PrettyLogAnn' annotation type which assigns different annotations
-- to different parts of the log message.  This is achieved by calling
-- 'prettyLogMessage'.
--
-- Alternatively, the 'Prettyprinter.Pretty' class @pretty@ method can
-- be used to get log message formatting for generic annotation types,
-- but the different parts of the message will not be distinguished
-- via annotation values.
data PrettyLogAnn = AnnLogType LogType
                  | AnnSeverity Severity
                  | AnnTime
                  | AnnTimeMinSec
                  | AnnTag
                  | AnnTagVal

-- Use prettyLogType instead
instance PP.Pretty LogType where pretty = anyPrettyLogType

anyPrettyLogType :: LogType -> PP.Doc ann
anyPrettyLogType Progress  = PP.pretty ("progress" :: Text)
anyPrettyLogType FuncEntry = PP.pretty ("entered" :: Text)
anyPrettyLogType FuncExit  = PP.pretty ("completed" :: Text)
anyPrettyLogType UserOp    = PP.pretty ("User-Op" :: Text)
anyPrettyLogType MiscLog   = PP.pretty ("misc" :: Text)

prettyLogType :: LogType -> PP.Doc PrettyLogAnn
prettyLogType t = PP.annotate (AnnLogType t) $ anyPrettyLogType t

-- Use prettySev instead
instance PP.Pretty Severity where pretty = anyPrettySev

anyPrettySev :: Severity -> PP.Doc ann
anyPrettySev Error   = PP.pretty ("ERR " :: Text)
anyPrettySev Warning = PP.pretty ("Warn" :: Text)
anyPrettySev Info    = PP.pretty ("I   " :: Text)
anyPrettySev Debug   = PP.pretty ("Dbg " :: Text)

prettySev :: Severity -> PP.Doc PrettyLogAnn
prettySev s = PP.annotate (AnnSeverity s) $ anyPrettySev s

-- Use prettyTime instead
instance PP.Pretty UTCTime where
  pretty t = PP.hcat [ PP.pretty (formatTime defaultTimeLocale "%Z-%F:%H:" t)
                     , PP.pretty (formatTime defaultTimeLocale "%M:%S" t)
                     , PP.pretty (take 4 (formatTime defaultTimeLocale ".%q" t))
                     ]

prettyTime :: UTCTime -> PP.Doc PrettyLogAnn
prettyTime t =
  if t == UTCTime (toEnum 0) (toEnum 0)
  then PP.annotate AnnTime $ PP.emptyDoc
  else PP.hcat
       [ PP.annotate AnnTime $ PP.pretty (formatTime defaultTimeLocale "%Z-%F_%H:" t)
       , PP.annotate AnnTimeMinSec $ PP.pretty (formatTime defaultTimeLocale "%M:%S" t)
       , PP.annotate AnnTime $ PP.pretty (take 4 (formatTime defaultTimeLocale ".%q" t))
       ]

anyPrettyTags :: [(Text, Text)] -> PP.Doc ann
anyPrettyTags =
  let anyPrettyTag (tag, val) = PP.group $ PP.cat [ PP.pretty tag
                                                  , PP.equals
                                                  , PP.pretty val
                                                  ]
  in foldl (\acc tagval -> acc PP.<+> (anyPrettyTag tagval)) mempty

prettyTags :: [(Text, Text)] -> PP.Doc PrettyLogAnn
prettyTags =
  let ppTag (tag, val) = PP.group $ PP.hcat [ PP.annotate AnnTag $ PP.pretty tag
                                            , PP.equals
                                            , PP.annotate AnnTagVal $ PP.pretty val
                                            ]
  in foldl (\acc tagval -> acc PP.<+> (ppTag tagval)) mempty


-- | Format the log message with annotation values designating the
-- different portions of the pretty-printed value.
--
-- The 'Prettyprinter.Pretty' class @pretty@ method can be used for
-- generic annotations, but this yields less information for output
-- management.
prettyLogMessage :: LogMessage -> PP.Doc PrettyLogAnn
prettyLogMessage (LogMessage {..}) = PP.hsep [ prettyTime logTime
                                             , prettySev logLevel
                                             , PP.brackets (prettyLogType logType)
                                             , prettyTags logTags
                                             , PP.pretty logText
                                             ]

instance PP.Pretty LogMessage where
  pretty (LogMessage {..}) = PP.hsep [ PP.pretty logTime
                                     , PP.pretty logLevel
                                     , PP.brackets (PP.pretty logType)
                                     , anyPrettyTags logTags
                                     , PP.pretty logText
                                     ]


-- | The 'termStyle' converts the LogMessage annotations into ANSI
-- terminal styles to add colors and other effects such as bolding to
-- various portions of log messages (for use with
-- prettyprinter-ansi-terminal).
termStyle :: PrettyLogAnn -> PP_Term.AnsiStyle
termStyle (AnnLogType Progress)  = PP_Term.colorDull PP_Term.Green
termStyle (AnnLogType FuncEntry) = PP_Term.colorDull PP_Term.Magenta
termStyle (AnnLogType FuncExit)  = PP_Term.colorDull PP_Term.Cyan
termStyle (AnnLogType UserOp)    = PP_Term.bold <> PP_Term.color PP_Term.Green
termStyle (AnnLogType MiscLog)   = mempty
termStyle (AnnSeverity Error)   = PP_Term.bold <> PP_Term.color PP_Term.Red <> PP_Term.bgColor PP_Term.Yellow
termStyle (AnnSeverity Warning) = PP_Term.bold <> PP_Term.colorDull PP_Term.Red
termStyle (AnnSeverity Info)    = mempty
termStyle (AnnSeverity Debug)   = PP_Term.color PP_Term.Blue
termStyle AnnTime       = mempty
termStyle AnnTimeMinSec = PP_Term.color PP_Term.White <> PP_Term.bold
termStyle AnnTag    = PP_Term.color PP_Term.Black <> PP_Term.bold
termStyle AnnTagVal = PP_Term.color PP_Term.Black <> PP_Term.bold


-- | Standard 'LogMessage' rendering function to convert a
-- 'LogMessage' into 'Text' with ANSI terminal colors and bolding and
-- other styling.  This can be used as the default converter for a
-- logger (via contramap).
cvtLogMessageToANSITermText :: LogMessage -> Text
cvtLogMessageToANSITermText = PP_Term.renderStrict .
                              PP.reAnnotateS termStyle .
                              PP.layoutSmart PP.defaultLayoutOptions .
                              prettyLogMessage

-- | Standard 'LogMessage' rendering function for converting a
-- 'LogMessage' into plain 'Text' (no colors or other highlighting).
-- This can be used as the default converter for a logger (via
-- contramap).
cvtLogMessageToPlainText :: LogMessage -> Text
cvtLogMessageToPlainText = PP_Text.renderStrict .
                           PP.layoutSmart PP.defaultLayoutOptions .
                           prettyLogMessage

-- ----------------------------------------------------------------------
-- * Helpers and convenience functions

-- $helpers
-- These functions are not part of the core Logging implementation,
-- but can be useful to clients to perform common or default
-- operations.


-- | A wrapper for a function call that will call the provided
-- 'LogAction' with a 'Debug' log on entry to the function and an
-- 'Info' log on exit from the function.  The total amount of time
-- taken during execution of the function will be included in the exit
-- log message.  No strictness is applied to the invoked monadic
-- operation, so the time taken may be misleading.  Like
-- 'logFunctionCallM' but needs an explicit 'LogAction' whereas
-- 'logFunctionCallM' will retrieve the 'LogAction' from the current
-- monadic context.
logFunctionCall :: (MonadIO m) => LogAction m LogMessage -> Text -> m a -> m a
logFunctionCall = logFunctionCallWith . writeLog


-- | A wrapper for a monadic function call that will 'Debug' log on
-- entry to and 'Info' log on exit from the function.  The exit log
-- will also note the total amount of time taken during execution of
-- the function.  Be advised that no strictness is applied to the
-- internal monadic operation, so the time taken may be misleading.
logFunctionCallM :: (MonadIO m, HasLog LogMessage m) => Text -> m a -> m a
logFunctionCallM = logFunctionCallWith writeLogM


-- | Internal function implementing the body for 'logFunctionCall' or
-- 'logFunctionCallM'
logFunctionCallWith :: (MonadIO m) => (LogMessage -> m ()) -> Text -> m a -> m a
logFunctionCallWith logger fName f =
  do logger $ msgWith { logType = FuncEntry, logText = fName }
     t <- liftIO getCurrentTime
     r <- f
     t' <- liftIO getCurrentTime
     let dt = diffUTCTime t' t
     logger $ msgWith { logType = FuncExit, logLevel = Info
                      , logText = fName <> ", executed for " <> pack (show dt) }
     return r


-- | Called to output a log message to indicate that some progress in
-- the current activity has been made.
logProgress :: (MonadIO m) => LogAction m LogMessage -> Text -> m ()
logProgress action txt = writeLog action $ msgWith { logLevel = Info, logType = Progress, logText = txt }


-- | Called to output a log message within a 'HasLog' monad to indicate
-- that some progress in the current activity has been made.
logProgressM :: (MonadIO m, HasLog LogMessage m) => Text -> m ()
logProgressM txt = writeLogM $ msgWith { logLevel = Info, logType = Progress, logText = txt }


-- | This is a helper function.  The LogMessage normally wants a Text,
-- but show delivers a String, so 'tshow' can be used to get the
-- needed format.
tshow :: (Show a) => a -> Text
tshow = pack . show


-- | When using a simple IO monad, there is no ability to store a
-- LogAction in the base monad.  The client can specify a specific
-- HasLog instance for IO that is appropriate to that client, and that
-- HasLog can optionally use the 'defaultGetIOLogAction' as the
-- 'getLogAction' implementation to log pretty messages with ANSI
-- styling to stdout.
--
--  > instance HasLog Env Text IO where
--  >     getLogAction = return defaultGetIOLogAction
--
defaultGetIOLogAction :: MonadIO m => LogAction m T.Text
defaultGetIOLogAction = LogAction $ liftIO . TIO.hPutStrLn stderr
