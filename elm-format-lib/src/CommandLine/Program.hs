module CommandLine.Program (ProgramResult(..), ProgramIO, run, runWithProgName, failed, CommandLine.Program.error, showUsage, liftEither, liftM, liftME, mapError) where

-- Common handling for command line programs

import Prelude ()
import Relude hiding (putStrLn, exitSuccess, exitFailure)

import CommandLine.InfoFormatter (ToConsole(..))
import CommandLine.World
import qualified Data.Text as Text
import System.Exit (ExitCode(..))

import qualified Options.Applicative as OptParse


class MapError f where
    mapError :: (x -> y) -> f x a -> f y a


data ProgramResult err a
    = ShowUsage
    | ProgramError err
    | ProgramSuccess a
    | ProgramFailed
    deriving (Functor)

instance MapError ProgramResult where
    mapError f = \case
        ShowUsage -> ShowUsage
        ProgramError x -> ProgramError $ f x
        ProgramSuccess a -> ProgramSuccess a
        ProgramFailed -> ProgramFailed

instance Applicative (ProgramResult err) where
    pure a = ProgramSuccess a
    liftA2 f (ProgramSuccess a) (ProgramSuccess b) = ProgramSuccess (f a b)
    liftA2 _ (ProgramError err) _ = ProgramError err
    liftA2 _ ProgramFailed _ = ProgramFailed
    liftA2 _ ShowUsage _ = ShowUsage
    liftA2 _ _ (ProgramError err) = ProgramError err
    liftA2 _ _ ProgramFailed = ProgramFailed
    liftA2 _ _ ShowUsage = ShowUsage

instance Monad (ProgramResult err) where
    ShowUsage >>= _ = ShowUsage
    ProgramFailed >>= _ = ProgramFailed
    (ProgramError err) >>= _ = ProgramError err
    (ProgramSuccess a) >>= f = f a


newtype ProgramIO m x a =
    ProgramIO (m (ProgramResult x a))
    deriving (Functor)

instance Functor m => MapError (ProgramIO m) where
    mapError f (ProgramIO m) = ProgramIO (fmap (mapError f) m)

instance Monad m => Applicative (ProgramIO m x) where
    pure a = ProgramIO $ return $ pure a
    liftA2 f (ProgramIO a) (ProgramIO b) =
        ProgramIO $ do
            a' <- a
            b' <- b
            return $ liftA2 f a' b'

instance Monad m => Monad (ProgramIO m x) where
    (ProgramIO m) >>= f = ProgramIO (m >>= f')
        where
            f' r =
                case r of
                    ShowUsage -> return $ ShowUsage
                    ProgramError x -> return $ ProgramError x
                    ProgramFailed -> return $ ProgramFailed
                    ProgramSuccess a -> (\(ProgramIO z) -> z) $ f a

failed :: Applicative m => ProgramIO m x a
failed = ProgramIO $ pure $ ProgramFailed

error :: Applicative m => x -> ProgramIO m x a
error x = ProgramIO $ pure $ ProgramError x

showUsage :: Applicative m => ProgramIO m x a
showUsage = ProgramIO $ pure $ ShowUsage

liftEither :: Applicative m => Either x a -> ProgramIO m x a
liftEither (Left x) = ProgramIO $ pure $ ProgramError x
liftEither (Right a) = ProgramIO $ pure $ ProgramSuccess a

liftM :: Functor m => m a -> ProgramIO m x a
liftM m = ProgramIO (ProgramSuccess <$> m)

liftME :: Monad m => m (Either x a) -> ProgramIO m x a
liftME m = ProgramIO (m >>= ((\(ProgramIO z) -> z) . liftEither))


run ::
    World m =>
    ToConsole err =>
    OptParse.ParserInfo flags
    -> (flags -> ProgramIO m err ())
    -> [String]
    -> m ()
run flagsParser run' args =
    let
        parsePreferences =
            OptParse.prefs (mempty <> OptParse.showHelpOnError)

        parseFlags =
            OptParse.execParserPure parsePreferences flagsParser
    in
    do
        flags <- handleParseResultWith Nothing $ parseFlags args
        case flags of
            Nothing -> return ()
            Just flags' ->
                do
                    result <- (\(ProgramIO m) -> m) $ run' flags'
                    case result of
                        ShowUsage ->
                            (handleParseResultWith Nothing $ parseFlags ["--help"])
                                -- TODO: handleParseResult is exitSuccess, so we never get to exitFailure
                                >> exitFailure

                        ProgramError err ->
                            putStrLnStderr (toConsole err)
                                >> exitFailure

                        ProgramSuccess () ->
                            exitSuccess

                        ProgramFailed ->
                            exitFailure


{-| copied from Options.Applicative -}
handleParseResultWith :: World m => Maybe String -> OptParse.ParserResult a -> m (Maybe a)
handleParseResultWith _ (OptParse.Success a) = return (Just a)
handleParseResultWith progOverride (OptParse.Failure failure) = do
    progn <- case progOverride of
        Just name -> return (Text.pack name)
        Nothing -> getProgName
    let (msg, exit) = OptParse.renderFailure failure (Text.unpack progn)
    case exit of
        ExitSuccess -> putStrLn (Text.pack msg) *> exitSuccess *> return Nothing
        _           -> putStrLnStderr (Text.pack msg) *> exitFailure *> return Nothing
handleParseResultWith _ (OptParse.CompletionInvoked _) =
    -- do
    --     progn <- getProgName
    --     msg <- OptParse.execCompletion compl progn
    --     putStr msg
    --     const undefined <$> exitSuccess
    Relude.error "Shell completion not yet implemented"

-- | Like 'run', but override the program name used in help/usage rendering.
runWithProgName ::
    World m =>
    ToConsole err =>
    OptParse.ParserInfo flags
    -> String
    -> (flags -> ProgramIO m err ())
    -> [String]
    -> m ()
runWithProgName flagsParser progName run' args =
    let
        parsePreferences =
            OptParse.prefs (mempty <> OptParse.showHelpOnError)

        parseFlags =
            OptParse.execParserPure parsePreferences flagsParser
    in
    do
        flags <- handleParseResultWith (Just progName) $ parseFlags args
        case flags of
            Nothing -> return ()
            Just flags' ->
                do
                    result <- (\(ProgramIO m) -> m) $ run' flags'
                    case result of
                        ShowUsage ->
                            (handleParseResultWith (Just progName) $ parseFlags ["--help"])
                                -- TODO: handleParseResult is exitSuccess, so we never get to exitFailure
                                >> exitFailure

                        ProgramError err ->
                            putStrLnStderr (toConsole err)
                                >> exitFailure

                        ProgramSuccess () ->
                            exitSuccess

                        ProgramFailed ->
                            exitFailure
