{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified MPD

import Control.Applicative ((<$>), (*>))
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Trans.Except

import Control.Monad.Reader (ReaderT(..), ask)

import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.String (fromString)
import qualified Data.Text.IO as T

import Network (PortID(..))
import System.IO (Handle)

import System.Environment (getArgs)

------------------------------------------------------------------------

main :: IO ()
main = do
  (cmdName, cmdArgs) <- viewArgv <$> getArgs
  maybe (unknownCommand cmdName)
        (runClient . ($ cmdArgs))
        (lookup cmdName commands)
  where
    viewArgv []     = ("status", [])
    viewArgv (x:xs) = (x, xs)

    unknownCommand name = fail ("unknown command: " ++ name)

------------------------------------------------------------------------

type Client a = ReaderT Handle (ExceptT MPD.ClientError IO) a

runMPD :: MPD.Command a -> Client a
runMPD c = ask >>= \hdl -> lift (MPD.run hdl c)

runClient :: Client a -> IO a
runClient c = MPD.withConn "localhost" (PortNumber 6600) $ \hdl ->do
  r <- runExceptT (runReaderT c hdl)
  either (fail . show) return r

------------------------------------------------------------------------

commands :: [(String, [String] -> Client ())]
commands =
  [
    ( "add", add )
  , ( "clear", clear )
  , ( "consume", consume )
  , ( "current", current )
  , ( "find", find )
  , ( "help", help )
  , ( "listall", listAll )
  , ( "ls", ls )
  , ( "next", next )
  , ( "pause", pause )
  , ( "play", play )
  , ( "playlist", playlist )
  , ( "prev", previous )
  , ( "random", random )
  , ( "repeat", repeat' )
  , ( "rescan", rescan )
  , ( "seek", seek )
  , ( "shuffle", shuffle )
  , ( "single", single )
  , ( "status", status )
  , ( "stats", stats )
  , ( "stop", stop )
  , ( "update", update )
  , ( "volume", volume )
  ]

------------------------------------------------------------------------

add = runMPD . foldr1 (*>) . map (MPD.add . fromString)

clear _ = runMPD MPD.clear

consume _ = runMPD (MPD.consume True)

current _ = do
  st <- runMPD MPD.status
  unless (MPD.statusPlaybackState st == MPD.PlaybackStopped) $
    liftIO . putStrLn . formatCurrentSong . fromJust =<< runMPD MPD.currentSong

help _ = liftIO . putStr . unlines $ map fst commands

find [typ, qry] = liftIO . putStr . unlines . map (show . MPD.songFile) =<<
  runMPD (MPD.find (read typ MPD.=? fromString qry))
find _ = return ()

listAll xs = liftIO . mapM_ f =<< runMPD (MPD.listAll . maybe "" fromString $ listToMaybe xs)
  where f (MPD.LsFile n) = T.putStrLn (MPD.unPath n)
        f _              = return ()

ls xs = liftIO . putStr . unlines . map (show . fmt) =<<
  runMPD (MPD.lsInfo . fmap fromString $ listToMaybe xs)
  where
    fmt (MPD.LsSongInfo x)    = MPD.songFile x
    fmt (MPD.LsDirInfo x _)   = x
    fmt (MPD.LsPListInfo x _) = x

next _ = runMPD MPD.next

pause _ = runMPD MPD.pause

play xs = runMPD (MPD.play . fmap read $ listToMaybe xs)

playlist _ = liftIO . putStr . unlines . map formatCurrentSong =<<
  runMPD MPD.playlistInfo

previous _ = runMPD MPD.previous

random _ = runMPD (MPD.random True)

repeat' _ = runMPD (MPD.repeat True)

rescan xs = do
  r <- runMPD (MPD.rescan . fmap fromString $ listToMaybe xs)
  liftIO $ print r

seek xs = case xs of
  [dest] -> runMPD (MPD.seekCur (read dest))
  _ -> error "seek <dest>"

single _ = runMPD (MPD.single True)

shuffle _ = runMPD (MPD.shuffle Nothing)

status _ = do
  st <- runMPD MPD.status
  unless (MPD.statusPlaybackState st == MPD.PlaybackStopped) $ do
    Just cur <- runMPD MPD.currentSong
    liftIO $ putStrLn (formatCurrentSong cur)
    liftIO $ putStrLn (formatPlaybackStatus st)
  liftIO $ putStrLn (formatPlaybackOptions st)

stats _ = do
  st <- runMPD MPD.stats
  liftIO $ print st

stop _ = runMPD MPD.stop

update :: [String] -> Client ()
update xs = do
  r <- runMPD (MPD.update . fmap fromString $ listToMaybe xs)
  liftIO $ print r

volume xs = case xs of [x] -> runMPD (MPD.setVolume (read x))
                       _   -> error "volume <num>"

------------------------------------------------------------------------

formatCurrentSong si = unwords [
    maybe "(none)" show (si `MPD.viewTag` "Artist")
  , "-"
  , maybe "(none)" show (si `MPD.viewTag` "Title")
  ]

formatPlaybackOptions st = intercalate "\t" [
    "volume: "  <> show (MPD.statusVolume st) <> "%"
  , "repeat: "  <> show (MPD.statusRepeatEnabled st)
  , "random: "  <> show (MPD.statusRandomEnabled st)
  , "single: "  <> show (MPD.statusSingleEnabled st)
  , "consume: " <> show (MPD.statusConsumeEnabled st)
  ]

formatPlaybackStatus st = intercalate "\t" [
    "[" <> show (MPD.statusPlaybackState st) <> "]"
  , "#" <> show (fromJust (MPD.statusSongPos st))
  , show (fromJust (MPD.statusTime st)) <> " (%)"
  ]
