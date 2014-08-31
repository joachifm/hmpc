{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified MPD

import Control.Applicative ((<$>), (*>))
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Trans.Either (EitherT(..))

import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.String (fromString)

import System.Environment (getArgs)

------------------------------------------------------------------------

main :: IO ()
main = do
  (cmdName, cmdArgs) <- viewArgv <$> getArgs
  maybe (unknownCommand cmdName)
        (run . ($ cmdArgs))
        (lookup cmdName commands)
  where
    viewArgv []     = ("status", [])
    viewArgv (x:xs) = (x, xs)

    unknownCommand name = fail ("unknown command: " ++ name)

------------------------------------------------------------------------

type Client a = EitherT MPD.ClientError IO a

run :: Client a -> IO a
run m = do
  r <- runEitherT m
  case r of
   Left e  -> fail (show e)
   Right x -> return x

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
  ]

------------------------------------------------------------------------

add = MPD.run . foldr1 (*>) . map (MPD.add . fromString)

clear _ = MPD.run MPD.clear

consume _ = MPD.run (MPD.consume True)

current _ = do
  st <- MPD.run MPD.status
  unless (MPD.statusPlaybackState st == "stop") $
    liftIO . putStrLn . formatCurrentSong . fromJust =<< MPD.run MPD.currentSong

help _ = liftIO . putStr . unlines $ map fst commands

find [typ, qry] = liftIO . putStr . unlines . map (show . MPD.songFile) =<<
  MPD.run (MPD.find (read typ MPD.=? fromString qry))
find _ = return ()

listAll xs = liftIO . putStr . unlines . fileNames =<<
  MPD.run (MPD.listAll . maybe "" fromString $ listToMaybe xs)
  where
    fileNames = mapMaybe $ \case MPD.LsFile n -> Just (show n); _ -> Nothing

ls xs = liftIO . putStr . unlines . map (show . fmt) =<<
  MPD.run (MPD.lsInfo . fmap fromString $ listToMaybe xs)
  where
    fmt (MPD.LsSongInfo x)       = MPD.songFile x
    fmt (MPD.LsDirInfo x _)      = x
    fmt (MPD.LsPlaylistInfo x _) = x

next _ = MPD.run MPD.next

pause _ = MPD.run MPD.pause

play xs = MPD.run (MPD.play . fmap read $ listToMaybe xs)

playlist _ = liftIO . putStr . unlines . map formatCurrentSong =<<
  MPD.run MPD.playlistInfo

previous _ = MPD.run MPD.previous

random _ = MPD.run (MPD.random True)

repeat' _ = MPD.run (MPD.repeat True)

rescan xs = do
  r <- MPD.run (MPD.rescan . fmap fromString $ listToMaybe xs)
  liftIO $ print r

seek xs = case xs of
  [pos, dest] -> MPD.run (MPD.seek (read pos) (read dest))
  _ -> error "seek <pos> <num>"

single _ = MPD.run (MPD.single True)

shuffle _ = MPD.run (MPD.shuffle Nothing)

status _ = do
  st <- MPD.run MPD.status
  unless (MPD.statusPlaybackState st == "stop") $ do
    Just cur <- MPD.run MPD.currentSong
    liftIO $ putStrLn (formatCurrentSong cur)
    liftIO $ putStrLn (formatPlaybackStatus st)
  liftIO $ putStrLn (formatPlaybackOptions st)

stats _ = do
  st <- MPD.run MPD.stats
  liftIO $ print st

stop _ = MPD.run MPD.stop

update :: [String] -> Client ()
update xs = do
  r <- MPD.run (MPD.update . fmap fromString $ listToMaybe xs)
  liftIO $ print r

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
