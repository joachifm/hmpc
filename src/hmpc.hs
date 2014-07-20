{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified MPD

import Control.Applicative ((<$>), (*>))
import Control.Monad (join, unless)

import Data.Either (rights)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.Environment (getArgs, getEnv)
import Text.Printf (printf)

------------------------------------------------------------------------

main :: IO ()
main = do
  (cmdName, cmdArgs) <- viewArgv <$> getArgs
  maybe (unknownCommand cmdName) ($ cmdArgs) (lookup cmdName commands)
  where
    viewArgv []     = ("status", [])
    viewArgv (x:xs) = (x, xs)

    unknownCommand name = fail ("unknown command: " ++ name)

------------------------------------------------------------------------

commands :: [(String, [String] -> IO ())]
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
  , ( "shuffle", shuffle )
  , ( "single", single )
  , ( "status", status )
  , ( "stop", stop )
  ]

------------------------------------------------------------------------

add = MPD.run . foldr1 (*>) . map (MPD.add . T.pack)

clear _ = MPD.run MPD.clear

consume _ = MPD.run (MPD.consume True)

current _ = do
  st <- MPD.run MPD.status
  unless (MPD.statusPlaybackState st == "stop") $
    T.putStrLn . formatCurrentSong =<< MPD.run MPD.currentSong

help _ = putStr . unlines $ map fst commands

find xs = T.putStr . T.unlines . map MPD.songFile =<<
  case xs of
    [typ, qry] -> MPD.run (MPD.find (T.pack typ) (T.pack qry))
    _          -> return []

listAll xs = T.putStr . T.unlines . rights =<<
  MPD.run (MPD.listAll . maybe "" T.pack $ listToMaybe xs)

ls xs = T.putStr . T.unlines . map (either fst MPD.songFile) =<<
  MPD.run (MPD.lsInfo $ fmap T.pack (listToMaybe xs))

next _ = MPD.run MPD.next

pause _ = MPD.run MPD.pause

play xs = MPD.run (MPD.play . fmap read $ listToMaybe xs)

playlist _ = T.putStr . T.unlines . map formatCurrentSong =<<
  MPD.run MPD.playlistInfo

previous _ = MPD.run MPD.previous

random _ = MPD.run (MPD.random True)

repeat' _ = MPD.run (MPD.repeat True)

single _ = MPD.run (MPD.single True)

shuffle _ = MPD.run (MPD.shuffle Nothing)

status _ = do
  st <- MPD.run MPD.status
  unless (MPD.statusPlaybackState st == "stop") $ do
    cur <- MPD.run MPD.currentSong
    T.putStrLn (formatCurrentSong cur)
    T.putStrLn (formatPlaybackStatus st)
  T.putStrLn (formatPlaybackOptions st)

stop _ = MPD.run MPD.stop

------------------------------------------------------------------------

formatCurrentSong si = T.unwords [
    maybe "(none)" id (si `MPD.viewTag` "Artist")
  , "-"
  , maybe "(none)" id (si `MPD.viewTag` "Title")
  ]

formatPlaybackOptions st = T.intercalate "\t" [
    "volume: "  <> MPD.statusVolume st <> "%"
  , "repeat: "  <> MPD.statusRepeatEnabled st
  , "random: "  <> MPD.statusRandomEnabled st
  , "single: "  <> MPD.statusSingleEnabled st
  , "consume: " <> MPD.statusConsumeEnabled st
  ]

formatPlaybackStatus st = T.intercalate "\t" [
    "[" <> MPD.statusPlaybackState st <> "]"
  , "#" <> MPD.statusSongPos st
  , MPD.statusTotalTime st <> " (%)"
  ]
