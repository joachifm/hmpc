{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified MPD

import Control.Applicative ((<$>), (*>))
import Control.Monad (join, unless)

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
  , ( "current", current )
  , ( "help", help )
  , ( "ls", ls )
  , ( "next", next )
  , ( "pause", pause )
  , ( "play", play )
  , ( "previous", previous )
  , ( "random", random )
  , ( "shuffle", shuffle )
  , ( "stop", stop )
  , ( "status", status )
  ]

------------------------------------------------------------------------

add = MPD.run . foldr1 (*>) . map (MPD.add . T.pack)

clear _ = MPD.run MPD.clear

current _ = do
  st <- MPD.run MPD.status
  unless (MPD.statusPlaybackState st == "stop") $
    T.putStrLn . formatCurrentSong =<< MPD.run MPD.currentSong

help _ = putStr . unlines $ map fst commands

ls xs = print =<<
  MPD.run (MPD.listAll . maybe "" T.pack $ listToMaybe xs)

next _ = MPD.run MPD.next

pause _ = MPD.run MPD.pause

play _ = MPD.run (MPD.play Nothing)

previous _ = MPD.run MPD.previous

random _ = MPD.run (MPD.random True)

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

formatCurrentSong si =
  let Just artist = si `MPD.viewTag` "Artist"
      Just title  = si `MPD.viewTag` "Title"
  in T.unwords [ artist, "-", title ]

formatPlaybackOptions st = T.intercalate "\t" [
    "volume: "  <> MPD.statusVolume st
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
