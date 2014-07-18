{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified MPD.Core as MPD
import qualified MPD
import MPD.CommandStr ((.+))

import Control.Applicative
import Control.Monad (join, unless)
import Data.Monoid
import System.Environment (getArgs, getEnv)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as List

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
    ( "add", MPD.run . foldr1 (*>) . map MPD.add . map T.pack )
  , ( "clear", \_ -> MPD.run MPD.clear )
  , ( "current", \_ -> currentSong >>= maybe (return ()) (T.putStrLn . formatCurrentSong) )
  , ( "help", \_ -> putStr . unlines $ map (\(n, _) -> n) commands )
  , ( "ls", \xs -> MPD.run (MPD.listAll (if null xs then "" else T.pack (head xs))) >>= print )
  , ( "next", \_ -> MPD.run MPD.next )
  , ( "pause", \_ -> MPD.run (MPD.command "pause" (return ())) )
  , ( "play", \_ -> MPD.run (MPD.play Nothing) )
  , ( "previous", \_ -> MPD.run MPD.previous )
  , ( "random", \_ -> MPD.run (MPD.random True) )
  , ( "shuffle", \_ -> MPD.run (MPD.shuffle Nothing) )
  , ( "stop", \_ -> MPD.run MPD.stop )
  , ( "status", \_ -> do
      (st, cur) <- MPD.run ((,) <$> MPD.status <*> MPD.currentSong)
      unless (MPD.statusPlaybackState st == "stop") $ do
        T.putStrLn (formatCurrentSong cur)
      T.putStrLn (formatStatus st) )
  ]
  where
    currentSong = do
      st <- MPD.run MPD.status
      if MPD.statusPlaybackState st /= "stop"
        then Just `fmap` MPD.run MPD.currentSong
        else return Nothing

    formatCurrentSong si =
      let Just artist = si `MPD.viewTag` "Artist"
          Just title  = si `MPD.viewTag` "Title"
      in T.unwords [ artist, "-", title ]

    formatStatus st = T.intercalate "\t"
      [ "volume: "  <> MPD.statusVolume st
      , "repeat: "  <> MPD.statusRepeatEnabled st
      , "random: "  <> MPD.statusRandomEnabled st
      , "single: "  <> MPD.statusSingleEnabled st
      , "consume: " <> MPD.statusConsumeEnabled st
      ]
