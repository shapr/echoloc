module Main where

{-
   PlayFile.hs (adapted from playfile.c in freealut)
   Copyright (c) Sven Panne 2005-2016
   This file is part of the ALUT package & distributed under a BSD-style license.
   See the file LICENSE.
-}

import Control.Monad (unless, when)
import Data.List (intersperse)
import Sound.ALUT
import Sound.OpenAL
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- This program loads and plays a variety of files.

playFile :: FilePath -> IO ()
playFile fileName = do
    -- Create an AL buffer from the given sound file.
    buf <- createBuffer (File fileName)

    -- Generate a single source, attach the buffer to it and start playing.
    source <- genObjectName

    buffer source $= Just buf
    play [source]

    -- Normally nothing should go wrong above, but one never knows...
    errs <- get alErrors
    unless (null errs) $ do
        hPutStrLn stderr (concat (intersperse "," [d | ALError _ d <- errs]))
        exitFailure
    print "about to waitWhilePlaying"
    -- Check every 0.1 seconds if the sound is still playing.
    let waitWhilePlaying theta = do
            sleep 0.01
            let pos = Vertex3 (cos theta * 5) 0 (sin theta * 5)
            hPutStrLn stderr $ show pos
            sourcePosition source $= pos
            state <- get (sourceState source)
            when (state == Playing) $
                waitWhilePlaying (theta + (pi / 60))
    waitWhilePlaying 0
    print "done with waitWhilePlaying"

main :: IO ()
main = do
    -- Initialise ALUT and eat any ALUT-specific commandline flags.
    withProgNameAndArgs runALUT $ \progName args -> do
        -- Check for correct usage.
        unless (length args == 1) $ do
            hPutStrLn stderr ("usage: " ++ progName ++ " <fileName>")
            exitFailure

        -- If everything is OK, play the sound file and exit when finished.
        playFile (head args)
