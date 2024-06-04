{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Core (main) where

import Control.Monad (unless, when)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.List (intersperse)
import Data.Maybe
import Foreign.C.Types
import SDL hiding (Texture)
import qualified SDL
import SDL.Vect
import Sound.ALUT
import Sound.OpenAL
import Sound.OpenAL.AL.Listener
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import Prelude hiding (any, mapM_)

-- import Paths_sdl2 (getDataFileName)

import Control.Applicative

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
    surface <- SDL.loadBMP filePath
    size <- SDL.surfaceDimensions surface
    let key = V4 0 maxBound maxBound maxBound
    SDL.surfaceColorKey surface $= Just key
    t <- SDL.createTextureFromSurface r surface
    SDL.freeSurface surface
    return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point V2 CInt) -> Maybe (V2 Bool) -> IO ()
renderTexture r (Texture t size) xy clip theta center flips =
    let dstSize =
            maybe size (\(SDL.Rectangle _ size') -> size') clip
     in SDL.copyEx
            r
            t
            clip
            (Just (SDL.Rectangle xy dstSize))
            (fromMaybe 0 theta)
            center
            (fromMaybe (pure False) flips)

main :: IO ()
main = do
    -- Initialise ALUT and eat any ALUT-specific commandline flags.
    withProgNameAndArgs runALUT $ \progName args -> do
        -- Check for correct usage.
        unless (length args == 1) $ do
            hPutStrLn stderr ("usage: " ++ progName ++ " <fileName>")
            exitFailure

        -- If everything is OK, play the sound file and exit when finished.
        let filename = head args

        -- Create an AL buffer from the given sound file.
        buf <- createBuffer (File filename)

        -- Generate a single source, attach the buffer to it and start playing.
        source <- genObjectName

        buffer source $= Just buf

        -- listenerPosition $= Vertex3 0 0 0 -- wow this works

        play [source]

        -- Normally nothing should go wrong above, but one never knows...
        errs <- get alErrors
        unless (null errs) $ do
            hPutStrLn stderr (concat (intersperse "," [d | ALError _ d <- errs]))
            exitFailure
        print "about to waitWhilePlaying"

        SDL.initialize [SDL.InitVideo]

        SDL.HintRenderScaleQuality $= SDL.ScaleLinear
        do
            renderQuality <- SDL.get SDL.HintRenderScaleQuality
            when (renderQuality /= SDL.ScaleLinear) $
                putStrLn "Warning: Linear texture filtering not enabled!"

        window <-
            SDL.createWindow
                "SDL Tutorial"
                SDL.defaultWindow{SDL.windowInitialSize = V2 screenWidth screenHeight}
        SDL.showWindow window

        renderer <-
            SDL.createRenderer
                window
                (-1)
                SDL.RendererConfig
                    { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                    , SDL.rendererTargetTexture = False
                    }

        SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

        pressTexture <- loadTexture renderer "press.bmp"
        upTexture <- loadTexture renderer "up.bmp"
        downTexture <- loadTexture renderer "down.bmp"
        leftTexture <- loadTexture renderer "left.bmp"
        rightTexture <- loadTexture renderer "right.bmp"

        --  begin SDL loop
        let loop theta = do
                events <- map SDL.eventPayload <$> SDL.pollEvents
                let quit = SDL.QuitEvent `elem` events

                keyMap <- SDL.getKeyboardState
                let texture =
                        if
                            | keyMap SDL.ScancodeUp -> upTexture
                            | keyMap SDL.ScancodeDown -> downTexture
                            | keyMap SDL.ScancodeLeft -> leftTexture
                            | keyMap SDL.ScancodeRight -> rightTexture
                            | otherwise -> pressTexture

                SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
                SDL.clear renderer

                renderTexture renderer texture 0 Nothing Nothing Nothing Nothing

                SDL.present renderer
                let listenPos =
                        if
                            | keyMap SDL.ScancodeUp -> (\(Vertex3 a b c) -> Vertex3 (a + 0.1) b c)
                            | keyMap SDL.ScancodeDown -> (\(Vertex3 a b c) -> Vertex3 (a - 0.1) b c)
                            | keyMap SDL.ScancodeLeft -> (\(Vertex3 a b c) -> Vertex3 a (b - 0.1) c)
                            | keyMap SDL.ScancodeRight -> (\(Vertex3 a b c) -> Vertex3 a (b + 0.1) c)
                            | keyMap SDL.ScancodeZ -> const $ Vertex3 0 0 0
                            | otherwise -> id
                listenerPosition $~ listenPos
                let pos = Vertex3 (cos theta * 5) 0 (sin theta * 5)
                sourcePosition source $= pos
                state <- get (sourceState source)
                sleep 0.01
                lpos <- get listenerPosition
                hPrint stderr lpos
                unless quit (loop (theta + (pi / 60)))

        -- end SDL loop
        loop 0
        -- clean up SDL
        SDL.destroyWindow window
        SDL.quit
