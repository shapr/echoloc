{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
        -- Create an AL buffer from the given sound file.
        slamBuf <- createBuffer (File "slam.wav")
        slamSource <- genObjectName
        buffer slamSource $= Just slamBuf

        stepBuf <- createBuffer (File "stepstep.wav")
        stepSource <- genObjectName
        buffer stepSource $= Just stepBuf

        huntingBuf <- createBuffer (File "hunting.wav")
        huntingSource <- genObjectName
        buffer huntingSource $= Just huntingBuf

        attackBuf <- createBuffer (File "attack.wav")
        attackSource <- genObjectName
        buffer attackSource $= Just attackBuf

        -- Generate a single source, attach the buffer to it and start playing.
        source <- genObjectName

        -- the monster is hunting you!
        play [huntingSource]

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

        let maxDistance = 25

        --  begin SDL loop
        let loop theta degrees = do
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
                            | keyMap SDL.ScancodeUp -> (\(Vertex3 a b c) -> Vertex3 a (b - 0.1) c) -- move forward
                            | keyMap SDL.ScancodeDown -> (\(Vertex3 a b c) -> Vertex3 a (b + 0.1) c) -- move backward
                            | keyMap SDL.ScancodeZ -> const $ Vertex3 0 0 0
                            | otherwise -> id
                listenerPosition $~ listenPos

                let degrees' =
                        if
                            | keyMap SDL.ScancodeLeft -> degrees - 5
                            | keyMap SDL.ScancodeRight -> degrees + 5
                            | keyMap SDL.ScancodeX -> 0
                            | otherwise -> degrees
                --  default listener orientation is (Vector3 0 0 (-1), Vector3 0 1 0)
                orientation $~ \(_, v2) -> (degreesToOrientation degrees', v2)
                let pos = Vertex3 (cos theta * 5) 0 (sin theta * 5)
                sourcePosition source $= pos
                state <- get (sourceState source)
                sleep 0.01
                lpos <- get listenerPosition
                lorient <- get orientation
                hPrint stderr $ "where are you? " <> show lpos
                hPrint stderr $ "looking in which direction?" <> show lorient
                unless quit (loop (theta + (pi / 60)) degrees')

        -- end SDL loop
        loop 0 0
        -- clean up SDL
        SDL.destroyWindow window
        SDL.quit

data GameState = GS
    { monsterLocation :: Vertex3 ALfloat -- where's the monster?
    , playerDegrees :: Int -- zero is true north, 180 or -180 is true south
    }
    deriving (Eq, Ord, Show)

-- | assume "north" is zero degrees
degreesToOrientation :: Int -> Vector3 ALfloat
degreesToOrientation d = Vector3 x 0 z
  where
    x = sin (fromIntegral d * 2 * pi / 360)
    z = cos (fromIntegral d * 2 * pi / 360)

{-
              state <- get (sourceState source)
            when (state == Playing) $
                waitWhilePlaying (theta + (pi / 60))
-}
