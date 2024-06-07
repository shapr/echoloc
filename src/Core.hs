{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core (main) where

import Control.Monad hiding (mapM_)
import qualified Control.Monad.Trans.State as State
import Data.Foldable hiding (elem)
import Data.List (intersperse)
import Data.Maybe
import Foreign.C.Types
import Linear
import SDL hiding (Playing, Texture)
import qualified SDL

import Sound.ALUT
import Sound.OpenAL
import Sound.OpenAL.AL.Listener
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import Prelude hiding (any, mapM_)

import Control.Monad.IO.Class (MonadIO, liftIO)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture !SDL.Texture !(V2 CInt)

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
    withProgNameAndArgs runALUT $ \_progName _args -> do
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
        loopingMode huntingSource $= Looping -- this plays until you die or escape
        attackBuf <- createBuffer (File "wilhelm.wav")
        attackSource <- genObjectName
        buffer attackSource $= Just attackBuf

        -- Normally nothing should go wrong above, but one never knows...
        errs <- get alErrors
        unless (null errs) $ do
            hPutStrLn stderr (concat (intersperse "," [d | ALError _ d <- errs]))
            exitFailure
        print ("main loop starts" :: String)

        SDL.initializeAll

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

        let roomRadius = 25

        --  begin SDL loop
        let loop = do
                (Hunted gameState) <- State.get
                events <- map SDL.eventPayload <$> SDL.pollEvents
                let quit = SDL.QuitEvent `elem` events

                keyMap <- SDL.getKeyboardState

                let listenPos :: V2 ALfloat =
                        if
                            | keyMap SDL.ScancodeUp -> newLocation 0.1 (degreesToOrientation' $ getPlayerDegrees gameState) (getPlayerPosition gameState)
                            | keyMap SDL.ScancodeDown -> newLocation (negate 0.1) (degreesToOrientation' $ getPlayerDegrees gameState) (getPlayerPosition gameState)
                            | keyMap SDL.ScancodeZ -> (V2 0 0)
                            | otherwise -> (getPlayerPosition gameState)
                listenerPosition $= v2ToVertex3 listenPos

                let degrees' =
                        if
                            | keyMap SDL.ScancodeLeft -> getPlayerDegrees gameState - 2
                            | keyMap SDL.ScancodeRight -> getPlayerDegrees gameState + 2
                            | keyMap SDL.ScancodeX -> 0
                            | otherwise -> getPlayerDegrees gameState
                --  default listener orientation is (Vector3 0 0 (-1), Vector3 0 1 0)
                orientation $~ \(_, v2) -> (degreesToOrientation degrees', v2)

                -- monster update
                -- are we close enough to eat the player?
                let deathDistance = distanceV2 (getMonsterPosition gameState) (getPlayerPosition gameState)
                let quit' = deathDistance < 1
                when (deathDistance < 1) $ do
                    sourcePosition attackSource $= v2ToVertex3 (getPlayerPosition gameState) -- play the sound where the player is listening!
                    playIfNotPlaying attackSource
                -- update the monster location
                let vectorFromMonsterToPlayer = getPlayerPosition gameState - getMonsterPosition gameState
                    vmpNorm = norm vectorFromMonsterToPlayer
                    monsterStep = fmap (* (0.025 / vmpNorm)) vectorFromMonsterToPlayer
                    monsterNewLocation = getMonsterPosition gameState + monsterStep
                -- update the monster sound location
                sourcePosition huntingSource $= v2ToVertex3 monsterNewLocation
                -- end monster update
                let updatedGameState = Hunted $ GS listenPos monsterNewLocation degrees'

                -- lpos <- get listenerPosition
                -- hPrint stderr $ "where are you? " <> show lpos
                -- lorient <- get orientation
                -- hPrint stderr $ "looking in which direction?" <> show lorient
                liftIO $ hPrint stderr $ "How  far away is the monster? " <> show deathDistance

                sleep 0.01 -- 10 ms, or 100 frames per second
                if quit' then State.put Died else State.put updatedGameState
                when quit' $ playUntilDone attackSource
                unless (quit || quit') loop

        -- end SDL loop
        let defaultGameState = GS (V2 0 0) (V2 (0 - roomRadius) 0) 0
        -- the monster is hunting you!
        play [huntingSource]
        State.evalStateT loop (Hunted defaultGameState)
        -- clean up SDL
        SDL.destroyWindow window
        SDL.quit

data GamePhase = Hunted GameState | Died | Escaped

playIfNotPlaying :: (MonadIO m) => Source -> m ()
playIfNotPlaying s = do
    state <- get (sourceState s)
    unless (state == Playing) $ play [s]

playUntilDone :: (MonadIO m) => Source -> m ()
playUntilDone s = do
    state <- get (sourceState s)
    when (state == Playing) $ do
        sleep 0.1
        playUntilDone s
    pure ()

-- d = √ [(x2 – x1)2 + (y2 – y1)2]
distanceV2 :: V2 ALfloat -> V2 ALfloat -> ALfloat
distanceV2 (V2 x1 y1) (V2 x2 y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

-- X and Y only! life is hard enough already
data GameState = GS
    { getPlayerPosition :: !(V2 ALfloat)
    , getMonsterPosition :: !(V2 ALfloat) -- where's the monster?
    , getPlayerDegrees :: !Int -- zero is true north, 180 or -180 is true south
    }
    deriving (Eq, Ord, Show)

v2ToVertex3 :: V2 ALfloat -> Vertex3 ALfloat
v2ToVertex3 (V2 x z) = Vertex3 x 0 (negate z) -- XXX axis flipped?

v2ToVector3 :: V2 ALfloat -> Vector3 ALfloat
v2ToVector3 (V2 x z) = Vector3 x 0 (negate z) -- XXX axis flipped?

-- vertex3ToV2 :: Vertex3 ALfloat -> V2 ALfloat
-- vertex3ToV2 (Vertex3 x _ z) = V2 x z

degreesToOrientation' :: Int -> V2 ALfloat
degreesToOrientation' d = V2 x y
  where
    x = sin (fromIntegral d * 2 * pi / 360)
    y = cos (fromIntegral d * 2 * pi / 360)

degreesToOrientation :: Int -> Vector3 ALfloat
degreesToOrientation = v2ToVector3 . degreesToOrientation'

-- distance -> orientation -> Location -> Location
newLocation :: ALfloat -> V2 ALfloat -> V2 ALfloat -> V2 ALfloat
newLocation dist orient location = fmap (* dist) orient + location

{- | assume "north" is zero degrees
degreesToOrientation :: Int -> Vector3 ALfloat
degreesToOrientation d = Vector3 x 0 z
  where
    x = sin (fromIntegral d * 2 * pi / 360)
    z = cos (fromIntegral d * 2 * pi / 360)
-}

{-
              state <- get (sourceState source)
            when (state == Playing) $
                waitWhilePlaying (theta + (pi / 60))
                V3 has a Num instance

-}
