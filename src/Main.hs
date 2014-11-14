{-# LANGUAGE ForeignFunctionInterface #-}

{-
module Main where

import Sound.PortAudio.Base
import Sound.PortAudio

import Control.Monad (foldM, foldM_)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)

import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

import qualified Data.Vector as V

import qualified Necronomicon.UGen as U
import Prelude


numSeconds :: Int
numSeconds = 5

sampRate :: Double
sampRate = 44100

framesPerBuffer :: Int
framesPerBuffer = 600

tableSize :: Int
tableSize = 200

data SineTable = SineTable { sine :: V.Vector Float }
data Phases = Phases { leftPhase :: Int, rightPhase :: Int }

newTable :: Int -> SineTable
newTable sze = SineTable vec where
    intSze = fromInteger $ toInteger sze
    vec = V.fromList $ map (\i -> sin $ (i / intSze) * pi * 2) [0..(intSze - 1)]

sineTable :: SineTable
sineTable = newTable tableSize

poker :: (Storable a, Fractional a) => Ptr a -> (Int, Int) -> Int -> IO (Int, Int)
poker out (l, r) i = do
    -- pokeElemOff out (2 * i)      (realToFrac $ (V.!) (sine sineTable) l)
    -- pokeElemOff out (2 * i + 1)  (realToFrac $ (V.!) (sine sineTable) r)
    pokeElemOff out (2 * i) (realToFrac . U.myCoolSynth . U.Time $ fromIntegral l)
    pokeElemOff out (2 * i + 1) (realToFrac . U.myCoolSynth . U.Time $ fromIntegral r)
    --let newL = let x = l + 1 in (if x >= tableSize then (x - tableSize) else x)
    --let newR = let x = r + 3 in (if x >= tableSize then (x - tableSize) else x)
    return (l + 1, r + 1)

paTestCallback :: MVar Phases -> StreamCallback CFloat CFloat
paTestCallback mvar _ _ frames _ out = do
    phases <- readMVar mvar

    (newL', newR') <- foldM (poker out) (leftPhase phases, rightPhase phases) [0..(fromIntegral $ frames - 1)]
        
    swapMVar mvar (phases { leftPhase = newL', rightPhase = newR' })
    return Continue

streamFinished :: String -> IO ()
streamFinished msg = putStrLn ("Stream Completed: " ++ msg)

withDefaults :: IO (Either Error ())
withDefaults = do
    tbl <- newMVar (Phases 0 0)
    
    let callback = Just $ paTestCallback tbl
        fincallback = Just $ streamFinished "Default Callback Finished!"
        
    withDefaultStream 0 2 sampRate (Just framesPerBuffer) callback fincallback $ \strm -> do
        _ <- startStream strm
        threadDelay $ numSeconds * 10000 * 10000
        _ <- stopStream strm
        return $ Right ()

withCustomSettings :: IO (Either Error ())
withCustomSettings = do
    outInfo <- getDefaultOutputInfo
    case outInfo of
        Left err -> return $ Left err
        Right (devIndex, devInfo) -> do
            tbl <- newMVar (Phases 0 0)
            let callback = Just $ paTestCallback tbl
                fincallback = Just $ streamFinished "Custom Callback Finished!"     
                outInfo' = Just $ StreamParameters devIndex 2 (defaultHighOutputLatency devInfo) 
          
            withStream Nothing outInfo' sampRate (Just framesPerBuffer) [ClipOff] callback fincallback $ \strm -> do
                _ <- startStream strm
                threadDelay $ numSeconds * 10000 * 10000
                _ <- stopStream strm
                return $ Right ()



withBlockingIO :: IO (Either Error ())
withBlockingIO = do
    let fincallback = Just $ streamFinished "Blocking IO Finished!"
        iterations  = 5000000000 :: Int
        numChannels = 2
    
    withDefaultStream 0 numChannels sampRate (Just framesPerBuffer) Nothing fincallback $ \strm -> do
        _ <- startStream strm

        allocaBytes (framesPerBuffer * numChannels) $ \out -> do
            
            out' <- newForeignPtr_ out
            
            let runFunc (l, r) _ = do
                (newL', newR') <- foldM (poker (out :: Ptr CFloat)) (l, r) [0..(fromIntegral $ framesPerBuffer - 1)]
                writeStream strm (fromIntegral framesPerBuffer) out'
                return (newL', newR')
                
            foldM_ runFunc (0,0) [0..iterations]
        
        _ <- stopStream strm
        return $ Right ()

main :: IO ()
main = do
    putStrLn $ "PortAudio Test: output sine wave. SR = " ++ (show sampRate) ++ ", BufSize = " ++ (show $ framesPerBuffer)
    
    -- Choose one of the Following,
    -- For some reason I can combine withBlockingIO with withDefaults strange...

    withPortAudio withBlockingIO
    --withPortAudio (withBlockingIO >> withCustomSettings)
    --withPortAudio (withDefaults >> withCustomSettings)
    return ()

-}


module Main where

import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK

import qualified Control.Monad.Trans.Class as Trans
import qualified Foreign.C.Error as E

import System.Environment (getProgName)
import Debug.Trace

import Data.Array.MArray (writeArray, )
import Foreign
import Foreign.C
import GHC.Float

import Prelude
--import qualified Necronomicon.UGen as U
import qualified Necronomicon.UGen as U
import Necronomicon.Patterns
import Necronomicon.Language.Layout
import Control.Applicative
--U.myCoolSynth . U.Time
import Data.Monoid

import Necronomicon.Networking.Client

foreign import ccall "startRuntime" startRuntime :: Ptr U.CUGen -> IO ()

main :: IO ()
main = do
    -- ugen <- U.compileUGen U.myCoolSynth
    -- ugenPtr <- new ugen
    -- print beat

    print melo
    -- print $ pvector 0 melo
    -- print $ pvector 1 melo
    -- print $ pvector 2 melo
    -- print $ pvector 3 melo
    -- print $ pvector 4 melo
    -- print funcs

    threadId <- imp melo
    input <- getLine
    return ()
    --runPatternDivisions 30 4 melo

    -- runPatternDivisions 30 4 (pstutter 2 $ pseq 2 [p, p2, p3])
        -- where
            -- p = [lich| b s [b d] s |]
            -- p2 = [lich| q [k l] _ [_ m] |]
            -- p3 = [lich| n n _ _ _ _ [n o n o] |]
        
    -- startRuntime ugenPtr

    -- return ()
    --poke ptr ugen
    --startRuntime ptr

-- beat = 

{-
beat = do
    n <- (PN.ptree . PN.PVal . toTree $ [lich| b s [b d] s |])
    return (PN.shuffle PN.stdGen $ show n)
-}

    

-- melo = [lich| 0 [1 2 [3 [4 5] [6 [7 8] ] 9] 10 11] 12 [13 14] _ |]
melo = [lich| 0 [1 2] _ [3 [4 5]] 6
              0 [1 2] _ [3 [4 5]] 6
              0 [1 2] _ [3 [4 5]] 6
              0 [1 2] _ [3 [4 5]] 6 |]
-- funcs= [lich| (+1) ((*2),(+2),(3/)) _ [(/2) (+2)] |]
-- mix  = [l| 1 2 s _ |]
    

{-

import Control.DeepSeq


main :: IO ()
main = do
    name <- getProgName
    JACK.handleExceptions $
        JACK.withClientDefault name $ \client ->
        JACK.withPort client "output1" $ \output ->
        JACK.withPort client "output2" $ \output2 ->
        JACK.withProcess client (process client output output2) $ do
        JACK.withActivation client $ do
            JACK.connect client (name ++ ":output1") "system:playback_1"
            JACK.connect client (name ++ ":output2") "system:playback_2"
            Trans.lift $ do
                putStrLn $ "started " ++ name ++ "..."
                JACK.waitForBreak
-}

{-
intFromNFrames :: JACK.NFrames -> Integer
intFromNFrames (JACK.NFrames n) = fromIntegral n

process :: JACK.Client -> Audio.Port JACK.Output -> Audio.Port JACK.Output -> JACK.NFrames -> Sync.ExceptionalT E.Errno IO ()
process !client !output !output2 !nframes = Trans.lift $ do
    outArr <- Audio.getBufferArray output nframes
    outArr2 <- Audio.getBufferArray output2 nframes
    frameTime <- JACK.lastFrameTime client
    case JACK.nframesIndices nframes of
        [] -> return ()
        arr@(_:_) -> do
            mapM_ (\i -> writeArray outArr i (processFunc frameTime i)) arr
            mapM_ (\i -> writeArray outArr2 i (processFunc frameTime i)) arr
            where
                processFunc !time !frame = CFloat $ force $ realToFrac (U.myCoolSynth (U.Time . fromIntegral . (+(intFromNFrames frame)) $ intFromNFrames time))
-}

{-
import qualified Necronomicon.UGen as U
import Prelude
import Sound.Pulse.Simple
import Control.DeepSeq

main=do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s ([force $ fromRational . toRational $ U.myCoolSynth $ U.Time $ fromRational t |t<-[1..44100*60]] :: [Float])
    simpleDrain s
    simpleFree s

-}
