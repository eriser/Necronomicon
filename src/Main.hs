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
        iterations  = 500 :: Int
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
