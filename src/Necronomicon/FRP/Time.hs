module Necronomicon.FRP.Time
    ( Time
    -- , tick
    -- , deltaTime
    -- , runTime
    -- , timestamp
    -- , every
    -- , fps
    , millisecond
    , second
    , minute
    , hour
    -- , lagSig
    ) where

-----------------------------------------------------------------
-- Time
-----------------------------------------------------------------

type Time = Rational

millisecond    :: Time
second         :: Time
minute         :: Time
hour           :: Time

millisecond      = 0.001
second           = 1
minute           = 60
hour             = 3600

-- deltaTime :: Signal Time
-- deltaTime = Signal $ \_ -> do
--     ref  <- newIORef 0
--     return (cont ref, 0)
--     where
--         cont ref (TimeEvent dt _) = writeIORef ref dt >> return   (Change dt)
--         cont ref _                = readIORef ref     >>= return . NoChange


-- runTime :: Signal Time
-- runTime = Signal $ \_ -> do
--     ref  <- newIORef 0
--     return (cont ref, 0)
--     where
--         cont ref (TimeEvent _ rt) = writeIORef ref rt >>  return  (Change rt)
--         cont ref _                = readIORef  ref    >>= return . NoChange

-- tick :: Signal (Time, Time)
-- tick = Signal $ \_ -> do
--     ref  <- newIORef (0, 0)
--     return (cont ref, (0, 0))
--     where
--         cont ref (TimeEvent dt rt) = writeIORef ref (dt, rt) >>  return (Change (dt, rt))
--         cont ref _                 = readIORef  ref          >>= return . NoChange

-- timestamp :: Signal a -> Signal (Time, a)
-- timestamp sig = Signal $ \state -> do
--     let timeRef = runTimeRef state
--     (scont, s) <- unSignal sig state
--     ref        <- newIORef (0, s)
--     return (cont timeRef ref scont, (0, s))
--     where
--         cont timeRef ref scont event = scont event >>= \se -> case se of
--             NoChange _ -> readIORef ref     >>= return . NoChange
--             Change   s -> readIORef timeRef >>= \t -> writeIORef ref (t, s) >> return (Change (t, s))

-- every :: Time -> Signal Time
-- every time = Signal $ \_ -> do
--     ref      <- newIORef 0
--     accref   <- newIORef 0
--     return (cont accref ref, 0)
--     where
--         cont accref ref (TimeEvent dt rt) = do
--             acc      <- readIORef accref
--             let acc'  = acc + dt
--             if acc'  >= time
--                 then writeIORef accref (acc' - time) >> return (Change rt)
--                 else writeIORef accref acc' >> (NoChange <~ readIORef ref)
--         cont _ ref _ = NoChange <~ readIORef ref

-- fps :: Time -> Signal Time
-- fps rtime = Signal $ \_ -> do
--     ref      <- newIORef 0
--     accref   <- newIORef 0
--     return (cont accref ref, 0)
--     where
--         time = 1 / rtime
--         cont accref ref (TimeEvent dt _) = do
--             acc     <- readIORef accref
--             let acc' = acc + dt
--             if acc' >= time
--                 then writeIORef accref (acc' - time) >> return (Change acc')
--                 else writeIORef accref acc'          >> (NoChange <~ readIORef ref)
--         cont _ ref _ = NoChange <~ readIORef ref

-- lagSig :: (Real a, Fractional a) => Double -> Signal a -> Signal a
-- lagSig lagTime sig = Signal $ \state -> do
--     (scont, s) <- unSignal sig state
--     ref        <- newIORef (realToFrac s, realToFrac s, 1)
--     return (cont scont ref, s)
--     where
--         cont scont ref event = do
--             s <- scont event
--             case s of
--                 Change v -> readIORef ref >>= \(start, _, _) -> writeIORef ref (start, realToFrac v, 0)
--                 NoChange _ -> return ()
--             case event of
--                 TimeEvent dt _ -> do
--                     (start, end, acc) <- readIORef ref
--                     let value'         = start * (1 - acc) + end * acc
--                     return $ Change $ realToFrac value'
--                 _ -> do
--                     (start, end, acc) <- readIORef ref
--                     if acc >= 1 then return (NoChange $ realToFrac end) else do
--                         dt        <- readIORef dtref
--                         let acc'   = min (acc + dt * lagTime) 1
--                         let value' = start * (1 - acc) + end * acc
--                         writeIORef ref (start, end, acc')
--                         return $ Change $ realToFrac value'
