
module Necronomicon.FRP.GUI (Gui(..),
                             Size(..),
                            --  button,
                             input,
                             element,
                             gui,
                             label,
                            --  slider,
                             chat,
                             netStat,
                             userBox)where

import           Data.IORef
import           Necronomicon.FRP.Signal
import           Necronomicon.Graphics
import           Necronomicon.Linear
import           Necronomicon.Networking (sendChatMessage)

data Gui a = Gui a SceneObject
data Size = Size Double Double

input :: Signal (Gui a) -> Signal a
input = lift $ \(Gui a _) -> a

element :: Signal (Gui a) -> Signal SceneObject
element = lift $ \(Gui _ s) -> s

gui :: [Signal SceneObject] -> Signal ()
gui gs = renderGUI $ root <~ combine gs

{- TO DO: Remove this?
guiEvent :: (Typeable a) => IORef (Gui b) -> Dynamic -> (a -> IO (EventValue (Gui b))) -> IO (EventValue (Gui b))
guiEvent ref v f = case fromDynamic v of
    Nothing -> print "button Type error" >> readIORef ref >>= return . NoChange
    Just v' -> f v'
-}

label :: Vector2 -> Font -> Color -> String -> SceneObject
label (Vector2 x y) font _ text = SceneObject (Vector3 x y 0) identity 1 (drawText text font ambient) []

userBox :: Vector2 -> Size -> Font -> Material -> Signal SceneObject
userBox (Vector2 x y) _ font _ = textBox <~ users
    where
        textBox    us = SceneObject (Vector3  x y 0)  identity 1 (drawText (userString us) font ambient) []
        userString us = "[ " ++ foldr (\u us'-> u ++ " " ++ us') [] us ++ "]"

--need widgets for network status and current users
netStat :: Vector2 -> Size -> Font -> Signal SceneObject
netStat (Vector2 x y) (Size w h) font = indicator <~ networkRunStatus
    where
        indicator Running      = emptyObject
        indicator Disconnected = background (RGBA 1 0.00 0 0.5) "Disconnected"
        indicator nstatus      = background (RGBA 0.75 0.5 0 0.5) $ show nstatus
        background         c t = SceneObject (Vector3  x y 0) identity 1 (Model (rect w h) (vertexColored c)) [textObject t]
        textObject           t = SceneObject (Vector3  0 0 1) identity 1 (drawText t font ambient) []

chat :: Vector2 -> Size -> Font -> Material -> Signal SceneObject
chat (Vector2 x y) (Size w h) font material = addChild <~ textEditSignal textInput (toggle $ lift2 (&&) ctrl $ isDown keyT) ~~ chatDisplay (Vector2 x y) (Size w h) font material
    where
        textEditSignal textInputSignal toggleSignal = Signal $ \state -> do
            inputCont  <- unSignal textInputSignal state
            toggleCont <- unSignal toggleSignal    state
            textRef    <- newIORef ""
            activeRef  <- newIORef False
            metrics    <- charMetrics font
            return $ processSignal textRef activeRef inputCont toggleCont metrics (necroNetClient state)

        processSignal textRef activeRef inputCont toggleCont metrics client update = toggleCont update >>= go
            where go (Change   isActive) = writeIORef activeRef isActive >> if isActive then readIORef textRef >>= return . Change . background else return $ Change emptyObject
                  go (NoChange isActive) = if not isActive then return $ NoChange emptyObject else do
                      t <- readIORef textRef
                      c <- inputCont update
                      case (c,t) of
                          (NoChange  _,_)      -> return . NoChange $ background t
                          (Change '\n',_)     -> sendChatMessage t client >> returnNewText textRef metrics ""
                          (Change '\b',(_:_)) -> returnNewText textRef metrics $ init t
                          (Change char,_)     -> if char == toEnum 0
                              then return . NoChange $ background t
                              else returnNewText textRef metrics $ t ++ [char]

        returnNewText r cm t = writeIORef r t >> (return . Change . background $ fitTextIntoBounds False t (w,0.055) cm)
        background         t = SceneObject (Vector3  0 h 0) identity 1 (Model (rect w 0.055) material) [textObject t]
        textObject         t = SceneObject (Vector3  0 0 1) identity 1 (drawText t font ambient) []

chatDisplay :: Vector2 -> Size -> Font -> Material -> Signal SceneObject
chatDisplay (Vector2 x y) (Size w h) font _ = Signal $ \state -> do
    chatCont <- unSignal receiveChatMessage state
    metrics  <- charMetrics font
    ref      <- newIORef ""
    return $ processSignal ref metrics chatCont
    where
        --TODO: delete if too many lines
        processSignal ref metrics chatCont update = chatCont update >>= \c -> case c of
            NoChange _ -> readIORef ref >>= return . NoChange . chatObject
            Change str -> do
                prevStr <- readIORef ref
                let val = (fitTextIntoBounds False (prevStr ++ str ++ "\n\n") (w * 1.0,h * 0.75) metrics)
                writeIORef ref val
                return $ Change $ chatObject val

        chatObject t = SceneObject (Vector3  x y 0) identity 1 (Model (rect w h) (vertexColored (RGBA 0 0 0 0))) [textObject t]
        textObject t = SceneObject (Vector3  0 0 1) identity 1 (drawText t font ambient) []

{-
slider :: Vector2 -> Size -> Color -> Signal (Gui Double)
slider (Vector2 x y) (Size w h) color = Signal $ \necro -> do
    ref <- newIORef $ Gui 0.0 (s 0.5)
    mpr <- newIORef False
    return (Gui 0.0 (s 0.5),processEvent ref mpr,IntSet.fromList [0,1])
    where
        processEvent ref mpr (Event uid val)
            | uid == 1 = guiEvent ref val $ \mp -> writeIORef mpr mp >> readIORef ref >>= return . NoChange
            | uid == 0 = guiEvent ref val $ \(x,y)  -> do
                isMouseDown <- readIORef mpr
                case isMouseDown     &&
                     x >= 0.5 - hw   &&
                     x <= 0.5 + hw   &&
                     y >= 0.5 - hh h &&
                     y <= 0.5 + hh h of
                    False -> readIORef ref >>= return . NoChange
                    True  -> do
                        let v = linlin (0.5 - hh h) (0.5 + hh h) 1 0 y
                        let g = Gui v (s v)
                        writeIORef ref g
                        return $ Change g
            | otherwise = readIORef ref >>= return . NoChange

        s  h = SceneObject (Vector3 x y 0)    identity 1 sm [v h]
        v  h = SceneObject (Vector3 0 0 0.01) identity 1 (vm h) []
        p0 v = Vector3 (0 - hw) (hh h - v * h) 0
        p1 v = Vector3 (0 - hw) (0 + hh h) 0
        p2 v = Vector3 (0 + hw) (0 + hh h) 0
        p3 v = Vector3 (0 + hw) (hh h - v * h) 0
        hw   = w * 0.5
        hh h = h * 0.5
        fc   = RGB 0.5 0.5 0.5
        -- sm   = SimpleMesh [p0 1,p1 1,p2 1,p3 1,p0 1,p2 1] [color,color,color,color,color,color]
        -- vm h = SimpleMesh [p0 h,p1 h,p2 h,p3 h,p0 h,p2 h] [color*fc,color*fc,color*fc,color*fc,color*fc,color*fc]
        sm = undefined
        vm h = undefined

button :: Vector2 -> Size -> Color -> Signal (Gui Bool)
button (Vector2 x y) (Size w h) color = Signal $ \necro -> do
    ref <- newIORef False
    mpr <- newIORef (0,0)
    return (Gui False sf,processEvent ref mpr,IntSet.fromList [0,1])
    where
        processEvent ref mpr (Event uid val)
            | uid == 0 = case fromDynamic val of
                Nothing -> print "button Type error" >> (return $ NoChange (Gui False sf))
                Just m  -> do
                    writeIORef mpr m
                    v <- readIORef ref
                    case v of
                        True  -> return $ NoChange (Gui v st)
                        False -> return $ NoChange (Gui v sf)
            | uid == 1 = case fromDynamic val of
                Nothing -> print "button Type error" >> (return $ NoChange (Gui False sf))
                Just v  -> case v of
                    False -> writeIORef ref False >> (return $ Change (Gui False sf))
                    True  -> do
                        (mx,my) <- readIORef mpr
                        case mx >= x - hw && mx <= x + hw && my >= y - hh && my <= y + hh of
                           True  -> writeIORef ref True >> (return $ Change (Gui True st))
                           False -> return $ NoChange (Gui False sf)
            | otherwise = do
                v <- readIORef ref
                case v of
                    True  -> return $ NoChange (Gui v st)
                    False -> return $ NoChange (Gui v sf)

        st = SceneObject (Vector3 x y 0) identity 1 (m color) []
        sf = SceneObject (Vector3 x y 0) identity 1 (m fc   ) []
        fc = RGB 0.5 0.5 0.5
        hw = w * 0.5
        hh = h * 0.5
        m c= Model (rect w h) (vertexColored c)
        --c is color....
-}
