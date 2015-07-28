module Necronomicon.FRP.Signal  where

--TODO:
--Look into revamping Texture data structure and texture loading
--Revive Fonts and text
--Revive GUI
--Revive post-rendering FX
--Revamp and Revive networking (Replace NetMessage type class usage with simple Binary type class)

--If there's extra time TODO:
--Replace and remove dependencies: mtl, Haskel OpengGL, perhaps Haskell OpenGLRaw
--Replace and remove extraenous extensions

--Looking Forward TODO:
--Revamp and finish collision detection
--Look into a deferred / Physically based rendering system
--REPL and hot swapping

------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.IORef
import           Control.Monad
import qualified Data.IntSet                       as IntSet

import           Necronomicon.FRP.Types
------------------------------------------------------

----------------------------------
-- Event
----------------------------------

data Event a = Change a
             | NoChange a
             deriving (Show)

unEvent :: Event a -> a
unEvent (Change   a) = a
unEvent (NoChange a) = a

instance Functor Event where
    fmap f (Change   a) = Change   $ f a
    fmap f (NoChange a) = NoChange $ f a


----------------------------------
-- Signal
----------------------------------

data Signal a = Signal { unSignal :: SignalState -> IO (Int -> IO (Event a), a, IntSet.IntSet) }
              | Pure a

instance Functor Signal where
    fmap f (Pure x)      = Pure $ f x
    fmap f (Signal xsig) = Signal $ \state -> do
        (xcont, x, uids) <- xsig state
        let fx            = f x
        ref              <- newIORef fx
        return (cont xcont ref, fx, uids)
        where
            cont xcont ref eid = xcont eid >>= \xe -> case xe of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   x -> do
                    let fx = f x
                    writeIORef ref fx
                    return $ Change fx

instance Applicative Signal where
    pure                x = Pure x

    Pure   f <*> Pure   x = Pure $ f x
    Signal f <*> Pure   x = fmap ($ x) $ Signal f
    Pure   f <*> Signal x = fmap f     $ Signal x
    Signal f <*> Signal x = Signal $ \state -> do
        (fcont, f', fids) <- f state
        (xcont, x', xids) <- x state
        let fx             = f' x'
            uids           = IntSet.union fids xids
        ref               <- newIORef fx
        return (cont fcont xcont uids ref, fx, uids)
        where
            cont fcont xcont uids ref eid
                | not $ IntSet.member eid uids = readIORef ref >>= return . NoChange
                | otherwise                    = fcont eid >>= \fe -> xcont eid >>= \xe -> case (fe, xe) of
                    (NoChange _, NoChange _) -> readIORef ref >>= return . NoChange
                    _                        -> do
                        let fx = unEvent fe $ unEvent xe
                        writeIORef ref fx
                        return $ Change fx

    Pure   _ *> Pure   g = Pure g
    Pure   _ *> Signal g = Signal g
    Signal f *> Pure   g = Signal $ \state -> do
        (fcont, _, fids) <- f state
        fchan             <- atomically $ newTBQueue 10
        _                 <- forkIO $ contf fcont fids fchan
        return (contg fchan, g, fids)
        where
            contf fcont fids fchan = forever $ atomically (readTBQueue fchan) >>= \eid -> when (IntSet.member eid fids) (fcont eid >> return ())
            contg fchan eid        = atomically (writeTBQueue fchan eid `orElse` return ()) >> return (NoChange g)
    Signal f *> Signal g = Signal $ \state -> do
        (fcont,  _, fids) <- f state
        (gcont, g', gids) <- g state
        ref               <- newIORef (NoChange g')
        fchan             <- atomically $ newTBQueue 10
        _                 <- forkIO $ contf fcont fids fchan
        return (contg gcont gids fchan ref, g', IntSet.union fids gids)
        where
            contf fcont fids fchan         = forever $ atomically (readTBQueue fchan) >>= \eid -> when (IntSet.member eid fids) (fcont eid >> return ())
            contg gcont gids fchan ref eid = do
                atomically $ writeTBQueue fchan eid `orElse` return ()
                if IntSet.member eid gids
                    then gcont eid >>= \ge -> writeIORef ref (NoChange $ unEvent ge) >> return ge
                    else readIORef ref

    (<*) = flip (*>)

-- instance Alternative Signal where
--     empty                 = Signal $ \_ -> return (const $ error "A Signal cannot be empty.", error "A Signal cannot be empty.", IntSet.empty)
--     Pure   x <|> Pure _   = Pure x
--     Pure   _ <|> s        = s
--     Signal s <|> Pure _   = Signal s
--     Signal x <|> Signal y = Signal $ \state -> do
--         (xcont, x', xids) <- x state
--         (ycont,  _, yids) <- y state
--         let uids           = IntSet.union xids yids
--         ref               <- newIORef x'
--         return (cont xcont ycont uids ref, x', uids)
--         where
--             cont xcont ycont uids ref eid
--                 | not $ IntSet.member eid uids = readIORef ref >>= return . NoChange
--                 | otherwise                    = xcont eid >>= \xe -> case xe of
--                     Change x' -> writeIORef ref x' >> return xe
--                     _         -> ycont eid >>= \ye -> case ye of
--                         Change y' -> writeIORef ref y' >>  return ye
--                         _         -> readIORef  ref    >>= return . NoChange

instance Num a => Num (Signal a) where
    (+)         = \x y -> (+) <$> x <*> y
    (*)         = \x y -> (*) <$> x <*> y
    (-)         = \x y -> (-) <$> x <*> y
    negate      = fmap negate
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (Signal a) where
    (/) = \x y -> (/) <$> x <*> y
    fromRational = pure . fromRational

instance Floating a => Floating (Signal a) where
    pi      = pure pi
    (**)    = \x y -> (**) <$> x <*> y
    exp     = fmap exp
    log     = fmap log
    sin     = fmap sin
    cos     = fmap cos
    asin    = fmap asin
    acos    = fmap acos
    atan    = fmap atan
    logBase = \x y -> logBase <$> x <*> y
    sqrt    = fmap sqrt
    tan     = fmap tan
    tanh    = fmap tanh
    sinh    = fmap sinh
    cosh    = fmap cosh
    asinh   = fmap asinh
    atanh   = fmap atanh
    acosh   = fmap acosh

-- instance Monoid a => Monoid (Signal a) where
--     mconcat ss = foldr (<>) (pure mempty) ss
--     mempty     = pure mempty
--     mappend    = \x y -> mappend <$> x <*> y

instance Monoid (Signal a) where
    mempty                        = Signal $ \_ -> return (const $ error "A Signal cannot be empty.", error "A Signal cannot be empty.", IntSet.empty)
    mappend (Pure   x) (Pure _  ) = Pure x
    mappend (Pure   _) (s       ) = s
    mappend (Signal s) (Pure _  ) = Signal s
    mappend (Signal x) (Signal y) = Signal $ \state -> do
        (xcont, x', xids) <- x state
        (ycont,  _, yids) <- y state
        let uids           = IntSet.union xids yids
        ref               <- newIORef x'
        return (cont xcont ycont uids ref, x', uids)
        where
            cont xcont ycont uids ref eid
                | not $ IntSet.member eid uids = readIORef ref >>= return . NoChange
                | otherwise                    = xcont eid >>= \xe -> case xe of
                    Change x' -> writeIORef ref x' >> return xe
                    _         -> ycont eid >>= \ye -> case ye of
                        Change y' -> writeIORef ref y' >>  return ye
                        _         -> readIORef  ref    >>= return . NoChange

    mconcat ss = Signal $ \state -> do
        (sconts, svals, sids) <- unzip3 <~ mapM (flip unSignal state) ss
        let uids               = foldr IntSet.union IntSet.empty sids  
        ref                   <- newIORef $ head svals
        return (cont sconts ref uids, head svals, uids)
        where
            cont sconts ref uids eid
                | not $ IntSet.member eid uids = readIORef ref >>= return . NoChange 
                | otherwise                    = foldM (changed eid) Nothing sconts >>= \mcs -> case mcs of
                    Nothing -> readIORef ref >>= return . NoChange
                    Just e  -> return $ Change e

            changed eid Nothing c = c eid >>= \mc -> case mc of
                NoChange _ -> return Nothing
                Change   e -> return $ Just e
            changed _ e _ = return e

