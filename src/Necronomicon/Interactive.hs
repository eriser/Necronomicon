{-# LANGUAGE CPP #-}
module Necronomicon.Interactive where

import GHC
import Outputable
import GHC.Paths ( libdir )
import DynFlags
import Control.Monad.Trans
import Exception (gtry)
import GHC.Exception (SomeException)
import Debugger (showTerm)
import Data.IORef
import ErrUtils

loadNecronomicon :: GhcMonad m => String -> String -> m ModSummary
loadNecronomicon targetFile modName = do
    dflags     <- getSessionDynFlags
    _          <- setSessionDynFlags $ dflags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
    -- let dflags' = foldl xopt_set dflags [Opt_ImplicitPrelude]
    -- _          <- setSessionDynFlags dflags'
    target     <- guessTarget targetFile Nothing
    setTargets [target]
    _          <- load LoadAllTargets
    modSum     <- getModSummary $ mkModuleName modName
    p <- parseModule modSum
    t <- typecheckModule p
    d <- desugarModule t
    _ <- loadModule d
    _ <- getNamesInScope
    _ <- return $ coreModule d
    -- l <- loadModule d
    -- n <- getNamesInScope
    -- c <- return $ coreModule d

    g <- getModuleGraph
    mapM_ showModule g
    return modSum
    -- return $ (parsedSource d,"/n-----/n",  typecheckedSource d)

runExpr :: String -> String -> String -> IO ()
runExpr targetFile modName expr = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    _ <- loadNecronomicon targetFile modName
#if __GLASGOW_HASKELL__ < 704
    GHC.setContext []
        [(GHC.simpleImportDecl . GHC.mkModuleName $ modName)
         {GHC.ideclQualified = True}
        ]
#else
    GHC.setContext
        [GHC.IIDecl $
         (GHC.simpleImportDecl . GHC.mkModuleName $ modName)
         {GHC.ideclQualified = True}
        ]
#endif
    rr <- runStmt expr RunToCompletion
    case rr of
        RunOk ns -> do
#if __GLASGOW_HASKELL__ < 706
            let q = (qualName &&& qualModule) defaultUserStyle
#else
            let q = QueryQualify (qualName defaultUserStyle) (qualModule defaultUserStyle) (qualPackage defaultUserStyle)
#endif
            mapM_ (\n -> do
                mty <- lookupName n
                case mty of
                    Just (AnId aid) -> do
                        df      <- getSessionDynFlags
                        t       <- gtry $ obtainTermFromId maxBound True aid
                        evalDoc <- case t of
                            Right term -> showTerm term
                            Left  exn  -> return (text "*** Exception:" <+> text (show (exn :: SomeException)))
                        liftIO $ putStrLn $ showSDocForUser df q evalDoc
                        return ()
                    _ -> return ()
                  ) ns
        RunException e -> liftIO $ putStrLn $ "RunException: " ++ show e
        _ -> return ()

-- LogAction == DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logHandler :: IORef String -> LogAction
logHandler ref dflags severity srcSpan style msg =
    case severity of
        SevError ->  modifyIORef' ref (++ printDoc)
        SevFatal ->  modifyIORef' ref (++ printDoc)
        _        ->  return () -- ignore the rest
    where
        cntx     = initSDocContext dflags style
        locMsg   = mkLocMessage severity srcSpan msg
        printDoc = show (runSDoc locMsg cntx)

