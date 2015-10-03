{-# LANGUAGE CPP #-}
module Necronomicon.Interactive where

import Control.Concurrent (threadDelay)
import GHC
import GHC.Paths ( libdir )
import Necronomicon.FRP
import System.Directory
import Data.Dynamic
import Exception

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
    p          <- parseModule modSum
    t          <- typecheckModule p
    d          <- desugarModule t
    _          <- loadModule d
    _          <- getNamesInScope
    _          <- return $ coreModule d
    g          <- getModuleGraph
    mapM_ showModule g
    return modSum

compileSignal :: String -> String -> String -> IO (Maybe (Signal ()))
compileSignal targetFile modName expr = flip catch failure $ runGhc (Just libdir) $ do
    _ <- loadNecronomicon targetFile modName
#if __GLASGOW_HASKELL__ < 704
    setContext [] [(simpleImportDecl . mkModuleName $ modName) {ideclQualified = True}]
#else
    setContext [IIDecl $ (simpleImportDecl . mkModuleName $ modName) {ideclQualified = True}]
#endif
    dynSig <- dynCompileExpr (modName ++ "." ++ expr)
    return $ fromDynamic dynSig
    where
        failure :: SomeException -> IO (Maybe (Signal ()))
        failure _ = return Nothing

runSignalWithFile :: FilePath -> String -> String -> IO ()
runSignalWithFile filePath modName expr = startSignalRuntime >>= maybeRunSig (return ()) (return ())
    where
        maybeRunSig archiver finalizer state = do
            time     <- getModificationTime filePath
            maybeSig <- compileSignal filePath modName expr
            case maybeSig of
                Nothing  -> putStrLn "necro error: necroMain type did not match expeced type: Signal ()" >> modificationTimeDaemon time archiver finalizer state
                Just sig -> do
                    hotSwapState archiver finalizer state
                    (_, archiver', finalizer') <- runSignalFromState sig state
                    modificationTimeDaemon time archiver' finalizer' state

        modificationTimeDaemon prevTime archiver finalizer state = do
            threadDelay 200000
            time <- getModificationTime filePath
            if time <= prevTime
                then modificationTimeDaemon time archiver finalizer state
                else maybeRunSig archiver finalizer state
