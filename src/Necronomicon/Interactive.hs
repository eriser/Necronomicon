{-# LANGUAGE CPP #-}
module Necronomicon.Interactive where

import Control.Concurrent (threadDelay)
import GHC
import GHC.Paths ( libdir )
import DynFlags
-- import Unsafe.Coerce
import Necronomicon.FRP.Signal'
import System.Directory
import Data.Dynamic

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

--We are declaring the module as an import and then compiling the expression with that context
--This should likely be Signal Fr () and we should have functions such as sigTrace
compileSignal :: String -> String -> String -> IO (Signal Fr ())
compileSignal targetFile modName expr = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    _ <- loadNecronomicon targetFile modName
#if __GLASGOW_HASKELL__ < 704
    setContext [] [(simpleImportDecl . mkModuleName $ modName) {ideclQualified = True}]
#else
    setContext [IIDecl $ (simpleImportDecl . mkModuleName $ modName) {ideclQualified = True}]
#endif
    -- maybeSig <- compileExpr (modName ++ "." ++ expr)
    dynSig <- dynCompileExpr (modName ++ "." ++ expr)
    return $ fromDyn dynSig (pure ())

--TODO: Need exception handling for when things go awry
runSignalWithFile :: FilePath -> String -> String -> IO ()
runSignalWithFile filePath modName expr = do
    sig                           <- compileSignal filePath modName expr
    state                         <- startSignalRuntime
    (sample, archiver, finalizer) <- runSignalFromState sig state
    t <- getModificationTime filePath
    modificationTimeDaemon t sample archiver finalizer state
    where
        modificationTimeDaemon prevTime sample archiver finalizer state = do
            -- sample >>= print
            time <- getModificationTime filePath
            if (time <= prevTime)
                then threadDelay 200000 >> modificationTimeDaemon time sample archiver finalizer state
                else do
                    hotSwapState archiver finalizer state
                    sig                              <- compileSignal filePath modName expr
                    (sample', archiver', finalizer') <- runSignalFromState sig state
                    threadDelay 200000
                    modificationTimeDaemon time sample' archiver' finalizer' state

