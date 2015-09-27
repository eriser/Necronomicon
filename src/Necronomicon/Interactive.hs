{-# LANGUAGE CPP #-}
module Necronomicon.Interactive where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import GHC
import GHC.Paths ( libdir )
import DynFlags
import Unsafe.Coerce
import Necronomicon.FRP.Signal'
import System.Directory

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
compileSignal :: String -> String -> String -> IO (Signal Fr Double)
compileSignal targetFile modName expr = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    _ <- loadNecronomicon targetFile modName
#if __GLASGOW_HASKELL__ < 704
    setContext [] [(simpleImportDecl . mkModuleName $ modName) {ideclQualified = True}]
#else
    setContext [IIDecl $ (simpleImportDecl . mkModuleName $ modName) {ideclQualified = True}]
#endif
    maybeSig <- compileExpr (modName ++ "." ++ expr)
    return $ unsafeCoerce maybeSig

runSignalWithFile :: FilePath -> IO ()
runSignalWithFile filePath = getModificationTime filePath >>= go
    where
        go prevTime = do
            time <- getModificationTime filePath
            when (time > prevTime) $ putStrLn $ "File was modified at time: " ++ show time
            threadDelay 500000
            go time

