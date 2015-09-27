{-# LANGUAGE CPP #-}
module Necronomicon.Interactive where

import GHC
import GHC.Paths ( libdir )
import DynFlags
import Unsafe.Coerce
import Necronomicon.FRP.Signal'

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

