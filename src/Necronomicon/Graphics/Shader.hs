module Necronomicon.Graphics.Shader (
    Shader(..),
    LoadedShader,
    VertexShader(..),
    FragmentShader(..),
    vert,
    frag,
    compileVert,
    compileFrag,
    shader,
    offset0,
    offsetPtr,
    loadVertexShader,
    loadFragmentShader
    ) where

import           Control.Monad              (unless)
import           Data.Maybe                 (fromMaybe)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Necronomicon.Utility
import           Prelude
import           System.IO                  (hPutStrLn, stderr)
import           Foreign.Ptr                (Ptr, wordPtrToPtr)
import           Paths_Necronomicon
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Graphics.Rendering.OpenGL  as GL

newtype VertexShader   = VertexShader   {unVertexShader        :: IO GL.Shader}
newtype FragmentShader = FragmentShader {unFragmentShader      :: IO GL.Shader}
data    Shader         = Shader         {key :: Int,loadShader :: IO LoadedShader}
type    LoadedShader   = (GL.Program,[GL.UniformLocation],[GL.AttribLocation])

instance Show Shader where
    show (Shader k _) = "Shader " ++ show k

loadShaderBS :: FilePath -> GL.ShaderType -> BS.ByteString -> IO GL.Shader
loadShaderBS filePath shaderType src = do
    newShader <- GL.createShader shaderType
    GL.shaderSourceBS newShader GL.$= src
    GL.compileShader  newShader
    printError
    ok      <- GL.get (GL.compileStatus newShader)
    infoLog <- GL.get (GL.shaderInfoLog newShader)
    unless (null infoLog) $ do
        putStrLn $ "Shader info log for " ++ filePath ++ ":"
        putStrLn infoLog
    unless ok $ do
        GL.deleteObjectName newShader
        ioError (userError "Failed to compile shader.")
    return newShader

loadVertexShader :: FilePath -> VertexShader
loadVertexShader path = VertexShader $ do
    putStrLn $ "loadVertexShader: " ++ path
    resources <- getDataFileName ""
    shaderPath <- BS.readFile $ resources ++ "shaders/" ++ path
    loadShaderBS path GL.VertexShader shaderPath

loadFragmentShader :: FilePath -> FragmentShader
loadFragmentShader path = FragmentShader $ do
    putStrLn $ "loadFragmentShader: " ++ path
    resources <- getDataFileName ""
    shaderPath <- BS.readFile $ resources ++ "shaders/" ++ path
    loadShaderBS path GL.FragmentShader shaderPath

printError :: IO ()
printError = GL.get GL.errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

compileVert :: String -> VertexShader
compileVert = VertexShader   . loadShaderBS "vert" GL.VertexShader   . C.pack

compileFrag :: String -> FragmentShader
compileFrag = FragmentShader . loadShaderBS "frag" GL.FragmentShader . C.pack

shader :: String -> [String] -> [String] -> VertexShader -> FragmentShader -> Shader
shader shaderName uniformNames attributeNames vs fs = Shader (hash shaderName) $ do
    putStrLn $ "Compiling shader: " ++ shaderName

    program <- GL.createProgram
    vs'     <- unVertexShader   vs
    fs'     <- unFragmentShader fs

    GL.attachShader    program  vs'
    GL.attachShader    program  fs'
    GL.validateProgram program
    GL.linkProgram     program

    uniforms   <- mapM (GL.get . GL.uniformLocation program) uniformNames
    attributes <- mapM (GL.get . GL.attribLocation  program) attributeNames

    return (program,uniforms,attributes)

-- |Produce a 'Ptr' value to be used as an offset of the given number
-- of bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0

vert :: QuasiQuoter
vert = QuasiQuoter (shaderQuoter "compileVert") (error "This quoter has not been defined") (error "This quoter has not been defined") (error "This quoter has not been defined")

frag :: QuasiQuoter
frag = QuasiQuoter (shaderQuoter "compileFrag") (error "This quoter has not been defined") (error "This quoter has not been defined") (error "This quoter has not been defined")

getValueName :: String -> Q Name
getValueName s = lookupValueName s >>= return . fromMaybe (mkName s)

shaderQuoter :: String -> String -> Q Exp
shaderQuoter compileString string = do
    name <- getValueName compileString
    return $ AppE (VarE name) (LitE $ StringL string)
