module Necronomicon.Graphics.Shader (
    Shader(..),
    LoadedShader,
    -- (Shader,vertexShader,fragmentShader),
    VertexShader(..),
    FragmentShader(..),
    loadShader,
    vert,
    frag,
    -- Shader'(Shader'),
    compileVert,
    compileFrag,
    shader
    ) where

import Prelude
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Graphics.Rendering.OpenGL as GL
import System.IO (hPutStrLn, stderr)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

data VertexShader   = VertexShader   {
    vertexString   :: String,
    unVertexShader :: IO GL.Shader
    } deriving (Show)

data FragmentShader = FragmentShader {
    fragmentString   :: String,
    unFragmentShader :: IO GL.Shader
    } deriving (Show)

type LoadedShader = (GL.Program,[GL.UniformLocation])

data Shader = Shader {
    key      :: String,
    unShader :: IO LoadedShader
    }

instance Show (IO GL.Shader) where
    show _ = "IO GL.Shader"

instance Show Shader where
    show _ = "Shader"

loadShader :: GL.ShaderType -> FilePath -> IO GL.Shader
loadShader shaderType filePath = BS.readFile filePath >>= loadShaderBS filePath shaderType

loadShaderBS :: FilePath -> GL.ShaderType -> BS.ByteString -> IO GL.Shader
loadShaderBS filePath shaderType src = do
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader GL.$= src
    GL.compileShader shader
    printError
    ok      <- GL.get (GL.compileStatus shader)
    infoLog <- GL.get (GL.shaderInfoLog shader)
    unless (null infoLog)
        (mapM_ putStrLn ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])
    unless ok $ do
        GL.deleteObjectName shader
        ioError (userError "shader compilation failed")
    return shader

printError :: IO ()
printError = GL.get GL.errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

vert :: QuasiQuoter
vert = QuasiQuoter{quoteExp = shaderQuoter "compileVert"}

frag :: QuasiQuoter
frag = QuasiQuoter{quoteExp = shaderQuoter "compileFrag"}

shaderQuoter :: String -> String -> Q Exp
shaderQuoter compileString string = do
    name <- getValueName compileString
    return $ AppE (VarE name) (LitE $ StringL string)

compileVert :: String -> VertexShader
compileVert s = VertexShader s   . loadShaderBS "quasi-vert" GL.VertexShader   $ C.pack s

compileFrag :: String -> FragmentShader
compileFrag s = FragmentShader s . loadShaderBS "quasi-frag" GL.FragmentShader $ C.pack s

getValueName :: String -> Q Name
getValueName s = do
    name <- lookupValueName s
    return $ case name of
        Just n  -> n
        Nothing -> mkName s

shader :: VertexShader -> FragmentShader -> Shader
shader vs fs = Shader (vertexString vs ++ fragmentString fs) $ do

    print "compiling shader"

    program <- GL.createProgram
    vs'     <- unVertexShader   vs
    fs'     <- unFragmentShader fs
    GL.attachShader    program  vs'
    GL.attachShader    program  fs'
    GL.validateProgram program
    GL.linkProgram     program

    mv1 <- GL.get $ GL.uniformLocation program "modelViewMatrix1"
    mv2 <- GL.get $ GL.uniformLocation program "modelViewMatrix2"
    mv3 <- GL.get $ GL.uniformLocation program "modelViewMatrix3"
    mv4 <- GL.get $ GL.uniformLocation program "modelViewMatrix4"

    pr1 <- GL.get $ GL.uniformLocation program "projMatrix1"
    pr2 <- GL.get $ GL.uniformLocation program "projMatrix2"
    pr3 <- GL.get $ GL.uniformLocation program "projMatrix3"
    pr4 <- GL.get $ GL.uniformLocation program "projMatrix4"

    return (program,[mv1,mv2,mv3,mv4,pr1,pr2,pr3,pr3])

