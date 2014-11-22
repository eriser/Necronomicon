module Main where

import Necronomicon.FRP

main :: IO()
-- main = runSignal tonsOfMouseAndTime
-- main = runSignal $ dropIf (\(x,y) -> x > 400) (0,0) mousePos
-- main = runSignal $ keepIf (\(x,y) -> x > 400) (0,0) mousePos
-- main = runSignal $ sampleOn mouseClicks mousePos
-- main = runSignal $ keepWhen ((\(x,_) -> x > 400) <~ mousePos) mousePos
-- main = runSignal $ every $ 2 * second
-- main = runSignal $ dropWhen ((\(x,_) -> x > 400) <~ mousePos) mousePos
-- main = runSignal $ sampleOn mouseClicks doubleMouse <|> mousePos
main = runSignal $ every second <|> fps 4.5

doubleMouse :: Signal (Double,Double)
doubleMouse = (\(x,y) -> (x*2,y*y)) <~ mousePos

tonsOfMouseAndTime :: Signal (Double,Double)
tonsOfMouseAndTime = hundredTests
    where
        tupleTest x _ = x
        tupleTime               = (\x ->  (x,x)) <~ fps 30
        test                    = tupleTest <~ mousePos
        tenTests                = test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~(test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ tupleTime))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        test2                   = tupleTest <~ tenTests
        hundredTests            = test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ mousePos)))))))))


