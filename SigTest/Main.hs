import Prelude
import Necronomicon.FRP

main :: IO()
main = runSignal tonsOfMouseAndTime
-- main = runSignal $ dropIf (\(x,y) -> x > 400) (0,0) mousePos
-- main = runSignal $ keepIf (\(x,y) -> x > 400) (0,0) mousePos
-- main = runSignal $ sampleOn mouseClicks mousePos
-- main = runSignal $ keepWhen ((\(x,_) -> x > 400) <~ mousePos) mousePos
-- main = runSignal $ every $ 2 * second
-- main = runSignal $ dropWhen ((\(x,_) -> x > 400) <~ mousePos) mousePos
-- main = runSignal $ sampleOn mouseClicks doubleMouse <|> mousePos
-- main = runSignal $ every second <|> fps 4.5

doubleMouse :: Signal (Double,Double)
doubleMouse = (\(x,y) -> (x*2,y*y)) <~ mousePos

tonsOfMouseAndTime :: Signal (Double,Double)
tonsOfMouseAndTime = thousandsTests
    where
        tupleTest x _ = x
        tupleTime               = (\x ->  (x,x)) <~ every second
        test                    = tupleTest <~ mousePos
        tenTests                = test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ mousePos)))))))))
        test2                   = tupleTest <~ tenTests
        hundredTests            = test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ mousePos)))))))))
        test3                   = tupleTest <~ hundredTests
        thousandsTests          = test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ mousePos)))))))))
        test4                   = tupleTest <~ thousandsTests
        tenThousandTest         = test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ mousePos)))
