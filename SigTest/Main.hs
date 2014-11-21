module Main where

import Necronomicon.FRP

main :: IO()
-- main = runSignal oneThousandMousePositions'
-- main = runSignal $ dropIf (\(x,y) -> x > 400) (0,0) mousePos
-- main = runSignal $ keepIf (\(x,y) -> x > 400) (0,0) mousePos
-- main = runSignal $ sampleOn mouseClicks mousePos
-- main = runSignal $ keepWhen ((\(x,_) -> x > 400) <~ mousePos) mousePos
-- main = runSignal $ every $ 2 * second
-- main = runSignal $ dropWhen ((\(x,_) -> x > 400) <~ mousePos) mousePos
main = runSignal $ sampleOn mouseClicks doubleMouse <|> mousePos

doubleMouse :: Signal (Double,Double)
doubleMouse = (\(x,y) -> (x*2,y*y)) <~ mousePos

oneThousandMousePositions :: Signal (Double,Double)
oneThousandMousePositions = thousandTest
    where
        tupleTest (x,y) (xx,yy) = (x-xx,y-yy)
        tupleTime               = (\x -> (x,x)) <~ every second
        test                    = tupleTest <~ mousePos
        tenTests                = test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ mousePos))))))))
        test2                   = tupleTest <~ tenTests
        hundredTests            = test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ mousePos)))))))))
        test3                   = tupleTest <~ hundredTests
        thousandTest            = test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ mousePos)))))))))
        test4                   = tupleTest <~ thousandTest
        tenThousandTest         = test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ mousePos)))

oneThousandMousePositions' :: Signal (Double,Double)
oneThousandMousePositions' = hundredTests
    where
        tupleTest (x,y) (xx,yy) = (x-xx,y-yy)
        tupleTime               = (\x ->  (x,x)) <~ every second
        test                    = tupleTest <~ mousePos
        tenTests                = test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~(test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ tupleTime))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        test2                   = tupleTest <~ tenTests
        hundredTests            = test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ mousePos)))))))))


