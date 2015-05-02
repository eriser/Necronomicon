-- import Necronomicon
import qualified Necronomicon.Physics.DynamicTree as DynTree

main :: IO ()
main = print tree
    where
        tree = DynTree.insert (1, 0)
             $ DynTree.insert (2, 1)
             $ DynTree.insert (3, 2)
             $ DynTree.insert (4, 3)
             $ DynTree.insert (5, 4)
             $ DynTree.insert (6, 5)
             $ DynTree.insert (7, 6)
             $ DynTree.insert (8, 7)
             $ DynTree.empty

-- main = DynTree.test
-- main = do
    -- DynTree.validate $
        -- snd $ DynTree.createProxy (AABB 1 3) (0, 0) $
        -- snd $ DynTree.createProxy (AABB (Vector3 8174.270554757964   5480.272260632468  (-2770.6720045540396)) (Vector3   8815.900371232714    1900.2369724882155 (-5878.599587069464))) (0, 0) $
        -- snd $ DynTree.createProxy (AABB (Vector3 8174.270554757964   5480.272260632468  (-2770.6720045540396)) (Vector3   8815.900371232714    1900.2369724882155 (-5878.599587069464))) (0, 0) $
        -- snd $ DynTree.moveProxy 3 (AABB (Vector3 9559.702141916616 (-1702.999822075346)   918.7117379482152)   (Vector3 (-3506.448395467103) (-8021.22423763858)    2042.381766807217)) DynTree.empty
{-
    DynTree.validate t1
    DynTree.validate t2
    DynTree.validate t3
    DynTree.validate t4
    DynTree.validate t5
    DynTree.validate t6
    DynTree.validate t7
    DynTree.validate t8
    DynTree.validate t9
    DynTree.validate t10
    DynTree.validate t11
    DynTree.validate t12
    DynTree.validate t13
    DynTree.validate t14
    DynTree.validate t15
    DynTree.validate t16
    DynTree.validate t17
    DynTree.validate t18
    DynTree.validate t19
    DynTree.validate t20
    DynTree.validate t21
    DynTree.validate t22
    DynTree.validate t23
    DynTree.validate t24
    DynTree.validate t25
    DynTree.validate t26
    DynTree.validate t27
    DynTree.validate t28
    where
        t1       = DynTree.empty
        (i2, t2) = DynTree.createProxy (AABB 1 3) (0, 0) t1
        (i3, t3) = DynTree.createProxy (AABB 2 4) (0, 1) t2
        (i4, t4) = DynTree.createProxy (AABB 0 9) (0, 2) t3
        (i5, t5) = DynTree.createProxy (AABB 1 8) (0, 3) t4
        (i6, t6) = DynTree.createProxy (AABB 4 5) (0, 4) t5
        (i7, t7) = DynTree.createProxy (AABB 7 8) (0, 5) t6
        (i8, t8) = DynTree.createProxy (AABB 4 7) (0, 6) t7
        (i9, t9) = DynTree.createProxy (AABB 3 9) (0, 7) t8

        (_, t10) = DynTree.moveProxy i3 (AABB 11 20) t9
        (_, t11) = DynTree.moveProxy i6 (AABB 0 1) t10
        (_, t12) = DynTree.moveProxy i2 (AABB (-10) 2) t11
        (_, t13) = DynTree.moveProxy i9 (AABB 3 5) t12
        (_, t14) = DynTree.moveProxy i8 (AABB 9 99) t13
        (_, t15) = DynTree.moveProxy i4 (AABB 2 8) t14
        (_, t16) = DynTree.moveProxy i7 (AABB 11 12) t15
        (_, t17) = DynTree.moveProxy i5 (AABB 4 8) t16
        (_, t18) = DynTree.moveProxy i4 (AABB 3 9) t17
        (_, t19) = DynTree.moveProxy i9 (AABB 2 9) t18
        (_, t20) = DynTree.moveProxy i6 (AABB 5 7) t19

        t21      = DynTree.destroyProxy i2 t20
        t22      = DynTree.destroyProxy i6 t21
        t23      = DynTree.destroyProxy i7 t22
        t24      = DynTree.destroyProxy i3 t23
        t25      = DynTree.destroyProxy i4 t24
        t26      = DynTree.destroyProxy i8 t25
        t27      = DynTree.destroyProxy i9 t26
        t28      = DynTree.destroyProxy i5 t27
-}
