import Necronomicon

main :: IO ()
main = runGame game start

start :: GameObject
start = GameObject (Transform 0 identity 1) Nothing Nothing Nothing [cam, cube1, cube2, cube3, cube4, cube5]
    where
        c     = Just $ Camera (60/2) 0.1 1000 black []
        cam   = GameObject (Transform (Vector3   0   0 10) identity              1)  Nothing            Nothing c       []
        cube1 = GameObject (Transform (Vector3   5   0  0) (fromEuler' 32 81 62) 1) (boxCollider 2 1 1) Nothing Nothing []
        cube2 = GameObject (Transform (Vector3 (-5)  0  0) (fromEuler' 99 12 29) 1) (boxCollider 1 2 1) Nothing Nothing []
        cube3 = GameObject (Transform (Vector3 (-2)  2  0) (fromEuler' 99 12 29) 1) (boxCollider 1 2 1) Nothing Nothing []
        cube4 = GameObject (Transform (Vector3   2 (-2) 0) (fromEuler' 99 12 29) 1) (boxCollider 2 1 1) Nothing Nothing []
        cube5 = GameObject (Transform (Vector3   0   1  1) (fromEuler' 15  0 2)  1) (boxCollider 1 1 1) Nothing Nothing []

--matrix rotations and or quaternions unstable!
game :: GameObject -> GameObject
game g
    | [cam, cube1, cube2, cube3, cube4, cube5] <- children g = gchildren_ [cam, cube1' cube1, cube2, cube3, cube4, cube5] g
    | otherwise                         = g
    where
        cube1' (GameObject (Transform p r s) c _ _ _) = GameObject (Transform p (r  * (fromEuler' 0.1 0.121 0)) s) c Nothing Nothing []
