








































module X where
import AronModule
import AronGraphic
import AronDevLib
import AronAlias
import Graphics.Rendering.OpenGL

takeIndexBetweenInc::(Int, Int) -> [a] -> [a]
takeIndexBetweenInc (x0, x1) cx = take (x1 - x0 + 1) $ drop x0 cx

fun4::Int
fun4 = 3

list2ToVex::[GLfloat] -> Vertex3 GLfloat
list2ToVex [x, y] = Vertex3 x y 0.0

main = do
    let s = [1, 2, 3, 4]
    let s1 = partList 3 s
    s <- rfl "/tmp/a.x" 
    let s1 = map (\x -> read x :: GLfloat) s
    let s1' = unique s1
    let cs = map list2ToVex $ combin 2 s1'
    fw "unique"
    print $ len s1'
    fw "cs"
    pre cs
--    pre $ take 10 cs
--    let cc = partList 3 cs
--    fw "len cc"
--    print $ len cc
--    pre cc
--    let co = map (\[x, y] -> (isColinear x y 0, [x, y])) cc 
--    let fo = filter (\(a, b) -> a) co
--    fw "fo"
--    pre fo
--    fw "cs"
--    pre cs
--    fw "len fo"
--    print $ len fo
--    print $ len cs
--    pre $ take 10 cs
--    let s2 = partList 3 s1
--    let s3 = map list3ToVertex3 s2 
--    pre s3 
--    pre s
    print "ok"
