{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module AronCommon where
import Control.Monad
import Control.Concurrent
import Data.Char 
import Data.List
import Data.List.Split
import Data.Time
import Data.Ratio
import Data.Maybe(fromJust, isJust)
import Data.Time.Clock.POSIX
import Data.Foldable (foldrM)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import System.Random
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Text.Printf
import Data.Array
import Debug.Trace (trace)

import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as M
import qualified Text.Regex.TDFA     as TD
import qualified Text.Regex          as TR
import qualified Data.Set            as DS

import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString           as BS
import qualified Data.Text.Lazy            as TL
import qualified Data.Text                 as TS
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.Text.Encoding        as TSE
import qualified Data.Text.Lazy.Encoding   as TLE

-------------------------------------------------------------------------------- 
-- | How to build AronModule.hs 
-- | ghc --make AronModule.hs
-------------------------------------------------------------------------------- 

{-|
  === any dimension list
  * 2*[1, 2] = [2, 4]
  * [1, 2]+[3, 4] = [4, 6]
  * 2*[[1, 2]] = [[2, 4]]
-}
instance Num a => Num [a] where 
    (+) = zipWith (+) 
    (-) = zipWith (-) 
    (*) = zipWith (*)  
    abs = map abs 
    signum = map signum 
    fromInteger = repeat . fromInteger

------------------------------------------------------------------   
-- | Overloading the sin, cos to avoid Haskell crazy strict type 
-- | TODO: add test cases
class Cnum a where
    _sqrt,_sin,_cos::a -> Float
instance Cnum Int where
    _cos n = cos(realToFrac n)
    _sin n = sin(realToFrac n)
    _sqrt n = sqrt(realToFrac n)
instance Cnum Float where
    _cos n = cos n 
    _sin n = sin n 
    _sqrt n = sqrt n 
instance Cnum Double where
    _cos n = cos $ realToFrac n 
    _sin n = sin $ realToFrac n 
    _sqrt n = sqrt $ realToFrac n 
instance Cnum Integer where
    _cos n = cos (realToFrac n) 
    _sin n = sin (realToFrac n) 
    _sqrt n = sqrt (realToFrac n) 

-- | = Division is painful in Haskell
-- |   Try to overload _div for many types
-- | TODO: add more combination?
-- | TODO: add test cases
class Dnum a b where
    type DivType a b
    _div::a->b-> DivType a b

instance Dnum Integer Float where
    type DivType Integer Float = Integer 
    _div a b = div a (round b)  

instance Dnum Float Integer where
    type DivType Float Integer = Integer 
    _div a b = div (round a) b  

instance Dnum Int Float where
    type DivType Int Float = Integer 
    _div a b = div (toInteger a) (round b)  

instance Dnum Float Int where
    type DivType Float Int = Integer 
    _div a b = div (round a) (toInteger b) 

instance Dnum Double Int where
    type DivType Double Int = Integer 
    _div a b = div (round a) (toInteger b) 

instance Dnum Double Integer where
    type DivType Double Integer = Integer 
    _div a b = div (round a) b 

instance Dnum Integer Double where
    type DivType Integer Double  = Integer 
    _div a b = div a (round b)  

class Add a b where
    type SumTy a b
    addd :: a -> b -> SumTy a b

instance Add Integer Double where
    type SumTy Integer Double = Double
    addd x y = fromIntegral x + y

instance Add Double Integer where
    type SumTy Double Integer = Double
    addd x y = x + fromIntegral y

instance Add Double Int where
    type SumTy Double Int = Double
    addd x y = x + fromIntegral y

instance Add Int Double where
    type SumTy Int Double = Double
    addd x y = (fromIntegral x) + y 

instance (Num a) => Add a a where
    type SumTy a a = a
    addd x y = x + y
    

-- | division is painful like hell in Haskell
div'::Int->Int->Double
-- div' n m = (fromInteger(toInteger n)) / (fromInteger(toInteger m)) 
div' n m = fromIntegral n / fromIntegral m 

{-| 
    real to Fractional
-} 
rf::(Real a, Fractional b) => a -> b
rf = realToFrac
------------------------------------------------------------------   
-- | = simple matrix can be used in GHCi

mat = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 10]
      ]

matr =[
      [1%1, 2%1, 3%1],
      [4%1, 5%1, 6%1],
      [7%1, 8%1, 10%1]
      ]

mat'= [
       ["-2/3" , "-4/3" , "1"], 
       ["-2/3" , "11/3" , "-2"], 
       ["1"    , "-2"   , "1"]
      ]

-- | Quaternion Implementation 
data Quaternion = Quaternion{a::Float, b::Float, c::Float, d::Float} deriving (Show)

instance Num Quaternion where
    (Quaternion a1 b1 c1 d1) + (Quaternion a2 b2 c2 d2) = Quaternion(a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)
    (Quaternion a1 b1 c1 d1) - (Quaternion a2 b2 c2 d2) = Quaternion(a1 - a2) (b1 - b2) (c1 - c2) (d1 - d2)
    (Quaternion a  b  c  d)  * (Quaternion e  f  g  h)  = Quaternion(a*e - b*f - c*g -e*h) (b*e + a*f -e*g + c*h) (c*e + e*f + a*g -b*h) (d*e -c*f + b*g + a*h)
    abs _                                               = undefined
    signum _                                            = undefined
    fromInteger _                                       = undefined


eps::(Num a, Ord a)=> a -> a -> a -> Bool
eps a b e = (abs $ a - b) < e

-- | = Implement complex number operations
data C = C{x::Float, y::Float} deriving (Show)

instance Num C where
        (C x1 y1) + (C x2 y2) = C(x1 + x2) (y1 + y2)
        (C x1 y1) - (C x2 y2) = C(x1 - x2) (y1 - y2)
        (C x1 y1) * (C x2 y2) = C(x1*x2 - y1*y2) (x1*y2 + x2*y2)
        fromInteger n                     = C(fromInteger n) 0 
        signum (C x y)              = C(signum x) (signum y) 
        abs (C x y)                 = C(abs x) (abs y) 

instance Eq C where
        (C x y) == (C x' y') = x == x' && y == y'
        (C x y) /= (C x' y') = not (x == y') && not (y == y') 

con::C->C
con (C x y) = C x (-y)

mag::C->Float
mag (C x y) = abs(sqrt(realToFrac(x*x + y*y))) 

re::C->Float
re (C x _) = x

im::C->Float
im (C _ y) = y

co::C->(Float, Float)
co (C x y) = (x, y)

{-| 
    === Convert Cartesian Coordinates to Polar Coordinates
-} 
toPolar::C ->(Float, Float)
toPolar (C x y) = (r, acos (x/r))
    where
        r = sqrt (x*x + y*y)

{-| 
    === Convert Polar Coordinates to Cartesian Coordinates 
-} 
toCard::(Float, Float) -> C
toCard (r, a) = C (r*cos(a)) (r*sin(a))

{-| 
    === square root for Complex number
-} 
sqrtC:: C -> C
sqrtC c = toCard (sqrt r, a/2) 
    where
        (r, a) = toPolar c

sqrtC':: C -> C
sqrtC' c = toCard (negate $ sqrt r, a/2) 
    where
        (r, a) = toPolar c

radianToDegree::Float->Float
radianToDegree x = x*r where r = 360/(2*pi)

degreeToRadian::Float->Float
degreeToRadian x = x*d where d = (2*pi)/360

tri::C->(Float, Float, Float)
tri (C x y) = (r, cos(x/r), sin(y/r)) where r = sqrt(x*x + y*y)

data GPoint = GPoint Int Int 
getX::GPoint->Int
getX (GPoint x _) = x

getY::GPoint->Int
getY (GPoint _ y) = y 

toUpperStr::String->String 
toUpperStr s = foldr(\x y -> (toUpper x):y) [] s 

cat::[String]->String
cat  [] = [] 
cat (x:xs) = x ++ cat xs

{-| 
    === get current time
    __NOTE__ 'getCurrentTime' is UTC timezone only
-} 
getTime::IO String
getTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    -- Note: Second is of type @Pico@: It contains a fractional part.
    -- Use @fromIntegral@ to convert it to a plain integer.
    return $ (show hour) ++ ":" ++ (show minute) ++ ":" ++ (show second)


-- | = ["cat", "dog"] "cat" "pig" => ["pig", "dog"]
-- | replace a matching string with other string
replaceList::[String]->String->String->[String]
replaceList [] _ _ = [] 
replaceList (x:xs) p r = subRegex (mkRegex p) x r : replaceList xs p r 

{-| 
    === Search replace word, the code is based on 'matchAllText' in package: Text.Regex.TDFA as TD

    >let s = "mydog dog-- dog pig cat dog fox"
    >searchReplace s "dog" "[\\0]"
    >"mydog [dog]-- [dog] pig cat [dog] fox"

    Text.Regex => subRegex does not support word boundary

    >r  = makeRegex ("\\<" ++ word ++ "\\>")::TD.Regex -- word boundary

    Regex is from TDFA, NOT Text.Regex
    >word => "[[:alpha:]]+"

    NOTE: support ONE word only, e.g. searchReplace s "dog"  "[\\0]"

    TODO: support multiple words, e.g. searchReplace s "dog|cat"  "[\\0]"
-} 
searchReplace::String -> String -> String -> String
searchReplace s word rep = ls 
            where
                ss = searchSplit s word 
                second = t3 $ last ss
                ls = foldr(\x y -> x ++ y) [] $ (map(\x -> (t1 x) ++ (TR.subRegex (TR.mkRegex word) (t2 x) rep)) $ ss) ++ [second]

                t1 (x, y, z) = x
                t2 (x, y, z) = y 
                t3 (x, y, z) = z 

                -- input Str -> word to be replaced -> return
                searchSplit::String -> String -> [(String, String, String)]
                searchSplit s word = map(\x -> let t1 = fst x;
                                                   t2 = snd x;
                                                   c  = (fst t1) + (snd t1) -- 0
                                                   fs = fst (snd x) -- 6 
                                                   sn = snd (snd x) -- 3
                                                   re = drop c s 
                                                   rs = take (fs - c) $ drop c s -- "mydog "
                                                   rx = drop (fs - c) s -- "dog-- dog pig cat"
                                                   in (rs, take sn $ drop fs s, drop (fs + sn) s)) tp 

                                            where 
                                                r  = makeRegex ("\\<" ++ word ++ "\\>")::TD.Regex -- word boundary
                                                ar = matchAllText r s 
                                                tt = (0, 0):(map(\x -> snd $ x ! 0 ) ar) -- [(0, 0), (6, 3), (12, 3)]
                                                tp = zipWith(\x y -> (x, y)) (init tt) (tail tt) -- [((0, 0), (6, 3)), ((6, 3), (12, 3))]


-- | = repeat's 3 "pig" => ["pig", "pig", "pig"]
repeat'::Integer->a->[a]
repeat' n a = map(const a)[1..n] 

{-| 
    === replicate' 4 'a' => "aaaa"
-} 
replicate'::Int->a->[a]
replicate' n x = take n (repeat x)


{-| 
    Convert Lazy ByteString to Strict ByteString
    Data.ByteString => Data.ByteString.Lazy
-} 
toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

{-| 
    === can be replaced with following: 

    >(++) <$> (Just [1]) <*> (Just [2])
-} 
catMaybe::Maybe [a]-> Maybe [a] -> Maybe [a]
catMaybe Nothing Nothing           = Nothing
catMaybe Nothing (Just list)       = Just list
catMaybe (Just list) Nothing       = Just list
catMaybe (Just list1) (Just list2) = Just (list1 ++ list2)

-- remove white space from string
removeSpace::String->String
removeSpace s = filter(\x -> isSpace x == False) s 

-- check if string is empty
isEmpty::String->Bool
isEmpty "" = True 
isEmpty _  = False 

trimWS::String->String
trimWS []  = [] 
trimWS s   = filter(not . isSpace) s 


trim::String->String
trim s  = TS.unpack $ TS.strip $ TS.pack s

-- | split list of string when string is empty/space
-- | ["dog", " ", "", "cat"] => [["dog"], [], ["cat"]]
splitListEmptyLine::[String]->[[String]]
splitListEmptyLine xs = filter(\x-> length x > 0) $ splitWhen(\x -> (length $ trimWS x) == 0) xs 

splitList::[String]->([String], [String])
splitList [] = ([], [])
splitList [x] = ([x], [])
splitList (x:y:xs) = (x:xp, y:yp) where (xp, yp) = splitList xs

-- | tuple to list
tuplify2::[a]->(a, a)
tuplify2 [a, b] = (a, b) 

tuplify3::[a]->(a, a, a)
tuplify3 [a, b, c] = (a, b, c) 


strToLazyText::String -> TL.Text
strToLazyText = TL.pack

lazyTextToStr::TL.Text -> String
lazyTextToStr = TL.unpack

strToStrictText::String -> TS.Text
strToStrictText = TS.pack

strictTextToStr::TS.Text -> String
strictTextToStr = TS.unpack


lazyTextToLazyByteString::TL.Text -> BL.ByteString
lazyTextToLazyByteString = TLE.encodeUtf8

lazyByteStringToLazyText::BL.ByteString -> TL.Text
lazyByteStringToLazyText = TLE.decodeUtf8

strictTextToStrictByteString::TS.Text -> BS.ByteString
strictTextToStrictByteString = TSE.encodeUtf8

strictByteStringToStrictText::BS.ByteString -> TS.Text
strictByteStringToStrictText = TSE.decodeUtf8


strictByteStringToLazyByteString::BS.ByteString -> BL.ByteString
strictByteStringToLazyByteString = BL.fromStrict

lazyByteStringToStrictByteString::BL.ByteString -> BS.ByteString
lazyByteStringToStrictByteString = BL.toStrict

strictTextToLazyText::TS.Text -> TL.Text
strictTextToLazyText = TL.fromStrict

lazyTextToStrictText::TL.Text -> TS.Text
lazyTextToStrictText = TL.toStrict

{-|
    === padding whitespace to 2d list
-}
ppad m = tran $ map (f) $ tran m
    where
        m' = (map . map) show m
        f n = map(\x -> pad x ma) $ map show n 
            where
                ma = foldr(\x y -> max x y) 0 $ map len $ join m' 
                pad x n = if length x < n  then pad (x ++ " ") n else x ++ ""


{-| 
    === C style printf, string format

    * import Text.Prinf

    > printf "%.2f" 3.0
    > 3.00
    > let n = 3::Int
    > let f = rf n
    > printf "%.2f, %.2f" f f
    >
    >let name = "dog"
    >let weight = 20
    >printf "(%s)=(%d)"
    >"(dog)=(20)"
    >
    >pf "%.2f" 3.0 
    >3.00
-} 
pf s d = printf s d

{-| 
    === Print matrix with labe, print matrix with string, 

    >pm "my matrix" mat
    >------------------------------------my matrix-----------------------------------
    >[1,2,3]
    >[4,5,6]
    >[7,8,10]
-} 
pm::(Show a)=>String->[[a]]->IO()
pm s x = fw s >> mapM_ print x  

{-| 
    === print matrix, show matrix
-} 
pa::(Show a)=>[[a]]->IO()
pa x = mapM_ print x

pa1::(Show a)=>[a]->IO()
pa1 x = mapM_ print x

{-| 
    === Partition list to n chunks

    __Note__: print partList n [] will cause error since print needs concrete type 

    split list to n blocks

    >partList 2 [1, 2, 3, 4, 5] 
    >[[1,2], [3, 4],[5]]
    >
    >partList 2 [1, 2, 3, 4, 5, 6] 
    >[[1,2], [3, 4],[5, 6]]
-} 
partList ::Int->[a]->[[a]]
partList _ [] = []
partList n xs = (take n xs) : (partList n $ drop n xs)

partList2 ::Int->[a]->[[a]]
partList2 _ [] = []
partList2 n xs = (take n xs) : (partList n $ drop 1 xs)

{-| 
    === Partition string to [String] according to character class []

    >splitStrRegex "[,.]" "dog,cat,cow.fox" => ["dog", "cat", "cow", "fox"]
-} 
splitStrRegex::String->String->[String]
splitStrRegex r s = splitWhen(\x -> matchTest rex (x:[])) s
                where
                    rex = mkRegex r

{-| 
    === split file with Regex e.g. empty line

    @
    ls <- readFileToList "/tmp/x.x"
    let lss = splitBlock ls "^[[:space:]]*"  => split file with whitespace
    pp lss

    let lss = splitBlock ls "^[[:space:]]*(---){1,}[[:space:]]*"  => split file "---"
    let lss = splitBlock ls "^[[:space:]]*(===){1,}[[:space:]]*"  => split file "==="
    @

    <file:///Users/cat/myfile/bitbucket/haskell/splitBlock.hs TestFile>
-} 
splitBlock::[String] -> String -> [[String]]
splitBlock [] _ = []
splitBlock cx pat = splitWhen (\x -> matchTest (mkRegex pat) x) cx

{-| 
    === parse file and partition it into blocks according to patterns: bs ws

    >bs = "^[[:space:]]*(---){1,}[[:space:]]*" -- split with delimiter "---"
    >ws = "[,. ]"                              -- split with [,. ]
    >parseFileBlock bs ws ["rat", 
    >                      "-----", 
    >                      "my dog eats my cat, my cat eats my dog."
    >                     ]
    >[(0, ["rat"]), (1, ["my", "dog", "eats", "my", "cat"])]

    <file:///Users/cat/myfile/bitbucket/haskell/parseRandom.hs Test_File>
-} 
parseFileBlock::String->String-> [String] -> [(Integer, [String])]
parseFileBlock _ _ [] = [] 
parseFileBlock bs ws cx = lt 
    where
        bl = filter(\x -> length x > 0) $ splitBlock cx bs
        ls = (map . map) (\x -> splitStrRegex ws x) bl
        ds = (map . map ) (\r -> filter(\w -> (len w > 0) && isWord w) r) ls
        sb = map(\x -> unique $ join x) ds --                                = > [["dog", "cat"], ["cow"]]
        lt = zip [0..] sb --                                                 = > [(0, ["dog", "cat"])]


{-| 
    === all prefix strings from given string
-} 
prefix::String->[String]
prefix s = filter(\x -> length x > 0) $ map(\n -> take n s) [0..length s] 

{-| 
    === unique, remove duplicate elements from a list

    Convert the list to Set and convert the Set back to List
-} 
unique::(Ord a)=>[a]->[a]
unique xs = DS.toList $ DS.fromList xs

-- length of two lists has to be same 
-- [["a"], ["b"]] [["1"], ["2"]] -> [["a", "1"], ["b", "2"], []]
mergeListList::[[String]]->[[String]]->[[String]]
mergeListList [[]] [[y]] = [["", y]] 
mergeListList [[x]] [[]] = [[x, ""]] 
mergeListList [[]] (y:ys) = ([]:y):ys 
mergeListList (x:xs) [[]] = ([]:x):xs
mergeListList (x:xs) (y:ys) = (zipWith(\ex ey -> [ex, ey]) x y) ++ (mergeListList xs ys) 

-- [1, 2, 3] [10, 11] => [1, 10, 2, 11, 3]
mergeList::[a]->[a]->[a]
mergeList _ [] = []
mergeList [] _ = [] 
mergeList (x:xs) (y:ys) = x:y:mergeList xs ys 

-- both list has to be the same length
-- ow. return Nothing
mergeListLen::[a]->[a]->Maybe [a]
mergeListLen [] [] = Just [] 
mergeListLen (x:xs) (y:ys) = 
            case mergeListLen xs ys of
            Just merged -> Just (x:y:merged)
            Nothing  -> Nothing
mergeListLen _ _  = Nothing

-- | merge two sorted lists
-- | [1, 4] [3] => [1, 3, 4]
mergeSortList::(Ord a)=>[a]->[a]->[a]
mergeSortList x [] = x
mergeSortList [] x = x
mergeSortList (x:xs) (y:ys) = if x < y then x:(mergeSortList xs (y:ys)) else y:(mergeSortList (x:xs) ys)

-- iterate a list like a for loop or forM_
iterateList::[a]->(a -> IO ()) -> IO ()
iterateList [] f = return ()
iterateList (x:xs) f = f x >> iterateList xs f

unwrap::Maybe a -> a
unwrap Nothing = error "There are some errors, Cruft" 
unwrap (Just x) = x

codeCapture::String->String
codeCapture str = subRegex(mkRegex pat) str rep 
                    where rep = "<cool>\\1</cool>" 
                          pat = "(([^`]|`[^[]]*|\n*)*)"

{-| 
    === Binary Search
-} 
binarySearch::(Ord a)=>a -> [a]-> Bool
binarySearch k [] = False
binarySearch k cx = if l == 1 then k == head cx 
                     else (if k < head re then binarySearch k le 
                           else (if k > head re then binarySearch k (tail re) else True))
                    where
                        l = length cx
                        m = div l 2
                        le = take m cx
                        re = drop m cx

-- |
-- = There are many versions of quicksort here
-- == bad version 
qqsort::(a->a->Bool)->[a]->[a]
qqsort cmp [] = []
qqsort cmp (x:xs) = qqsort cmp ([ e | e <- xs, cmp e x ]) ++ [x] ++ qqsort cmp [ e | e <- xs, not $ cmp e x]

{-| 
    === Sort list in nature ordering, e.g. [2, 1] => [1, 2] 
-} 
sort::(Ord a)=>[a]->[a]
sort (x:xs) = qqsort cmp ([ e | e <- xs, cmp e x ]) ++ [x] ++ qqsort cmp [ e | e <- xs, not $ cmp e x]
    where
        cmp x y = x < y

{-| 
    === Quick Sort in Vector, Did not see any speed decreasing 
-} 
sqVec::(Ord a) => V.Vector a -> V.Vector a
sqVec v = if V.length v == 0 then v 
                          else (sqVec $ V.filter(\x -> x < h ) rest) V.++ (V.fromList [h]) V.++ (sqVec $ V.filter(\x -> x > h) rest)
    where
        h = V.head v
        rest = V.tail v

-- |
-- == better version 
quickSort::[Int]->[Int]
quickSort [] = []
quickSort [x] = [x]
quickSort l = quickSort(left) ++ [p] ++ quickSort right 
                    where
                        left =  [x | x <- init l, x < p]
                        right = [x | x <- init l, x >= p]
                        p = last l 

-- |
-- == nice version 
quickSort'::[Int]->[Int]
quickSort' [] = []
quickSort' (x:xs) = quickSort' ([ l | l <- xs, l < x]) ++ [x] ++ quickSort' ([ r | r <- xs, r >= x]) 

-- note: quickSort1 [] -- get error
-- print quickSort1 ([]::Int)  -- it works
{-| 
    === Deprecated 
    === Use 'quickSortAny'
-} 
quickSort1::(Ord a)=>[a]->[a]
quickSort1 [] = [] 
quickSort1 (x:xs) = quickSort1 ([ l | l <- xs, l < x ]) ++ [x] ++ quickSort1 ([ r | r <- xs, r >= x])

{-| 
    === Quick Sort for any type
    == same as quickSort1, just a better name
-} 
quickSortAny::(Ord a)=>[a]->[a]
quickSortAny [] = [] 
quickSortAny (x:xs) = quickSortAny ([ l | l <- xs, l < x ]) ++ [x] ++ quickSortAny ([ r | r <- xs, r >= x])


mergeSortC::(a -> a -> Bool)->[a]->[a]
mergeSortC _ []  = []
mergeSortC _ [x] = [x]
mergeSortC f l   = merge f (mergeSortC f left) (mergeSortC f right) 
                 where
                    half = (length l) `div` 2
                    left = take half l
                    right= drop half l  

                    merge::(a -> a -> Bool)->[a]->[a]->[a]
                    merge f [] r = r
                    merge f l [] = l
                    merge f (x:xs) (y:ys)  = if f x y 
                                            then 
                                               x:merge f xs (y:ys) 
                                            else
                                               y:merge f (x:xs) ys 

mergeSort::[Int]->[Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge(mergeSort left) (mergeSort right) 
                where
                    half = (length l) `div` 2
                    left = take half l
                    right= drop half l  

                    merge::[Int]->[Int]->[Int]
                    merge [] r = r
                    merge l [] = l
                    merge (x:xs) (y:ys)  = if x < y
                                            then 
                                               x:merge xs (y:ys) 
                                            else
                                               y:merge (x:xs) ys 

mergeSortM::(Num a, Eq a)=>[[a]]->[[a]]
mergeSortM [] = []
mergeSortM [x] = [x]
mergeSortM l = merge(mergeSortM left) (mergeSortM right) 
                where
                    half = (length l) `div` 2
                    left = take half l
                    right = drop half l  

                    merge::(Num a, Eq a)=>[[a]]->[[a]]->[[a]]
                    merge [] r = r
                    merge l [] = l
                    merge (x:xs) (y:ys)  = if not $ moreZero x y 
                                            then 
                                               x:merge xs (y:ys) 
                                            else
                                               y:merge (x:xs) ys 

                    -- [0, 1]  [1]         => False
                    -- [0, 1]  [1, 1]      => True
                    -- [1, 1]  [0, 1]      => False
                    -- [0, 1, 0] [1, 0, 0] => True
                    moreZero::(Num a, Eq a)=>[a]->[a]->Bool
                    moreZero [] [] = False
                    moreZero m1 m2 = (len $ takeWhile (== 0) m1) > (len $ takeWhile(== 0) m2)

{-| 
    === Read Snippet
    
    >read $b/vim/snippet.m file 
    >partition blocktexts into list
    >parse the header. e.g. latex_eqq:*.tex: latex equation
    >return [([header str]), [blocktext])]

    input file:
    >main:*.hs: my dog, my cat 
    >my fox eats my rat 

    >output:
    >[(["*.hs","main","my cat","my dog"],["main:*.hs: my dog, my cat ","my fox eats my rat "])]
    
-} 
readSnippet::FilePath->IO [([String], [String])]
readSnippet path = do 
            list <- readFileToList path;
            let ll = filter(\x -> length x > 0) $ splitWhen(\x -> (length $ trim x) == 0) list
            -- let plist = map(\x -> ((splitStrRegex "[,:]" $ head x), x) ) ll
            let plist = map(\x -> ((removeIndex 1 $ splitStrRegex "[:]" $ head x), x) ) ll
            let pplist = map(\k -> (
                                       unique $ foldr(++) [] (map(\x -> map(\r -> trim r) $ splitStrRegex "[,]" x) (fst k)), 
                                       snd k
                                   ) 
                            ) plist
            return pplist 

{-| 
    === read dir, all dir or files in [String]

    >["f1", "f2"]
-} 
readFileToList::FilePath->IO [String] 
readFileToList = (fmap lines) . readFile -- or lines <$> readFile

{-| 
    === Write [String] to file
-} 
writeToFile::FilePath->[String]->IO()
writeToFile p list = writeFile p $ unlines list 

{-| 
    === shorthand Write [String] to file
-} 
wfs::FilePath->[String]->IO()
wfs p list = writeToFile p list
  
wf::FilePath->String->IO()
wf = writeFile

writeToFileAppend::FilePath->[String]->IO()
writeToFileAppend f list = appendFile f $ unlines list 

{-| 
    === Matrix: Integer to Fractional, convert integer matrix to Fractional matrix

    see 'nToNumMat'

    >class (Num a)=>Fractional a where
    >class (Num a, Ord a)=> Real where
    >class (Real a, Enum a)=> Integral where

-} 
nToFractMat::(Real a, Fractional b)=>[[a]]->[[b]]
nToFractMat = (map . map) realToFrac 

{-| 
    === Convert Integer matrix to Num Matrix

    see 'nToFractMat'
-} 
nToNumMat m = (map . map) fromInteger m 

{-| 
    === Matrix: Real to Rational

    see 'nToFractMat', 'nToNumMat'

    >data Rational = Ratio Integer
-} 
rToRatMat::(Real a)=>[[a]] -> [[Rational]]
rToRatMat = (map. map) toRational 

{-| 
    === Write 2d Matrix to file, write 2d list to file, write matrix to file

    see 'writeToFileMat'

    \[
        \begin{bmatrix}
        1 & 2 \\
        3 & 4 \\
        \end{bmatrix} \Rightarrow 
        \begin{matrix}
        1 & 2 \\
        3 & 4 \\
        \end{matrix}
    \]
   
    > m = [[1, 2], [3, 4]]
    > writeToFile2dMat "/tmp/m.x" m 
    > :!cat /tmp/m.x
-}
writeToFile2dMat::(Num a, Fractional a, Show a)=>FilePath->[[a]]->IO()
writeToFile2dMat f m2 = writeToFile f $ map(\r -> foldr(\x y -> x ++ " " ++ y) [] r) $ ppad m2 

{-| 
    === write String matrix to file

    see 'writeToFile2dMat'
-} 
writeToFileMat::FilePath->[[String]]->IO()
writeToFileMat p m2 = writeToFile p $ map(\r -> foldr(\x y -> x ++ " " ++ y) [] r) $ ppad m2 


{-| 
    print output, similar Print.java 
-} 
p::(Show s)=>s->IO()
p s = print s 

pp::(Show s)=>s->IO()
pp s = print s 

fl::IO()
fl = print $ foldr (++) "" $ replicate 80 "-" 

pw::(Show s)=>String->s->IO()
pw w s = fpp w s 

fpp::(Show s)=>String->s->IO()
fpp msg s = fw msg >> pp s 

fw::String->IO()
fw msg = putStr (left ++ msg ++ right ++ "\n")
                where
                    line = foldr(++) "" $ replicate 80 "-"
                    diff   = 80 - (length msg)
                    half   = div diff 2
                    isEven = mod diff 2
                    left   = foldr(++) "" $ replicate (half + isEven)  "-"
                    right  = foldr(++) "" $ replicate half  "-"


ff::(Show var)=>String->var->IO()
ff msg var = putStr (left ++ msg ++ right ++ "\n" ++ show(var) ++ "\n")
                where
                    line = foldr(++) "" $ replicate 80 "-"
                    diff   = 80 - (length msg)
                    half   = div diff 2
                    isEven = mod diff 2
                    left   = foldr(++) "" $ replicate (half + isEven)  "-"
                    right  = foldr(++) "" $ replicate half  "-"

infixl 1 <<<
(<<<)::(Show a)=>String -> a -> String 
(<<<) s a = s ++ (show a)

{-| 
    === Backward substitute
    
    Given upper triangle matrix \(A\) and vector \(\vec{b}\), solve for \(x\)

    <http://localhost/pdf/backwardsubstitute.pdf backward_substitute>

    \[
        Ax = b
    \]
    \[  
        A = \begin{bmatrix}
            1 & 2 \\ 
            0 & 4 \\
            \end{bmatrix} 
            \begin{bmatrix} 
            x_1 \\
            x_2
            \end{bmatrix} = 
            \begin{bmatrix} 
            b_1 \\
            b_2
            \end{bmatrix} 
    \]
-} 
bs::[[Double]] -> [Double] -> [Double] -> [Double]
bs []  _  _        = []
bs _   _  []       = []
bs m x b = f (reverse m) x (reverse b)
    where
        f []  _  _        = []
        f _   _  []       = []
        f (r:xa) x (b:xb) = let x' = bs' r x b in x':(f xa (x':x) xb)
            where
                bs' r x b = xi
                    where 
                        s = sum $ (init $ reverse r) * x -- a[i][1]x[1] + a[i][2]x[2]
                        a = last $ reverse r 
                        xi = (b - s)/a  --  x_i = (b_ii - sum ())/ a_i

{-| 
    === forward substitute
    
    Given lower triangle matrix \(A\) and vector \(\vec{b}\), solve for \(x\)

    <http://localhost/pdf/backwardsubstitute.pdf backward_substitute>

    \[
        Ax = b
    \]
    \[  
        A = \begin{bmatrix}
            1 & 0 \\ 
            2 & 4 \\
            \end{bmatrix} 
            \begin{bmatrix} 
            x_1 \\
            x_2
            \end{bmatrix} = 
            \begin{bmatrix} 
            b_1 \\
            b_2
            \end{bmatrix} 
    \]
-} 
fs::[[Double]] -> [Double] -> [Double] -> [Double]
fs []  _  _        = []
fs _   _  []       = []
fs m x b = f (id m) x (id b)
    where
        f []  _  _        = []
        f _   _  []       = []
        f (r:xa) x (b:xb) = let x' = fs' r x b in x':(f xa (x':x) xb)
            where
                fs' r x b = xi
                    where 
                        s = sum $ (init $ id r) * x -- a[i][1]x[1] + a[i][2]x[2]
                        a = last $ id r 
                        xi = (b - s)/a  --  x_i = (b_ii - sum ())/ a_i


-- dot product list
listDot::(Num a)=>[a]->[a]->a
listDot [] y = 0 
listDot x [] = 0 
listDot x y = sum $ zipWith(*) x y 

listDots::(Num a)=>[[a]]->[[a]]->a
listDots x y = (sum . join) $ (zipWith . zipWith)(*) x y

-- multiply scale and list 
scaleList::(Num a)=>a -> [a] -> [a]
scaleList _ [] = [] 
scaleList c v  = map(\x -> c*x) v

listScale::(Num a)=>[a] -> a -> [a]
listScale [] _ = [] 
listScale v  c = map(\x -> c*x) v

-- add two lists
listAdd::(Num a)=>[a]->[a]->[a]
listAdd [] _ = []
listAdd _ [] = []
listAdd u v  = zipWith(\x y -> x+y) u v 

listSub::(Num a)=>[a]->[a]->[a]
listSub [] _ = []
listSub _ [] = []
listSub u v  = zipWith(\x y -> x - y) u v 

listMul::(Num a)=>[a]->[a]->[a]
listMul [] _ = []
listMul _ [] = []
listMul u v  = zipWith(\x y -> x * y) u v 

--listDiv::[Integer]->[Integer]->[a]
--listDiv [] _ = []
--listDiv _ [] = []
--listDiv u v  = zipWith(\x y -> (realToFrac x)/ (realToFrac y)) u v 

listNeg::(Num a)=>[a]->[a]
listNeg [] = []
listNeg u  = map(\x -> -x) u


{-| 
    === get dimenstion of matrix -> (nrow, ncol)

    if [] => (0, 0)
-} 
dim::[[a]]->(Int, Int)
dim  [] = (0, 0)
dim  (x:xs) = (length (x:xs), length x)

{-| 
    === Remove element from a list
    
    > removeIndex 1 [1,2,3] 
    > [1, 3]
    >
    > removeIndex 3 [] 
    > []
    >
    > removeIndex 2 [1]
    > error
-} 
removeIndex::Int->[a]->[a]
removeIndex _ [] = []
removeIndex n cx = (fst $ splitAt n cx) ++ (tail $ snd $ splitAt n cx)


removeRowCol::Integer->Integer->[[a]]->[[a]]
removeRowCol _ _ [] = []
removeRowCol m n cx = transpose $ removeIndex (fromInteger n) $ transpose $ removeIndex (fromInteger m) cx

{-|
    === Use co-factor expantion to find the determinant of n by n matrix
    * It is supper slow. 
    * Note: \( \det M = \det M^{T} \)
    <http://localhost/html/indexMathDefinition.html#prove_determinant Proof> 
-}
det::(Num a)=>[[a]] -> a 
det [[x]] = x 
det m = sum $ zipWith(\x y -> let a = fst x; b = snd x in (-1)^a*b*(det y)) c' co 
        where
            row = head m
            len = length m
            len' = toInteger len
            c   = [0..len']
            c'  = zipWith(\x y -> (x, y)) c row
            lm  = replicate len m 
            co  = zipWith(\x m -> removeRowCol 0 x m) c lm

-- | matrix multiplicaiton in Int 
multiMatInt::[[Int]]->[[Int]]->[[Int]]
multiMatInt a b = [ [sum $ zipWith (*) ar bc | bc <- (transpose b)]  | ar <- a]

-- | matrix multiplication in Double
multiMatDouble::[[Double]]->[[Double]]->[[Double]]
multiMatDouble a b = [ [sum $ zipWith (*) ar bc | bc <- (transpose b)]  | ar <- a]

--multiMatVec::(Num a)=>[[a]]->[a]->[a]
--multiMatVec m v = map(\r -> listDot r v) m

-- | matrix multiplication in Rational Number 
multiMatR::[[Rational]]->[[Rational]]->[[Rational]]
multiMatR mat1 mat2= transpose $ map(\m2 -> map(\m1 -> sum $ zipWith(*) m1 m2) mat1) $ transpose mat2


{-|
    === Use outer product to compute the matrix multiplication

   * use outer product to compute matrix mulitplication
   * outer(col_1  row_1) + outer(col_2  row_2) + outer(col_3 row_3)
   
   column vector
   @
   [[1], 
    [2], 
    [3]]
   @ 
   row vector
   @
    [[1, 2, 3]]
   @
-}
multiMat::(Num a)=>[[a]]->[[a]]->[[a]]
multiMat mat1 mat2= foldr(\x y -> matSum x y) zero ma 
                where
                    -- a list of matries
                    ma = zipWith(\x y -> outer x y) (transpose mat1) mat2
                    zero = map(\x -> replicate (length mat1) 0) [1..length mat1]
                    -- zero = geneMat (len mat1) Zero
                    -- sum matrix                                                           
                    matSum::(Num a)=>[[a]]->[[a]]->[[a]]                                                        
                    matSum m1 m2 = zipWith(\x y -> zipWith(\x1 y1 -> x1+y1) x y) m1 m2

matDiv::[[Integer]]->Integer->[[Integer]]
matDiv m n = map(\x -> map(\y -> div y n) x) m

-- | matrix multiplication in String/Rational
multiRatMat::[[String]]->[[String]]->[[String]]
multiRatMat a b = [[sumRatList $ zipWith (multR) ar bc | bc <- (transpose b)]  | ar <- a]

sumRatList::[String]->String
sumRatList cx = foldl(addR) "0/1" cx


-- add rational numbers as String 
addR::String->String->String
addR s1 s2 = cx 
            where
                n1 = splitR s1
                n2 = splitR s2
                nn1 = map(\x -> x*(last n2)) n1 
                nn2 = map(\x -> x*(last n1)) n2 
                nn = ((head nn1) + (head nn2)):(tail nn1)
                list = reduce nn 
                cx = show (head list) ++ "/" ++ show (last list)
                
{-| 
    === Convert Rational to Integer:
    >>> ratToInt "3/1"
    3
-} 
ratToInt::String->String
ratToInt s = if dn == 1 then show nu else s 
        where
            xs = splitR s
            nu = head xs
            dn = last xs
{-| 
    === 'normalR' -3/-1 = 3/1, 3/-2 => -3/2
-} 
normalR::String->String
normalR s = ss 
        where
            xs = splitR s
            nu = head xs
            dn = last xs
            ss = if nu < 0 && dn < 0 then show (-nu) ++ "/" ++ show (-dn) 
                    else (if nu > 0 && dn < 0 then show (-nu) ++ "/" ++ show (-dn) else s)

{-| 
    === 'reduceForm' list of rational
-} 
reduceForm::[[String]]->[[String]]
reduceForm m = map(\r -> map (ratToInt . normalR) r) m 

{-| 
    === reduce numerator and denominator:
    >>> reduce 6/2 
    3/1
-} 
reduce::[Integer]->[Integer]
reduce cx = cx' 
            where
                d = gcd (head cx) (last cx) 
                cx' = map(\x -> x `quot` d) cx

{-| 
    === Convert string to Integer, str to int

    >stringToInt "123"
    >123
    >
    >stringToInt "a"
    >error
-} 
stringToInt::String->Integer
stringToInt s = read s::Integer 


{-| 
    === split first word with delimiter: isSpace

    <https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser parser>
-} 
word::String -> (String, String)
word [] = ([], [])
word (c:cs) | isSpace c = ([], cs) 
            | otherwise = let (c', cs') = word cs 
                          in  (c:c', cs')

{-| 
    === Check a string whether it is a word or not
    == word is defined as  "^[[:alpha:]]+$"

    >isWord "dog"
    >True
    >
    >isWord "dog1"
    >False
    >
    >isWord::String -> Bool
    >isWord s = matchTest (mkRegex "^[[:alpha:]]+$") s 
-} 
isWord::String -> Bool
isWord s = matchTest (mkRegex "^[[:alpha:]]+$") s 


{-| 
    === split all words with delimiter: isSpace

    <https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser parser>

    >print $ sentence "dog cat 133"
    >["dog", "cat", "133"]
-} 
sentence::String -> [String]                          
sentence [] = []
sentence str = let (w, cs) = word str
               in w:sentence cs 


{-| 
    === Permutation of any type, the algo is based on prefix string 

    <http://localhost/image/permtree1.svg permutation_tree>

    @
    fun (prefix, int index, removeIndex(index str))
    @

    > perm "12"
    >[["12", "21"]]
-} 
perm::[a] -> [[[a]]]
perm [] = [] 
perm cx = map (\i -> pp i cx) [0..(length cx) - 1]
    where
        -- p 1 "dog" => "odg"
        p i s = (s !! i) : (removeIndex i s) 
        -- 
        pp i s = if i < len then [p i s] ++ (pp (i+1) s) else [] 
            where 
                len = length s

{-| 
    === Permutation of any type 

    > perm "12"
    >[["12", "21"]]

    @
        abc
            a   bc
                b c
                    [[]]
                c b
                    [[]]

            b   ac
                a c
                    [[]]
                c a
                    [[]]

            c   ab
                a b
                    [[]]
                b a
                    [[]]
    @ 

-} 
perm2::(Eq a)=>[a] -> [[a]]
perm2 [] = [[]]
perm2 cx = [ pre:r | pre <- cx, 
                   r <- perm2 $ filter(\x -> x /= pre) cx]
        
{-| 
    === And other Permutation

    <https://stackoverflow.com/questions/40097116/get-all-permutations-of-a-list-in-haskell SO>
-} 
perm3 :: [a] -> [[a]]
perm3 []        = [[]]
perm3 il@(x:xs) = concatMap ((rotations len).(x:)) (perm3 xs)
                  where len = length il

                        rotations :: Int -> [a] -> [[a]]
                        rotations len xs = take len (iterate (\(y:ys) -> ys ++ [y]) xs)

{-| 
    === Convert Integer to string, int to str

    >>> intToString 123
    "123"
-} 
intToString::Integer->String
intToString n = show n

splitR::String->[Integer]
splitR s = xx' 
            where 
                xs = splitRegex(mkRegex "/") s
                -- it is integer only:"3" => "3/1"
                xs' = if length xs == 1 then xs ++ ["1"] else xs 
                xx = map(\x -> read x::Integer) xs' 
                xx' = case last xx of
                        0 -> error "denominator cannot be zero"
                        _ -> xx

multR::String->String->String
multR s1 s2 = ss 
            where
                n1 = splitR s1
                n2 = splitR s2
                nn = zipWith(\x y-> x*y) n1 n2
                cx = reduce nn
                ss = show (head cx) ++ "/" ++ show (last cx)

-- "3" ["1, "2"] => ["3", "6"]
multRL::String->[String]->[String]
multRL s cx = map(\x -> multR s x) cx

normR::[String]->[String] ->String
normR [] _ = []
normR _ [] = []
normR x y = foldl(addR) "0/1" $ zipWith(\x y -> multR x y) x y 

getVec::Int->[[a]]->[[a]]
getVec n m = map(\x -> [x]) $ map(\r -> r !! n) m

{-|
  === Normalize a list as vector
  @
  [[1, 2, 3]]     is row vector
  [[1], [2], [3]] is column vector
  @
 \[
  \| \vec{v} \| = \sqrt{x^2 + y^2 + z^2} \quad \text{ where } 
  \vec{v} = \begin{bmatrix}
  x & y & z
  \end{bmatrix}
 \]
-} 
normList::(Floating a)=>[[a]] -> [[a]]
normList m = (map . map) (/n) m 
        where
            n = sqrt $ sum $ join $ (zipWith . zipWith) (*) m m

-- u project on w
proj::[String]->[String]->[String]
proj w u = multRL (divR (normR w u) (normR w w)) w 

--projm::[[Rational]]->[[Rational]]->[[Rational]]
--projm c r = multiMat c r
--        where
--            m = multiMat c r
--            n2= multiMat c (tran r)
--            m2 = map(\x ->map(\y -> y*y) x) m



-- | projection from u onto v in n dimensional list
-- | 
-- |   puv = (u v /<v, v>) v
-- |
-- | 
projn::(Fractional a)=>[[a]]->[[a]]->[[a]]
projn u v = c `mu` v
  where
   dot w k = (sum . join) $ (zipWith . zipWith) (*) w k
   d      = dot u v
   c      = d/(dot v v)
   mu a w = (map . map)(*a) w

{-|
    === Form a almost R matrix: almost an upper triangle matrix

    <http://localhost/pdf/gram_schmidt.pdf QR_Decomposition>
    \[
       R' = \begin{bmatrix}    
            \left<e_1, v_1 \right> & \left<e_1, v_2 \right> & \left< e_1, v_3 \right> \\
            \emptyset              & \left<e_2, v_2 \right> & \left< e_2, v_3 \right> \\
            \emptyset              & \emptyset              & \left< e_3, v_3 \right>
            \end{bmatrix} \quad    
       R = \begin{bmatrix}    
            \left<e_1, v_1 \right> & \left<e_1, v_2 \right> & \left< e_1, v_3 \right> \\
            0                      & \left<e_2, v_2 \right> & \left< e_2, v_3 \right> \\
            0                      & 0                      & \left< e_3, v_3 \right>
            \end{bmatrix} \\
       R' \neq R
    \]
-}
rMatrixUpperTri ex vx = (zipWith . zipWith) (\e v -> listDots e v) ex' vx'
    where
        ex' = liste ex
        vx' = listv vx
        liste cx  = zipWith(\n v -> replicate n v) (reverse [1..len cx]) cx
        listv cx  = map(\n -> drop n cx) [0..(len cx - 1)]

{-|
    === rejection \(v_k\) onto span of \( \{ a_1 \dots a_{k-1} \} \)
    <http://localhost/pdf/gram_schmidt.pdf QR_Decomposition>
    \[
        a_k = v_k - \sum_{i=1}^{k-1} \frac{\left<v_k, a_i \right>}{\left<a_i, a_i \right> } a_i 
    \]
    
    * v3 projects onto \( \{a_1, a_2 \} \)

    > a3 = v3 - ((projn v3 a1) + (projn v3 a2)) 

    * if the matrix is singular then one of \( \left\| a_k \right\| = 0 \)
    * that can be used to determinate the determinant of a matrix, but not the sign of a determinant
    * compute the determiant is hard problem: \( \mathcal{O}(n^3) \), there is better algo.
-}
rejection::(Fractional a)=>[[[a]]] -> [[[a]]] -> [[[a]]]
rejection  an [] = an 
rejection  an (vv:cx) = rejection ([vv - (foldr(\x acc -> x + acc) z $ join $ [map(\a -> projn vv a) an])] ++ an)  cx
        where
            z = replicate (len vv) [0]

{-|
    === QR decomposition or QR factorization
    <http://localhost/pdf/gram_schmidt.pdf QR_Decomposition>

     * Given \(M\) is square matrix, there exists a pair of matrices \( Q \) is unitary matrix
      and \( R \) is upper triangle matrix such that:
    \[
        M = QR    
    \]
-}
qrDecompose'::[[Double]]->([[Double]], [[Double]])
qrDecompose' m = (qm, rm)
    where
        -- [v1, v2, v3]
        vx = map(\r -> map(\x -> [x]) r) $ tran m 
        -- [e1, e2, e3]
        hd = take 1 vx 
        rs = tail vx
        ex = reverse $ map normList $ rejection hd rs
        -- R matrix, upper triangle matrix
        tm = rMatrixUpperTri ex vx
        -- R matrix, padding with zeros
        rm = zipWith(\n  x -> (replicate n 0) ++ x) [0..] tm 
        -- Q matrix
        qm = tran $ map (\x -> foldr(++) [] x) ex
   
{-| 
    === Add two list of Rational string

    >addRL ["1/2", "1/2"] ["1/2", "1/2"]
    >["1/1", "1/1"]
-} 
addRL::[String]->[String] ->[String]
addRL [] _ = []
addRL _ [] = []
addRL x y = zipWith(\x y -> addR x y) x y 


{-| 
    === Division of Rational string
    > divR "1/2" "3/4"
    > "2/3"
-} 
divR::String->String->String
divR s1 s2 = multR s1 $ invR s2
        where
            n1 = reduce $ splitR s1
            n2 = reduce $ splitR s2

divR'::Integer->Integer->String
divR' m n = divR m' n' 
        where 
            m' = show m
            n' = show n

invR::String->String
invR s = xx 
            where
                cx = splitR s
                xx = case head cx of
                        0 -> error "denominator can not be zero"
                        _ -> (last cx') ++ "/" ++ (head cx')
                            where
                                cx' = map(\x -> show x) cx


subR::String->String->String
subR s1 s2 = addR s1 $ negR s2

negR::String->String
negR s = multR "-1" s

negList::[String]->[String]
negList cx = map(\x -> negR x) cx 

subRL::[String]->[String]->[String]
subRL cx cs = addRL cx (negList cs)

{-|
    === get the diagonal of the matrix
-}
diag1::(Num a)=>[[a]]->[a]
diag1 (cx:[]) = [head cx] 
diag1 (x:cx) = (head x):diag1(map tail cx)

-- | get upper triangle of matrix
uptri::[[a]]->[[a]]
uptri (cx:[]) = [cx] 
uptri (x:cx) = x:uptri(map tail cx)

-- | reverse words in string
reverseWord::String->String
reverseWord s = trim $ foldr(\x y-> x ++ " " ++ y) [] $ reverse $ splitRegex(mkRegex " ") s

rw = reverseWord

{-|
 === Transpose matrix
 \[
    \begin{bmatrix}
    a & b \\
    c & d \\
    \end{bmatrix} \Rightarrow 
    \begin{bmatrix}
    a & c \\
    b & d \\
    \end{bmatrix}
 \]
-}
tran::[[a]]->[[a]]
tran ([]:_) = []
tran x = (map head x):tran(map tail x) 

-- Sat Dec 15 00:46:53 2018 
-- TODO delte it, never use it
--data Vec a = Vec [String] deriving Show
--data Rat a = Rat String deriving Show
--
---- | unicode operator
---- | dot product of two vectors
--Vec u ⨂ Vec v = Rat $ normR u v 

---- | addition of two vectors
--Vec u ⨁ Vec v = Vec $ addRL u v 
---- | subtraction of two vectors
--Vec u ⦵	Vec v = Vec $ subRL u v 
---- | division of two rational numbers
--Rat x ⨸	Rat y = Rat $ divR x y 
-- | concat operator
-- | pp $ "n=" +: fun x
-- | (+:)::(Show a)=> String-> a -> String
-- | (+:) s a = s ++ (show a)

-- | sort row for list of list
sortRow::[[Integer]]->[[Integer]]
sortRow [] = []
sortRow (x:cx) = (sortRow [s | s <- cx, not $ moreZero s x]) ++ [x] ++ (sortRow [ s | s <- cx, moreZero s x])
        where            
            moreZero::[Integer]->[Integer]->Bool
            moreZero _ [] = False
            moreZero (x:cx) (y:cy) = if length (x:cx) /= length (y:cy) then False 
                                    else 
                                        if x == 0 && x == y then moreZero cx cy 
                                         else 
                                            (if x == 0 && y /= 0 then True else False)

{-|
    === Tranform the matrix to echolon/upper triangle form 

    * Thu Jan 10 23:29:20 2019 

    * gx <http://localhost/pdf/upperTri.pdf upperTri>

    * gf /Users/cat/myfile/bitbucket/math/upperTri.tex  
      
    * Note: the determinant is different than the original matrix

    * Thu Jan 10 23:29:27 2019 

    * Add lcm to the multiplier, make matrix dim from 6 to 13

    * TODO: fix the num overflow issue
-}
upperTri::[[Integer]]->[[Integer]]
upperTri  []         = []
upperTri  m = (head sm):(upperTri am) 
        where
            -- | Note: quicksort will not work here because it is unstable sort 
            sm = mergeSortM m 

            -- [
            --  [1,2, 3]
            --  [4,5, 6]
            --  [7,8, 9]
            -- ]
            -- first row
            -- m1 = [
            --       [1, 2, 3]
            --       [1, 2, 3]
            --      ]
            m1 = toInt $ init $ map(\x -> head sm) sm 
            -- rest of the rest
            -- m2 =  [         
            --        [4, 5, 6]
            --        [7, 8, 9]
            --       ]         
            m2 = toInt $ tail $ sm 

            -- m1 = [          m2=[         
            --       4*[1, 2, 3] - [4, 5, 6] = [0, 3, 6]
            --       7*[1, 2, 3] - [7, 8, 9] = [0, 9, 12]
            --      ]             ]         
            ma = zipWith(\rx ry -> 
                            let 
                                xx = head rx  -- head [1 2 3] => 1 
                                yy = head ry  -- head [4, 5, 6] => 4
                                lc = lcm' xx yy -- lcm 4 6 => 12
                                x' = div lc xx -- div 12 4 => 3
                                y' = div lc yy -- div 12 6 => 2
                            in 
                                if xx == 0 || yy == 0 
                                then ry 
                                else zipWith(\x y -> x'*x - y'*y) rx ry 
                        ) m1 m2
            am = map (tail) ma

            moreZero::(Num a, Eq a)=>[a]->[a]->Bool
            moreZero [] [] = False
            moreZero m1 m2 = (len $ takeWhile (== 0) m1) > (len $ takeWhile(== 0) m2)

            toInt = (map . map) toInteger
            lcm' a b = lcm (toInteger a) (toInteger b)

upperTri'::[[Rational]]->[[Rational]]
upperTri'  []         = []
upperTri'  m = (head sm):(upperTri' am) 
        where
            -- | Note: quicksort will not work here because it is unstable sort 
            sm = mergeSortM m 

            -- [
            --  [1,2, 3]
            --  [4,5, 6]
            --  [7,8, 9]
            -- ]
            -- first row
            -- m1 = [
            --       [1, 2, 3]
            --       [1, 2, 3]
            --      ]
            m1 = init $ map(\x -> head sm) sm 
            -- rest of the rest
            -- m2 =  [
            --        [4, 5, 6]
            --        [7, 8, 9]
            --       ]
            m2 = tail $ sm 
            ma = zipWith(\rx ry -> 
                            let 
                                xx = head rx  -- head [1 2 3] => 1 
                                yy = head ry  -- head [4, 5, 6] => 4
                                de = denominator
                                nu = numerator
                                xx' = div (nu xx) (de xx)
                                yy' = div (nu yy) (de yy)
                                lc = lcm xx' yy' -- lcm 4 6 => 12
                                x' = toRational $ div lc xx'   -- div 12 4 => 3
                                y' = toRational $ div lc yy'   -- div 12 6 => 2
                            in 
                                if xx == 0 || yy == 0 
                                then ry 
                                else zipWith(\x y -> x'*x - y'*y) rx ry 
                        ) m1 m2
            am = map (tail) ma

            moreZero::(Num a, Eq a)=>[a]->[a]->Bool
            moreZero [] [] = False
            moreZero m1 m2 = (len $ takeWhile (== 0) m1) > (len $ takeWhile(== 0) m2)

{-|
    === Division is painful
    @
    class (Num a)=> Fractional a where
        (/):: a -> a -> a

    divI::(Fractional a)=>Integer -> Integer -> a

    class Num a where
        fromInteger::Integer -> a

    class (Num a, Ord a)=> Real a where


    fromIntegral::(Integral a, Num b) => a -> b
    fromIntegral = fromInteger . toInteger

    class (Real a, Enum)=> Integral a where   [1]
    toInteger(Real a, Enum a)=> a -> Integer  [2]
    fromInteger::(Num a)=>Integer -> a        [3]
    
    proof:
    [1] [2] [3] =>
    fromIntegral(Num a) => Integral -> a

    @
-}
divI::(Fractional a)=>Integer->Integer->a
divI n m = (fromInteger n) / (fromInteger m) 



--divI::(Fractional a)=>Integer->Integer->a
--divI n m = (realToFrac n) / (realToFrac m) 

{-| 
    === Find the invertible matrix, return ([[]], [[]]) if the matrix is singular 
    The code does not check whether the matrix is singular or not
     
    Thu Dec 13 20:38:04 2018  
    I'm sure there is better way to it  

    >m <- randomMatrix 7 7  -- Int matrix
    >m' = (map . map) rf m  -- Double matrix
    >inverse m'
    >
    >inverse::(Integral a, Fractional a, Real a, Show a, Eq a)=>[[a]]->([[a]], [[String]])
    >inverse::(Fractional a, Show a, Eq a)=>[[a]]->([[a]], [[String]])

    TODO: Remove division 
-} 
inverse::[[Double]]->([[Double]], [[String]])
inverse m = if diag == 0 then ([[]], [[]]) else (mb', mc)
        where
            id = ident' $ length m 
            -- argumented matrix [m] ++ [id]
            argm = zipWith(\x y -> x ++ y) m id 
            -- argm = 
            -- [[1 2 3 1 0 0]]
            -- [[4 5 6 0 1 0]]
            -- [[7 8 9 0 0 1]]
            -- mt = upperTri $ (map . map) toInteger argm 
            mt = upperTri $ (map . map) round argm 
            -- mt = 
            -- [[1, 2, 3 x x x]]
            -- [[   2, 2 x x x]]
            -- [[      1 x x x]]
            -- 
            -- If diag[onal] == 0 then it is single matrix
            diag = foldl(*) 1 [head x | x <- mt] 
            ar = zipWith(\x y -> (replicate x 0) ++ y) [0..] mt 
            -- ar = 
            -- [[1 2 3 x x x]
            --  [0 2 2 x x x]
            --  [0 0 1 x x x]]
            pm = map(\x -> partList (length ar) x ) ar 
            -- pm = 
            -- [[[1 2 3] [x x x]]
            --  [[0 1 2] [x x x]]
            --  [[0 0 1] [x x x]]]
            m1 = map(\r -> head r) pm
            m2 = map(\r -> last r) pm
            -- m1 = 
            -- [[1 2 3]
            --  [0 1 2]
            --  [0 0 1]]
            -- m2 = 
            -- [[x x x]
            --  [x x x]
            --  [x x x]]
            m11= reverse $ map(\x -> reverse x) m1 
            -- m11 = 
            -- [[3 2 1]
            --  [2 1 0]
            --  [1 0 0]]
            -- [[1 0 0] 
            --  [2 1 0]
            --  [3 2 1]]
            m22= reverse $ map(\x -> reverse x) m2 
            -- m22 = 
            -- [[x x x]
            --  [x x x]
            --  [x x x]]

            m3 = zipWith(\x y -> x ++ y) m11 m22
            m4 = (map . map) fromIntegral $ upperTri m3
            --m4'= map(\r -> map(\x -> divI x   $ toInteger (head r)) r) m4
            -- Fri Dec 14 16:04:32 2018 
            -- remove the division here
            -- m4'= map(\r -> map(\x -> divI x $ head r) r) m4
            -- m4'= map(\r -> map(\x -> let h = head r in divI x h ) r) m4
            -- m4'= map(\r -> map(\x -> let h = head r in divI (toInteger x) h  ) r) m4
            m4'= map(\r -> map(\x -> let h = head r in (rf x)/(rf h) ) r) m4
            mm'= zipWith(\x y -> (replicate x 0) ++ y) [0..] m4' 
            mm = map(\x -> partList (length mm') x) mm' 
            m1'= map(\x -> head x) mm
            m2'= map(\x -> last x) mm
            ma'= map(\x -> reverse x) $ reverse m1'
            -- mb'= (map . map) fromIntegral $ map(\x -> reverse x) $ reverse m2'
            mb'= map(\x -> reverse x) $ reverse m2'

            -- Rational representation, e.g. "3/4", "3/1" 
            -- m4''= map(\r -> map(\x -> divR' x $ toInteger (head r)) r) m4
            m4''= map(\r -> map(\x -> divR' x $ head r) r) m4
            mm''= zipWith(\x y -> (replicate x "0") ++ y) [0..] m4'' 
            xm' = map(\x -> partList (length mm'') x) mm'' 
            m1''= map(\x -> head x) xm'
            m2''= map(\x -> last x) xm' 
            ma''= map(\x -> reverse x) $ reverse m1''
            mb''= map(\x -> reverse x) $ reverse m2''
            mc  = reduceForm $ map(\r -> map ratToInt r) mb''

--- | return all odd elements from list
odds::[a]->[a]
odds cx = map(\x -> snd x) $ filter(\(n, _) -> (mod n 2) == 1) $ zip [1..] cx

--- | return even elements from list
evens::[a]->[a]
evens cx = map(\x -> snd x) $ filter(\(n, _) -> (mod n 2) == 0) $ zip [1..] cx

-- TODO
--inverseR::[[Rational]]->[[Rational]]
--inverseR m = 
--

{-|
    === check whether a matrix is singular using <http://localhost/pdf/gram_schmidt.pdf QR_Decompoisition>
-}
isInver::(Fractional a, Ord a)=> [[a]] -> Bool 
isInver m = if len (filter(< 0.0001) cx) > 0 then False else True
    where
        -- compute all the dot products of [a1, a2, ..] => [a1*a1, a2*a2, ...]
        -- if any dot product of (a_k, a_k) is zero or approx zero then the matrix is singular
        cx = map(\col -> listDots col col) alist
        -- [a1, a2, ..]
        alist = rejection vFir vTai
        -- first column
        vFir = [getVec 0 m]
        -- rest of columns
        vTai = f m     
        -- function to extract all columns from matrix: 
        -- m = [[a]] => [a1, a2..] = [[[a]], [[a]], ..]
        f ma = map(\n -> getVec n ma) [1..(len ma - 1)]

{-| 
    === Multiply all the diagonal elements and check whether the product is zero or not

    Algorithm:

    1. Find the upper triangle of m

    2. Multiply all the diagonal entries

    3. If their product is NOT zero then it is __invertible__, otherwise __singular__

    @
    -- Test case:
    rm <- randomMatrix 100 100
    pp $ isInvertible rm
    @
-} 
isInvertible::[[Integer]]->Bool
isInvertible [] = False
isInvertible m = if p == 0 then False else True
            where
                mx = upperTri m
                p  = foldl(*) 1 $ map(\x -> if (length x > 0) then head x else 0 ) mx

-- | Generate n dimentional identity matrix 
ident::Integer->[[Integer]]
ident n = map(\x -> (takeN x m) ++ [1] ++ (takeN (n-1 - x) m)) [0..n-1] 
        where 
            m = repeat' n 0
            takeN::Integer->[Integer]->[Integer]
            takeN n cx = take (fromIntegral n) cx 

ident'::(Num a)=>Int->[[a]]
ident' n = map(\x -> (take x m) ++ [1] ++ (take (n-1 - x) m)) [0..n-1] 
        where 
            m = replicate n 0

identS::Integer->[[String]]
identS n = map(\x -> (takeN x m) ++ ["1"] ++ (takeN (n-1-x) m)) [0..n-1] 
        where 
           m = repeat' n "0" 
           takeN::Integer->[String]->[String]
           takeN n cx = take (fromIntegral n) cx 

-- | integer multiply integer list
-- | reverse $ 9 x [9, 8] = [8, 8, 2]                                                                                    
mlist::Integer → [Integer] → [Integer]                                                                       
mlist  n [] = []                                                                                             
mlist  n s = let s' = map(*n) $ (reverse s) ++ [0] in f' n 0 s'                                              
  where                                                                                                      
    f' _ _ [] = []                                                                                           
    f' n c (y:ys) = let (q, r) = divMod (y + c) 10 in  (inte r):(f' n (inte q) ys)                         
    inte = toInteger                                                                                        


-- | generate Int from x to y random number
drawInt::Int->Int->IO Int
drawInt x y = getStdRandom(randomR(x, y))

-- | generate Integer from x to y random number
drawInteger::Integer->Integer->IO Int
drawInteger x y = getStdRandom(randomR(fromInteger x, fromInteger y))

{-| 
    === Generate list of Double in \( x \in [0 \dots n], n = 100 \) 

    >[0.3, 0.4, 0]

    Use 'randomFrac'
-} 
randomDouble::Int->IO [Double]
randomDouble n = mapM(\x -> fmap (\x -> div' x mx) $ drawInteger (0::Integer) (toInteger x)) $ replicate n mx 
                where
                    mx = 100

{-| 
    === Generate list of Double in \( x \in [0 \dots n], n = 100 \) 

    >[0.3, 0.4, 0]
    
    Use 'randomFrac'
-} 
randomFloat::Int->IO [Float]
randomFloat n = mapM(\x -> fmap (\x -> rf $ div' x mx) $ drawInteger (0::Integer) (toInteger x)) $ replicate n mx 
                where
                    mx = 100

{-| 
    === Generate list of Fractional in \( x \in [0 \dots n], n = 100 \) 

    >[0.3, 0.4, 0]

    Num and Fractional are type class so you can NOT do [Num] or [Fractional]

    Type class in Haskell is similar like interface in Java, but there are many differences

    e.g. Integer implement Num => Integer is a concrete class
    e.g. Fractional inherits from Num => Fractional type class
    e.g. Float implement Fractional => Float is concrete class
-} 
randomFrac::(Fractional a)=>Int->IO [a]
randomFrac n = mapM(\x -> fmap (\x -> rf $ div' x mx) $ drawInteger (0::Integer) (toInteger x)) $ replicate n mx 
                where
                    mx = 100

-- | generate random of list Integer
randomList::Int->IO [Integer] 
randomList 0 = return []
randomList n = do
        r <- randomRIO (10, 400)
        rs<- randomList (n - 1)
        return (r:rs)

-- | generate m x n random matrix
randomMatrix::(Num a)=>Int->Int->IO [[a]]
randomMatrix 0 _ = return [] 
randomMatrix _ 0 = return [] 
randomMatrix m n = do
        list <- replicateM m $ randomList n  
        return $ (map . map) fromIntegral list 

-- | generate zero matrix
data Mat = Zero | Id 

-- | generate matrix from 1 to n with dimention n
-- | n = 2
-- | [[1, 2]
-- |  [3, 4]]
-- | 
geneMat1ToN::Integer->[[Integer]]
geneMat1ToN n = [ [ x + n*y  | x <- [1..n] ] | y <- [0..n-1]] 
                 
-- | generate zero or identity matrix
-- |
geneMat::Integer ->Mat ->[[Integer]]
geneMat n m = case m of
                Zero -> map(\x -> repeat' n 0) [1..n] 
                Id   -> ident n


help::IO()
help = do 
        putStrLn "------------------------------------------"
        putStrLn "run [zip]  [foo]                     => zip -r foo.zip foo"
        putStrLn "run [zip]  [foo] [foo.zip]           => zip -r foo.zip foo"
        putStrLn "run [gz]   [file.txt]                => gzip foo => foo.gz"
        putStrLn "run [tar]  [file.txt]                => tar -czvf foo.tar.gz"
        putStrLn "------------------------------------------"
        putStrLn "run uzip file.txt.zip          => unzip foo.zip foo"
        putStrLn "run ugz  file.txt.gz           => gunzip foo.gz foo"
        putStrLn "run utar file.txt.tar.gz       => file.txt"
        putStrLn "------------------------------------------"
        putStrLn "run grep pattern               => grep --color --include=\"*.java\" -Hnris pattern ."
        putStrLn "run grep \"*.hs\" pattern      => grep --color --include=\"*.hs\"   -Hnris pattern ."
        putStrLn "------------------------------------------"
        putStrLn "run find \"*.hs\"              => find -iname \"*.hs\" -print"
        putStrLn "run find a1                    => find -iname [\"*.hs\"] -print"
        putStrLn "run ssh                        => ssh-keygen -C noname"
        putStrLn "------------------------------------------"

-- | Add more doc here
cmd::[String] -> IO () 
cmd x = case x of 
        [op]     -> case op of
                         "h"  -> help

                         "k"  -> createProcess (proc "ssh-keygen" ["-C", "noname"]){ cwd = Just "." } >>= \_ -> return ()
                         "ssh" -> do
                                    print cmd
                                    (Nothing, Just hout, Nothing, ph) <- createProcess p
                                    out <- hGetContents hout 
                                    mapM_ putStrLn $ lines out
                                    where 
                                        p = (shell cmd)
                                            { std_in  = Inherit
                                            , std_out = CreatePipe
                                            , std_err = Inherit
                                            }
                                        cmd = "ssh-keygen -C noname"
                         _    -> putStrLn "Invalid option"
        [op, a1] -> case op of
                         "zip" -> createProcess (proc "/usr/bin/zip"    ["-r", (a1 ++ ".zip"), a1]){ cwd = Just "." } >>= \_ -> return () 
                         "gz"  -> createProcess (proc "/usr/bin/gzip"   [a1]){ cwd = Just "." } >>= \_ -> return () 
                         "tar" -> createProcess (proc "/usr/bin/tar"    ["-czvf", (a1 ++ ".tar.gz"), a1]){ cwd = Just "." } >>= \_ -> return () 
                         "utar"-> createProcess (proc "/usr/bin/tar"    ["-xzvf", a1]){ cwd = Just "." } >>= \_ -> return () 
                         "uzip"-> createProcess (proc "/usr/bin/unzip"  [a1]){ cwd = Just "." } >>= \_ -> return () 
                         "ugz" -> createProcess (proc "/usr/bin/gunzip" [a1]){ cwd = Just "." } >>= \_ -> return () 
                         "find"-> do
                                    print cmd
                                    (Nothing, Just hout, Nothing, ph) <- createProcess p
                                    ec <- waitForProcess ph
                                    out <- hGetContents hout 
                                    mapM_ putStrLn $ lines out
                                    where 
                                        p = (shell cmd)
                                            { std_in  = Inherit
                                            , std_out = CreatePipe
                                            , std_err = Inherit
                                            }
                                        cmd = "/usr/bin/find . -iname \"" ++ a1 ++ "\" -print"
                         "grep"-> do
                                    (Nothing, Just hout, Nothing, ph) <- createProcess p
                                    ec <- waitForProcess ph
                                    out <- hGetContents hout 
                                    mapM_ putStrLn $ lines out
                                    where 
                                        p = (shell $ "grep --color --include=\"*.hs\" -Hnris " ++ a1 ++ " . ")
                                            { std_in  = Inherit
                                            , std_out = CreatePipe
                                            , std_err = Inherit
                                            }
                         _     -> print $ "[" ++ op ++ "][" ++ a1 ++ "]"

        [op, a1, a2] -> case op of
                 "zip" -> createProcess (proc "/usr/bin/zip" ["-r", a2, a1]){ cwd = Just "." } >>= \_ -> return () 
                 "grep"-> do
                            (Nothing, Just hout, Nothing, ph) <- createProcess p
                            ec <- waitForProcess ph
                            out <- hGetContents hout 
                            mapM_ putStrLn $ lines out
                            where 
                                p = (shell $ "grep --color --include=" ++ "\"" ++ a1 ++ "\"" ++ " -Hnris " ++ a2 ++ " . ")
                                    { std_in  = Inherit
                                    , std_out = CreatePipe
                                    , std_err = Inherit
                                    }
                 _     -> print $ "[" ++ op ++ "][" ++ a1 ++ "][" ++ a2 ++ "]"

        _ -> help 


-- | Compile haskell code to $ff/mybin/myHaskell => create symbol link  $sym/sym
-- | [file:myHaskell.hs]  [sym:symbol] link name in $sym
compileHaskellToBin::String->String->IO()
compileHaskellToBin file sym = rm bin >> (run $ toBin file) >> run link >> return ()
                where
                    toBin s = "ghc -i/Users/cat/myfile/bitbucket/haskell -O2 /Users/cat/myfile/bitbucket/haskell/" ++ s ++ " -o " ++ bin
                    link    = "ln -f -s " ++ bin ++ " " ++ symbin
                    symbin  = "/Users/cat/myfile/symbin" </> sym
                    mybin   = "/Users/cat/myfile/mybin"
                    base    = foldr(++) [] $ init $ splitRegex(mkRegex "\\.") file
                    bin     = mybin </> base




-- | compare two string ignore cases 
strCompareIC::String->String->Bool
strCompareIC x y = toUpperStr x == toUpperStr y

-- | walk inside the dir
-- | if there is file, then append it to list
-- | otherwise, walking inside the dir 
-- | Pass lambda func: (FilePath -> IO [String]) as argument
-- | 
-- |        let filefunc fname = 
-- |             do if (strCompareIC (takeExtension fname) (toUpperStr ".png"))
-- |                  then return [fname]
-- |                  else return []

dirWalk :: FilePath -> (FilePath -> IO [String]) -> IO [String] 
dirWalk top filefunc = do
  isDirectory <- doesDirectoryExist top
  if isDirectory
    then 
      do
        files <- listDirectory top
        foldrM(\f d -> (dirWalk (top </> f) filefunc >>= \x -> return (x ++ d))) [] files 
    else
      filefunc top

------------------------------------------------------------------ 
-- | Try to replace as many as shell command as possible
-- | See how far I can go
-- | 
-- write many shell commands as possible
-- try to emulate shell commands 
------------------------------------------------------------------ 

ls::IO()
ls = createProcess(proc "ls"  [] ) >> return () 


-- | list file in the dir = s
-- | NOTE: Not full path
lsFile::String->IO [String]
lsFile s =  do 
            (_, hout, _, _) <- createProcess(proc "ls" [s]){std_out = CreatePipe}
            case hout of
                 Just out -> hGetContents out >>= \x -> return $ lines x 
                 Nothing -> return []

-- return full path files list
-- lsFileFull ["/dog/cat" | "."] 
lsFileFull::String->IO [String]
lsFileFull s =  do 
            cur <- getPwd
            let path = if s == "." then cur else s 
            l <- lsFile path 
            return $ map(\x -> path </> x) l 

{-| 
    === list file with regex match, see 'lsRegexFull'

    >ff <- lsRegex (getEnv j) ".java$"
    >ff <- lsRegex (getEnv j) "^try"
-} 
lsRegex::String->String->IO [String]
lsRegex s r = lsFile s >>= \f -> return $ filter(matchTest reg) f 
            where
                reg = mkRegexWithOpts r True False

{-| 
    === list full path with regex match, see 'lsRegex'

    > lsRegexFull "." "\\.hs"
    > lsRegexFull "/tmp" "\\.hs"
-} 
lsRegexFull::String->String->IO [String]
lsRegexFull s r = lsFileFull s >>= \f -> return $ filter(matchTest reg) f 
            where
                reg = mkRegexWithOpts r True False

{-| 
    === Deprecated, use 'lsRegexFull'
    === list full path with regex match

    > lsRegexFull "." "\\.hs"
    > lsRegexFull "/tmp" "\\.hs"

-} 
lsFullRegex::String->String->IO [String]
lsFullRegex s r = lsFileFull s >>= \f -> return $ filter(matchTest reg) f 
            where
                reg = mkRegexWithOpts r True False
{-| 
    === remove file only
-} 
rm::FilePath -> IO()
rm s =  doesFileExist s >>= \x -> if x then (isFile s >>= \x -> if x then removeFile s else return ()) else return ()
                

{-| 
    === remove directory recursive
    
    >rm "dir"
-} 
rmDir::FilePath->IO()
rmDir s = removeDirectoryRecursive s


--removeDirectoryRecursive s 
--isFile::FilePath->IO Bool
--isFile s =  getFileStatus s >>= \st ->return $ isRegularFile st

pwd::IO()
pwd = createProcess(proc "pwd"  [] ) >> return ()

{-| 
    === get current dir
-} 
getPwd::IO FilePath
getPwd = getCurrentDirectory

{-| 
    === change dir
-} 
cd::FilePath->IO()
cd p = setCurrentDirectory p 

{-| 
    === getEnt
-} 
en::String->IO String
en s = getEnv s 

cc::String->IO ()
cc cmd = callCommand cmd

g::IO()
g = getEnv "g" >>= \x -> print x >> return ()

{-| 
    === sleep 1 sec = 100000
-} 
sleep::Int->IO()
sleep n = threadDelay n

{-| 
    === split path

    > asplitPath "/dot/cat/"
    > ["dog", "cat"]
    >
    > asplitPath "dot/cat/"
    > ["dog", "cat"]
    >
    > asplitPath "/dot/cat"
    > ["dog", "cat"]
    >
    > asplitPath "dot/cat"
    > ["dog", "cat"]
-} 
asplitPath::FilePath -> [String]
asplitPath s =  filter( \x -> length x > 0) $ splitRegex(mkRegex "/") s

{-| 
    === splitPathA from System.FilePath.Posix.splitPath 
    
    >splitPathA "/dog/cat/"
    >["/", "dog/", "cat/"]
    >
    >splitPathA "/dog/cat"
    >["/", "dog/", "cat"]
    >
    >splitPathA "dog/cat"
    >["dog/", "cat"]
    >
    >splitPathA "/"
    >["/"]
    >
    >splitPathA "./"
    >["./"]
-} 
splitPathA::FilePath -> [FilePath]
splitPathA s = splitPath s

{-|
  === better length function
  * Convert Int to polymorphic values
  * Convert Int to Num
  * fromIntegral::(Integral a, Num b)=> a -> b
 -}
len::(Foldable t, Num b)=>t a -> b
len a = fromIntegral $ length a

take'::(Integral n)=> n -> [a] -> [a]
take' n cx = take (fromIntegral n) cx


drop'::(Integral n)=> n -> [a] -> [a]
drop' n cx = drop (fromIntegral n) cx

pathBase::FilePath -> FilePath
pathBase s = if length list > 0 then last list else [] 
            where
                list = asplitPath s
-- | 1. copy FilePath to "/tmp"
-- | 2. move the file back to dir with newName
-- | copy file and rename it in the same dir
copyRename::FilePath->String->IO()
copyRename fp newName = do
           copyFileToDir fp "/tmp" 
           mv ("/tmp" </> fname) (dir </> newName) 
           rm ("/tmp" </> fname)
           where
                fname = takeFileName fp
                dir = takeDirectory fp

-- if source is valid file and dest is valid dir
-- otherwise error
copyFileToDir::FilePath -> FilePath -> IO()
copyFileToDir s d = copyFile s (d </> pathBase s)

isFile::FilePath->IO Bool
isFile s =  getFileStatus s >>= \st ->return $ isRegularFile st

fExist::FilePath->IO Bool
fExist s = doesFileExist s

fe = fExist

-- | create empty file 
createFile::FilePath->IO()
createFile f = writeFile f [] 

-- | copy dir to other dir
copyDir::FilePath -> FilePath -> IO() 
copyDir s d = do 
                status <- getFileStatus s
                if isRegularFile status then copyFileToDir s d 
                    else do 
                            tmpList <- listDirectory s 
                            let list = map(\x -> s </> x) tmpList
                            fList <- filterM(\x -> getFileStatus x >>= \x -> return $ isRegularFile x) list 
                            dirList <- filterM(\x -> getFileStatus x >>= \x -> return $ isDirectory x) list 
                            let dList = map(\x -> pathBase x) dirList
                            let soList = asplitPath s 
                            let deList = asplitPath d 
                            let full = d </> pathBase s 
                            mkdir full
                            mapM(\x -> copyFile x (full </> (pathBase x)) ) fList 
                            mapM(\x -> copyDir x  full) dirList
                            return () 

-- | rename dir or file => dir or file 
mv::FilePath->FilePath->IO()
mv s d = isFile s >>= \x -> if x then renameFile s d else renameDirectory s d 

rename = mv

-- rename all files in p, e.g. s=XXX img.JPG => img_XXX.JPG
renameAllFile::String->String->IO()
renameAllFile p s = do 
                list <- lsFileFull p 
                mapM(\x -> mv x ((dropExtension x) ++ s ++ (takeExtension x))) list
                return () 

mvFiles = renameAllFile
mvFile = renameFile


mkdir::FilePath -> IO ()
mkdir s = createDirectoryIfMissing False s

mkdirp::FilePath->IO()
mkdirp s = createDirectoryIfMissing True s 

sfilter::String->[String]->[String]
sfilter s l = filter(matchTest $ mkRegex s) l

-- | clear terminal screen
clear = putStr "\ESC[2J"

-- | run shell cmd, send output to std_out
run'::String->IO()
run' s = createProcess(proc s  [] ) >> return ()

{-| 
    === Do not need to return IO [String] like run

    or use :! cmd in GHCi
-} 
sys::String -> IO ExitCode
sys s = system s 

{-| 
     === Run shell cmd, capture std_out

     1. Change it to System, there is issue with the function
     TODO: fix it:o
-} 
run::String->IO [String]
run cmd = do 
    (Nothing, Just hout, Nothing, ph) <- createProcess p
    -- some issue with waitForProcess
    -- it might be deadlock, e.g. run "ps aux"
    ec <- waitForProcess ph
    if (ec == ExitSuccess)  
        then hGetContents hout >>= \x -> return $ lines x
        else do 
            pp $ show ec
            exitFailure
            -- error $ "error" ++ show ec
    --mapM_ putStrLn $ lines out
    where 
        p = (shell cmd)
            { std_in  = Inherit
            , std_out = CreatePipe
            , std_err = Inherit
            }
{-|
    === Differentiate on \(f(x)\) 
    === Find the first derivative at \( x \) on function \( (a \rightarrow a)) \)
    
    > x = 1.0
    > df (\x ->x^2) x
    > 2.0

    \( f(x) = x^2 \Rightarrow f'(x) = 2x \Rightarrow f'(1.0) = 2.0  \)

    First derivative on f or slop of f at point x

    \( f'(x) = \lim_{h \rightarrow 0} \frac{f(x + h) - f(x)}{h} \)

-} 
df::(Fractional a)=>(a -> a) -> (a -> a)
df f x = ((f (x + h)) - f x)/h where h = 0.001

{-|
    === Compute the tangent equation at point \((x_0, f(x_0))\) for function \( f \)

    Find tangent equation at \( x_0 = 1.0 \) in \( f(x) = x^2 \)

    >tanglent (\x -> x^2) 0 1

    tangent equation: 

    \( f'(x) = \frac{y - y_0}{x - x_0} \)

    \( y = f'(x_0)(x - x_0) + y_0 \)

    e.g.

    \( f(x) = x^2 \)

    \( \Rightarrow f'(x_0) = 2x_0 \)

    \( f (x) = x^2 \) where \( x = x_0 \)

    \( y = 2x_0(x - x_0) + y_0 \) where \( y_0 = f (x_0) \)

    \( f' (x_0) = \frac{y - y_0}{x - x0} \)

    \( y = f'(x_0)(x - x_0) + y_0 \)
-}
tangent::(Fractional a)=>(a -> a) -> a -> a -> a
tangent f x x0 = (df f x0)*(x - x0) + (f x0)

{-| 
    Compute the tangent vector at point (x0, f x0)
-} 
tangentVec::(Fractional a)=>(a -> a) -> a -> a -> (a, a) 
tangentVec f x x0 = ve 
            where
                f' = df f 
                y' = f' x0 
                ve = (x0, y')

{-|
    === Generate prime number with Sieve Algorithm
-}
prime = sieve [2..]
    where sieve (p:xs) = p:sieve[ x | x <- xs, x `mod` p /= 0]

{-| 
    === Find root for any polynomial function

    * Example: <Users/cat/myfile/bitbucket/haskell/findRoot.hs FindRoot>

    * Partition the interval [x0, xn] into list = [x0, x1) [x1, x2) [x2, x3) ..[x(n-1), xn)
    * Concat [xn, xn] with the list since the left boundary is checked only
    * __Note__: f(xn) might be zero, we need to check the xn boundary
    * TODO: need to check f(xn), DONE, concat [xn, xn] to the list

    === Good test cases:

    \( f(x) = x^2 - 4 \quad x \in [-2, 2] \)

    \( f(x) = x^5 -4x^4 + 0.1x^3 + 4x^2 - 0.5 \quad x \in [-4, 4] \Rightarrow \) 5  solutions

    * limitation:
    * if each subinterval contains two or more values, then ONLY one value can be found
    * subinterval can be adjusted in arbitrary small 

    > [0, 2] (2-0)/2 = [0, 1, 2] = [0, 1) ∪ [1, 2) ∪ [2, 2]
-} 
oneRoot::(Double->Double)->Double->Double->Double->Maybe Double 
oneRoot f x0 x1 ε   | abs(f x0)<= ε = Just x0 
                    | abs(f m) <= ε = Just m
                    | f x0 < 0 && f x1 > 0 && f m < 0 = oneRoot f m x1 ε 
                    | f x0 < 0 && f x1 > 0 && f m > 0 = oneRoot f x0 m ε 
                    | f x0 > 0 && f x1 < 0 && f m < 0 = oneRoot f x0 m ε 
                    | f x0 > 0 && f x1 < 0 && f m > 0 = oneRoot f m x1 ε 
                    | otherwise                       = Nothing 
                    where
                        m = (x0 + x1)/2

-- | Find all the roots for a given close interval: [1, 2], 1 or 2 might be the root
-- |
rootList::(Double->Double)->Double->Double->Double->Integer->[Maybe Double] 
rootList f x0 x1 ε n = filter(isJust) $ map(\(t0, t1) -> oneRoot f t0 t1 ε) full 
                where
                    n'   = fromInteger n 
                    list = map(\k ->delt*k + x0) [0..n'] 
                    inte = zipWith(\x y -> (x, y)) (init list) (tail list) 
                    full = inte ++ [(x1, x1)] -- make sure the left boundary is checked
                    delt = abs(x1 - x0)/ n'


-- | Sun Oct 21 00:00:05 2018 
-- | outer product = col ⊕ row

--------------------------------------------------------------------------------- 
outer::(Num a)=>[a]->[a]->[[a]]
outer s1 s2 = map(\x1 -> map(\x2 -> x1*x2) s2) s1

outerStr::(a -> a-> a)->[[a]]->[[a]]->[[a]]
outerStr f v r = [ map(\rr -> f (head v') rr ) r' | v' <- v, r' <- r]


vcol::[[a]] -> Int -> [[a]]  
vcol m n = tran $ vrow (tran m) n

vrow::[[a]] -> Int -> [[a]]
vrow m n = drop (n - 1) $ take n m

-- sum matrix
-- matSum::(Num a)=>[[a]]->[[a]]->[[a]]
-- matSum m1 m2 = zipWith(\x y -> zipWith(\x1 y1 -> x1+y1) x y) m1 m2 

-- zipWith in two dimensions
zipWith2::(Num a)=>(a ->a ->a)->[[a]]->[[a]]->[[a]]
zipWith2 f m1 m2 = zipWith(\x y -> zipWith(\x1 y1 -> f x1 y1) x y) m1 m2 


data XNode = XNode (M.HashMap Char XNode)  Bool deriving(Eq, Show)
{-| 
    === insert operation for Tries data structure

    >let xn = insertTries "a" (XNode M.empty False)
    >let xxn = insertTries"ab" xn
    >pp $ "containsTries=" <<< (containsTries "a" xxn == True)
    >pp $ "containsTries=" <<< (containsTries "ab" xxn == True)

    1. If String is empty return XNode is the end of a word
    2. If x points to Nothing(x is not on the Tries), then recur to cx 
    3. If x points to an XNode, then recur to cx
-} 
insertTries::String -> XNode -> XNode
insertTries [] (XNode m _) = XNode m True  -- it is a word
insertTries (x:cx) (XNode m b) = case xn of 
                                 Nothing  -> XNode (M.insert x (insertTries cx (XNode M.empty False)) m) b 
                                 Just xn' -> XNode (M.insert x (insertTries cx xn') m) b 
                where
                    xn = M.lookup x m 

{-| 
    === Insert list of strings to Tries, see 'insertTries'
-} 
insertTriesList::[String] -> XNode -> XNode
insertTriesList [] n = n
insertTriesList (x:cx) n = insertTriesList cx (insertTries x n)

{-| 
    === contain operation for Tries data structure

    >let xn = insertTries "a" (XNode M.empty False)
    >let xxn = insertTries"ab" xn
    >pp $ "containsTries=" <<< (containsTries "a" xxn == True)
    >pp $ "containsTries=" <<< (containsTries "ab" xxn == True)
-} 
containsTries::String -> XNode -> Bool
containsTries [] (XNode m b) = True 
containsTries (x:cx) (XNode m b) = case xn of 
                                   Nothing -> False
                                   Just xn' -> containsTries cx xn'
                where
                    xn = M.lookup x m


-- | -------------------------------------------------------------------------------- 
-- | Thu Nov  8 21:24:37 2018 
-- | Most functions are relative Binary Search Tree
-- | -------------------------------------------------------------------------------- 
data Tree a = Empty 
            | Node a (Tree a) (Tree a) deriving (Show, Eq)

insertNode::(Ord a)=>Tree a -> a -> Tree a
insertNode Empty a = Node a Empty Empty 
insertNode (Node a left right) b = if b < a then (Node a (insertNode left b) right) else (Node a left (insertNode right b))

insertFromList::(Ord a)=>Tree a ->[a]->Tree a
insertFromList Empty [] = Empty
insertFromList (Node a l r) [] = Node a l r
insertFromList Empty (x:xs) = insertFromList (insertNode Empty x) xs 
insertFromList (Node a l r) (x:xs) = insertFromList (insertNode (Node a l r) x) xs


inorder::Tree a->[a]
inorder Empty = []
inorder (Node a l r) = (inorder l) ++ [a] ++ (inorder r) 

maxlen::Tree a->Integer
maxlen Empty = 0 
maxlen (Node a l r) = 1 + max (maxlen l) (maxlen r)

-- | TODO: Use better error message
-- | better head
head'::[a]->a
head' cx = if length cx > 0 then head cx else error "list is empty"

{-| 
    === check whether a Tree is Binary tree

    == defintion of BST

    * Null is BST
    * Left subtree is BST
    * Right subtree is BST
    * minimum of left subtree is less than parent node
    * maximum of right subtree is greater than parent node
-} 
isBST::(Ord a)=>Tree a -> Bool
isBST Empty = True
isBST (Node a l r) =   isBST l 
                    && isBST r 
                    && (if l /= Empty then max l < a else True) 
                    && (if r /= Empty then min r > a else True)   
            where
                min::(Ord a)=>Tree a -> a 
                min Empty                = error "min error"
                min (Node a Empty Empty) = a
                min (Node a l _)         = min l

                max::(Ord a)=>Tree a -> a 
                max Empty                = error "max error"
                max (Node a Empty Empty) = a
                max (Node a _ r)         = max r

{-| 
    === Binary tree insection
-} 
binsert::Tree Integer->Tree Integer->Tree Integer 
binsert Empty (Node a Empty Empty) = (Node a Empty Empty)
binsert (Node a l r) (Node b Empty Empty) = if b < a 
                                            then Node a (binsert l (Node b Empty Empty)) r 
                                            else Node a l (binsert r (Node b Empty Empty))   


sym::Tree a ->Bool
sym Empty = True
sym (Node a Empty Empty) = True
sym (Node a l Empty) = False 
sym (Node a Empty r) = False 
sym (Node a l r) = sym l && sym r 
                
{-| 
    === Lease common ancestor

    * assume two nodes are in the tree 
    * if two nodes are in the same path then the top node will be LCA 
-} 
lca::(Eq a)=>Tree a -> a -> a -> Maybe a 
lca Empty _ _ = Nothing 
lca (Node a l r) x y = if a == x || a == y then Just a else 
                            let left = lca l x y; right = lca r x y
                                in if left /= Nothing && right /= Nothing then Just a
                                    else if left == Nothing then right else left 


           
{-| 
    === Build binary tree from preorder and inorder
-} 
buildTree::[Char]->[Char]->Tree Char 
buildTree _ [] = Empty
buildTree [] _ = Empty
buildTree preorder inorder = Node h  (buildTree leftPre leftIn) (buildTree rightPre  rightIn) 
                            where
                                h = head preorder
                                leftIn  = filter(\x->x < h) inorder
                                rightIn = filter(\x->x > h) inorder
                                leftPre = take (length rightIn) $ tail preorder 
                                rightPre = subList preorder (length leftIn) $ length preorder

                                -- ['a', 'b', 'c', 'd']
                                -- subList s 1 2 => 'b'
                                subList::[a]->Int->Int->[a]
                                subList [] _ _ = [] 
                                subList _  m n | m >= n = []
                                subList (x:xs) m n 
                                                 | m > 0 = subList xs (m-1) (n-1) 
                                                 | m == 0 = take (n) (x:xs) 



{-| 
    === Find all anagrams from a list of strings

    >anagram "dog" ["god", "cat", "ogd"] 
    >["god", "ogd"]
-} 
anagram::String->[String] -> [String]
anagram s cx = let mlist = M.lookup (sort s) map in if mlist == Nothing then [] else fromJust mlist 
    where
        map = insertMap M.empty cx 

        sort = Data.List.sort
        insertMap::M.HashMap String [String] -> [String] -> M.HashMap String [String]
        insertMap m [] = m
        insertMap m (x:cx) = insertMap (insertStr m x) cx
                where 
                    insertStr::M.HashMap String [String] -> String -> M.HashMap String [String]
                    insertStr m s = nmap 
                        where
                            nmap = let s' = sort s; v = M.lookup s' m 
                                    in if v == Nothing then M.insert s' [s] m 
                                                       else let k = fromJust v in M.insert s' (s:k) m 

{-| 
   Html textarea
   textArea row col string
   textArea 4 5 "dog" 
-} 
textArea::Integer -> Integer -> String-> String
textArea r c s = topen ++ row ++ col ++ bclose ++ s ++ tclose 
    where
        topen = "<textarea"
        bclose = ">"
        row = sp ++ "rows=\"" ++ (show r) ++ "\""
        col = sp ++ "cols=\"" ++ (show c) ++ "\""
        tclose = "</textarea>"
        sp = " "
