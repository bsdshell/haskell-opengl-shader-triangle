module AronSimple where

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Monad
import qualified Control.Monad.IO.Class as CM
import Control.Concurrent
import Data.Array.IO
import Data.Char 
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Ratio
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Time.Clock.POSIX
import Data.Foldable (foldrM)
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import Data.Typeable 
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Posix.Types
import System.Process
import System.Random
import Text.Read (Read)
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Text.Printf
import Debug.Trace (trace)
import Text.Pretty.Simple (pPrint)

import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as M
import qualified Text.Regex.TDFA     as TD
import qualified Text.Regex          as TR
import qualified Data.Set            as DS
import qualified Data.Word           as DW 

import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.Char8 as LC8 
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Internal as IN (ByteString)
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.ByteString           as BS
import qualified Data.Text.Lazy            as TL
import qualified Data.Text                 as TS
import qualified Data.Text.IO              as TSO
import qualified Data.Text.Encoding        as TSE
import qualified Data.Text.Lazy.Encoding   as TLE
import qualified Data.ByteString.Char8     as S8 (putStrLn, putStr, lines)   -- strict ?
import qualified Data.ByteString.Internal  as BSI (c2w) 
import qualified Data.Array as DR
import qualified Turtle as TUR -- (empty, shellStrictWithErr, ExitCode)
import Data.Array.IO

-- import Rainbow
-- import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)

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



{-|
    === Run shell command with Turtle lib

    @
    (e2, so, si2) <- runSh $ toSText cmd
    if e2 /= ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdout=si2}
                                  replyJson = toSBS $ DA.encode $ rcode
                              in response $ responseNothingBS replyJson
    else do
        pp so
        let replyCode = ReplyCode{rcmd="", rerror="", stdout= so}
        let replyJson = toSBS $ DA.encode $ replyCode
        response $ responseNothingBS replyJson
    @
-}
runSh :: TS.Text -> IO (ExitCode, TS.Text, TS.Text)  -- runSh TS.Text
runSh s = TUR.shellStrictWithErr s TUR.empty


-- | clear terminal screen
clear = putStr "\ESC[2J"


pp::(Show s)=>s->IO()
pp s = print s


{-|
    === Prevent shell expand argument variable, e.g '$1', '$2'

    * runRawCmd "write_to_shell" ["cat $1 $2"] => cat $1 $2
    * 'waitForProcess' to finish or 'exitFailure'
-}
--runRawCmd::String->[String]->IO()
--runRawCmd s cx = createProcess(proc s cx) >> return ()
runRawCmd::String ->[String] ->IO[String]
runRawCmd cmd cx = do
            (Nothing, Just hout, Nothing, ph) <- createProcess p
            -- some issue with waitForProcess
            -- it might be deadlock, e.g. run "ps aux"
            -- man ps -x => still d't underand why the process is not terminated
            -- top command issue => process is not terminated
            ec <- waitForProcess ph
            if (ec == ExitSuccess)
                then hGetContents hout >>= \x -> return $ lines x
                else do
                    pp $ show ec
                    exitFailure
                    -- error $ "error" ++ show ec
            --mapM_ putStrLn $ lines out
            where
                p = (proc cmd cx)
                    { std_in  = Inherit
                    , std_out = CreatePipe
                    , std_err = Inherit
                    }


{-|
    === Try to replace as many as shell command as possible

    * shell ls command

    * See how far I can go
    * write many shell commands as possible
    * try to emulate shell commands

    * Sat Feb  1 23:40:55 2020
-}
ls::IO()
ls = runRawCmd "ls" [] >>= \x -> pre x
--ls::IO()
--ls = createProcess(proc "ls"  [] ) >> return ()

{-|
    === get current dir
-}
getPwd::IO FilePath
getPwd = getCurrentDirectory
                 
{-|
    === KEY: list file in the dir = s

    * Sat Feb  1 22:25:09 2020
    * FOUND ERROR: don't use the function, it creates zombie process
    * Sun Feb  2 13:12:01 2020
    * Fixed ERROR with 'runRawCmd'

    * NOTE: Not a full path
    * See 'lsFileFull' => list full path
-}
lsFile::String->IO [String]  -- list file, not a full path
lsFile p = runRawCmd "ls" [p]


{-|
    === KEY: random Integer

    * generate Int from x to y random number
-}
drawInt::Int->Int->IO Int
drawInt x y = getStdRandom(randomR(x, y))

{-|
    === KEY: list of FULL path files

    * 'lsFileFull' is NON recursive
    * See 'lsFile' or 'runRawCmd'
    * return full path files list

    > lsFileFull "."
    > lsFileFull "/dog/cat"

    * Sat Feb  1 22:25:09 2020
    * FOUND ERROR: don't use the function, it creates zombie process
    * Sat Feb  1 23:33:12 2020
    * Fixed ERROR with 'runRawCmd'

    * NOTE: list of full path
    * See 'lsFileFull' => list full path
-}
lsFileFull::String->IO [String]  -- lsFileFull "/tmp"
lsFileFull s =  do
            cur <- getPwd
            let path = if s == "." then cur else s
            l <- lsFile path
            return $ map(\x -> path </> x) l
                 
type RegexStr = String

{-|
    === KEY: list full path with regex match, see 'lsRegex', list file with filter, file filter, file path with regex

    > lsRegexFull "." "\\.hs$"
    > lsRegexFull "/tmp" "\\.hs$"
-}
lsRegexFull::String-> RegexStr ->IO [String] -- lsRegexFull "/tmp" "\\.tex$"
lsRegexFull s r = lsFileFull s >>= \f -> return $ filter(matchTest reg) f
            where
                reg = mkRegexWithOpts r True False

{-|
    === Deprecated, use 'lsRegexFull'
    === list full path with regex match

    > lsRegexFull "." "\\.hs"
    > lsRegexFull "/tmp" "\\.hs"

-}
lsFullRegex::String-> RegexStr ->IO [String]  -- lsFullRegex str regexStr  lsFullRegex "/tmp" "\\.hs$"
lsFullRegex s r = lsFileFull s >>= \f -> return $ filter(matchTest reg) f
            where
                reg = mkRegexWithOpts r True False


randomInt::Int -> Int -> IO Int
randomInt = drawInt

     
{-|
    === Better name: write a list to file

    * write string to file
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:writeFile writeFile>

    * Append string to file
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:appendFile appendFile>

-}
writeFileList::FilePath->[String]->IO()
writeFileList p list = writeFile p $ unlines list


{-|
    === Char to word8, char to int

    * readFile to ByteString

    >ls <- BS.readFile "/tmp/b.x"
    >lsBS = BS.split (c2w_ '\n') ls
-}
c2w_::Char -> DW.Word8
c2w_ = BSI.c2w

{-|
    === Convert Integer to string, int to str

    >>> intToString 123
    "123"
-}
intToString::Integer->String
intToString n = show n

{-|
    === Convert Integer to string, int to str
    >>> integerToString 123
    "123"
-}
integerToString::Integer -> String
integerToString n = show n

{-|
    === Convert Integer to Int

    Use 'fromInteger'
-}
integerToInt::Integer -> Int
integerToInt = fromInteger


{-| 
    === Int to char

    See 'intToDigit'
-} 
intToChar::Int -> Char
intToChar = intToDigit

{-|
    === Extract a string from a string

    See 'extract' or 'Extract'

    > class Extract source where
    >   before :: Int -> source -> source
    >   after  :: Int -> source -> source
    >   extract:: (Int, Int) -> source -> source
    >
    > instance Extract String where
    >   extract :: (Int, Int) -> String -> String
    >
    > extractStr::(index, len) -> String  -> String

-}
extractStr::(Integer, Integer) -> String -> String -- extractStr (1, 2) "abc" == "bc", extractStr (1, 0) "abc" == ""
extractStr (a, b) s = extract (integerToInt a, integerToInt b) s

integerToBinary::Integer -> String
integerToBinary 0 = "0" 
integerToBinary n =  reverse $ init $ g n 
    where
        g::Integer -> String
        g 0 = "0"
        g n = let (q, r) = divMod n 2 in (integerToChar r):(g q)


integerToHex::Integer -> String
integerToHex 0 = "0" 
integerToHex n =  reverse $ init $ g n 
    where
        g::Integer -> String
        g 0 = "0"
        g n = let (q, r) = divMod n 16 in case (M.lookup r m3) of
                                                    Just x  -> x:(g q)
                                                    Nothing -> error "error" 
            where
                ls = [(10, 'A'), (11, 'B'), (12, 'C'), (13, 'D'), (14, 'E'), (15, 'F')]
                m1 = map(\x -> (x, integerToChar x)) [0..9]
                m2 = m1 ++ ls
                m3 = M.fromList m2 
        


charToInt::Char -> Int 
charToInt c = fromEnum $ (c2w_ c) - (c2w_ '0')

charToInteger::Char -> Integer
charToInteger c = toInteger $ charToInt c

integerToChar::Integer -> Char
integerToChar n = intToChar $ fromInteger n



{-|
    === Convert string to Integer, str to int, str to num, string to num

    * 'strToInteger'

    >stringToInteger "123"
    >123
    >
    >stringToInteger "a"
    >error

    * The Read class
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Read.html#v:readPrec Text.Read>
-}
stringToInteger::String->Integer
stringToInteger s = read s::Integer

{-|
    === String to Integer

    alias of 'stringToInteger'
-}
strToInteger::String -> Integer
strToInteger = stringToInteger

{-|
    === KEY: Convert string to Integer, str to int, str to num, string to num, string to int

    * 'strToInt'

    >stringToInt "123"
    >123
    >
    >stringToInt "a"
    >error

    * 'Read' is a typeclass all the instance need to implement following method
    @
        strToInteger :: String -> Integer
        strToInteger s = foldr (+) zipWith(\x y -> (digitToInt x) * 10^10) (reverse s) [0..]

        class Read a where
            read :: String -> a

        instance Read Integer where
            read s = strToInteger s
    @

    > read "123"::Int

    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Read.html#v:readPrec Text.Read>
-}
stringToInt::String->Int
stringToInt s = read s::Int

{-|
    === String to Int, str to Int

    Same as 'stringToInt'
-}
strToInt::String -> Int
strToInt = stringToInt
{-|
    === pretty print

    * print tuple, print list, print two dimensions list
    * print record
    * print list of tuple
    * use pPrint

    > [("a", "b"), ("c", "d")]

    'Text.Pretty.Simple'
-}
pre::(CM.MonadIO m, Show a) => a -> m ()
pre s = pPrint s


{-|
    === use 'latin1' encoding to avoid error when reading non ASCII characters
-}
readFileLatin1ToList::FilePath -> IO [String] -- use 'latin1' encoding to avoid error when reading non ASCII characters
readFileLatin1ToList p = do
            h <- openFile p ReadMode
            hSetEncoding h latin1
            contents <- hGetContents h
            return $ lines contents

readFileList::FilePath -> IO [String] -- readFileList "/tmp"  , save as readFileLatin1ToList
readFileList = readFileLatin1ToList

{-|
    === read file data, covert each line to ['Double'] => [['Double']]

    /tmp/x.x
    3.14 2.0
    1.0  2.0

    >ls <- readFileDouble "/tmp/x.x"
    >[[3.14, 2.0], [1.0, 2.0]]
-}
readFileDouble::FilePath -> IO [[Double]] -- "3.14 2.0" => [[3.14, 2.0]]
readFileDouble fp = do
  ls <- readFileList fp
  let lss = map(\x -> splitStr " " x) ls
  let dls = (map . map)(\x -> (read x)::Double) lss
  return dls

readFileFloat::FilePath -> IO [[Float]]
readFileFloat fp = do
  ls <- readFileList fp
  let lss = map(\r -> splitStr " " r) ls
  let dls = (map . map)(\x -> (read x)::Float) lss
  return dls
  
    
{-|
    >(round . (* 10^12)) <$> getPOSIXTime
-}
timeNowPico::IO Integer
timeNowPico = (round . (* 10^12)) <$> getPOSIXTime

{-|
    >(round . (* 10^9)) <$> getPOSIXTime
-}
timeNowNano::IO Integer
timeNowNano = (round . (* 10^9)) <$> getPOSIXTime

{-|
    >(round . (* 10^6)) <$> getPOSIXTime
-}
timeNowMicro::IO Integer
timeNowMicro = (round . (* 10^6)) <$> getPOSIXTime

{-|
    >(round . (* 10^3)) <$> getPOSIXTime
-}
timeNowMilli::IO Integer
timeNowMilli = (round . (* 10^3)) <$> getPOSIXTime

{-|
    >(round . (* 1)) <$> getPOSIXTime
-}
timeNowSecond::IO Integer
timeNowSecond = (round . (* 1)) <$> getPOSIXTime

{-| 
    === get local time with TimeZone

    > getLocalTime
    2020-07-08 12:14:46.10179

    'LocalTime'

    >utcTime <- getCurrentTime
    >z <- getCurrentTimeZone
    >let utc = utcToLocalTime z utcTime
    >return utc
-} 
getLocalTime::IO LocalTime -- 2020-07-08 12:14:46.10179
getLocalTime = do
   utcTime <- getCurrentTime
   z <- getCurrentTimeZone
   let utc = utcToLocalTime z utcTime
   return utc

{-|
    === KEY: get local date, get current time

    >"2019-05-27 12:57:41.520758 PDT"
-}
getLocalDate::IO String
getLocalDate = do
               ct <- getCurrentTime
               tz <- getTimeZone ct
               let localDate = utcToZonedTime tz ct
               return $ show localDate


{-|
    === KEY: get local current time, local time, time zone
    __NOTE__ 'getCurrentTime' is UTC timezone only,

    'getTimeDay' time with day

    > return $ (show hour) ++ ":" ++ (show minute) ++ ":" ++ (show second)
-}
getTime::IO String
getTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    -- Note: Second is of type @Pico@: It contains a fractional part.
    -- Use @fromIntegral@ to convert it to a plain integer.
    return $ (show hour) ++ ":" ++ (show minute) ++ ":" ++ (show second)




{-|
    === Trim, remove whitespace characters from either side of string.

    see 'trimWS' all whitespace
-}
trim::String->String -- trim ws from both sides
trim s  = TS.unpack $ TS.strip $ TS.pack s

{-|
    === KEY: split string, split str

    > splitStr "::" "dog::cat" => ["dog", "cat"]
-}
splitStr::RegexStr -> String -> [String]  -- splitStr "::" "dog::cat" => ["dog", "cat"]
splitStr r s = splitRegex (mkRegex r) s

{-|
    === Partition string to [String] according to character class []

    @
    splitStrChar "[,.]" "dog,cat,cow.fox" => ["dog", "cat", "cow", "fox"]y
    splitStrChar::String->String->[String]
    splitStrChar r s = splitWhen(\x -> matchTest rex (x:[])) s
                where
                    rex = mkRegex r
    @

    * See 'splitStrCharTrim'

    >splitStrRegex => splitStrChar
-}
splitStrChar::RegexStr -> String -> [String]  -- splitStrChar "[,.]" "dog,cat,cow.fox" => ["dog", "cat", "cow", "fox"]
splitStrChar r s = splitWhen(\x -> matchTest rex (x:[])) s
                where
                    rex = mkRegex r

{-|
    === Split String. 'trim' and Remove empty String
    @
    splitStrCharTrim "[,.]" " dog,fox. " => ["dog", "fox"]
    @

    * See 'splitStrChar'
-}
splitStrCharTrim::RegexStr -> String ->[String]
splitStrCharTrim r s = filter (\x -> len x > 0) $ map trim $ splitWhen(\x -> matchTest rex (x:[])) s
                where
                  rex = mkRegex r


{-|
    === Match all pat from a given str
-}
matchAllBS::BS.ByteString -> BS.ByteString -> [(MatchOffset, MatchLength)]
matchAllBS pat str = join $ fmap DR.elems $ matchAll (makeRegex pat :: Regex) str

{-|
    === Better length function

    * Convert Int to polymorphic values
    * Convert Int to Num
    * fromIntegral::(Integral a, Num b)=> a -> b
 -}
len::(Foldable t, Num b)=>t a -> b
len a = fromIntegral $ length a

{-|
    === split key and value

    >splitStrTuple "="  "host = localhost" => (host, localhost)
    * TODO: fix the function if  host = dog = cat => ("host", "dogcat")
-}
splitStrTuple::String -> String -> (String, String)
splitStrTuple p s = (trim $ head $ splitStr p s, trim $ last $ splitStr p s)

