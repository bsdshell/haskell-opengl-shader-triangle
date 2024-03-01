{-# OPTIONS_GHC -Wmissing-fields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- empty the map (ref HMap) 
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StrictData #-}

-- https://github.com/ndmitchell/record-dot-preprocessor#readme
-- dot operator for record
-- {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
-- {-# LANGUAGE TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| 
    The Module contains all the functions for __haskellwebapp2__

    * Use Aeson to serialize record to Json
    * Record: Person
    * Insert data to MySqlit-simple file-based database
    * Upload file to server.
    * Use Redis(memcached) to store snippet and query snippet.
    * *src/aronlib.js* is symbollink to *$b/jslib/aronjs.js*
    * All Javascript functions are in *src/aronlib.js*
    * Use 'responseJavascript' to send *src/aronlib.js* to client side.
-} 
module WaiLib where

import Data.Default
import Data.Typeable (typeOf)
import Data.Typeable 
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp (run)
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time
import Data.IORef 
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Text.RE.TDFA.String
import Network.Wai.Parse
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.ByteString.Builder (byteString, Builder)

import qualified Text.Email.Validate            as EM 
import qualified Data.Word8                     as DW
import qualified Data.Text                      as TS               -- strict Text         
import qualified Data.Text.Lazy                 as DL
import qualified Data.Text.Lazy.IO              as LIO
import qualified Data.Text.IO                   as TIO 

import qualified Control.Concurrent             as Concurrent
import qualified Data.List                      as L
import qualified Data.HashMap.Strict            as M 
import qualified Control.Exception              as Exception
import qualified Safe

import qualified Data.ByteString.UTF8          as BU
import qualified Data.ByteString.Lazy.Internal as IN (ByteString)
import qualified Data.ByteString.Char8      as S8 (unpack,pack, putStrLn)   -- strict ?
import qualified Data.ByteString.Lazy       as LA (writeFile, fromChunks, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as LC 
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BI (c2w, w2c)

-- import PortableLines
-- import AronModule                hiding(run, cmd)
import AronModule hiding(run, cmd)
-- import HtmlForm                
import AronHtml                             as H1
import AronHtml2                            as H2 
import qualified AronModule                 as A
import qualified GenePDFHtmlLib             as PDF
-- import qualified WaiConstant                as WC 

import qualified Turtle as TUR -- (empty, shellStrictWithErr, ExitCode)
-- import Data.Text.Lazy -- lazy Text

import Network.HTTP.Types (status200)
import Network.Wai
-- import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Handler.Warp as WARP
import Network.Wai.Util
import Network.URI
import Network.HTTP.Types.Status

import Network.Wai.Handler.WebSockets (websocketsOr)
-- import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Control.Exception              as EXP
       
import Language.Haskell.Ghcid

import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

-- {-# LANGUAGE QuasiQuotes       #-}
import Text.RawString.QQ (r)         -- Need QuasiQuotes too 

-- http://hackage.haskell.org/package/neat-interpolation-0.3.2.4/docs/NeatInterpolation.html
import qualified NeatInterpolation as NI -- variable interpolation

-- remove it since there is issue to build in stack
-- copy the source code and create a module called PortableLines
-- import qualified Text.PortableLines as POR   -- (lines replace window newline '\r\n' with '\n')

import           Data.Int (Int64)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

import           GHC.Generics
import qualified Data.Aeson as DA
import Data.Aeson.Text (encodeToLazyText)
-- import Data.Aeson (ToJSON, decode, encode)

import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.ByteString           as BS
import qualified Data.Text                 as TS    -- strict Text

import qualified Data.Text.Lazy            as TL    -- lazy Text
import qualified Data.Text.Encoding        as TSE
import qualified Data.Text.Lazy.Encoding   as TLE

import qualified Data.Bifunctor            as DB


-- BEG_993 concurrency
import System.Timeout
-- import Criterion.Measurement
import System.IO.Unsafe
import System.Process
import Control.Exception
import System.IO
import System.IO.Error
import GHC.IO.Exception
import System.Exit
import Control.Concurrent.MVar
import Control.Concurrent
-- END_993 concurrency



{-| 
    KEY: Say something
    
    M-x openurl
    help: file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/jupyterlab.html
    gx /Library/WebServer/Documents/xfido/image/foldlistimage.jpg 
-} 

-- query_redis = "/Users/cat/myfile/symbin/RedisQuery "
query_redis = "RedisQuery "
eleIdCodeBlock="t"
pdfdir = "pdf"

indexEditorHTML = "src/datadir/latex/indexEditorACE/indexEditorACE.html"
indexEditorJSON = "src/datadir/latex/indexEditorACE/indexEditorACE.json"


data Block = Block{bblock::[DL.Text]} deriving (Generic, Show)
data MBlock = MBlock{mblock::[Integer]} deriving (Generic, Show)
data GeneMatrix = GeneMatrix{
                             cmd :: TS.Text,
                             ncol :: Integer,
                             nrow :: Integer 
                            } deriving (Generic, Show)

instance DA.FromJSON GeneMatrix 
instance DA.ToJSON GeneMatrix where
    toEncoding = DA.genericToEncoding DA.defaultOptions

-- create instance of FromJSon an ToJSon
data Bgcolor = Bgcolor{ colorname :: TS.Text } deriving (Generic, Show)
instance DA.FromJSON Bgcolor
instance DA.ToJSON Bgcolor where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data Textcolor = Textcolor{ textcolor :: TS.Text } deriving (Generic, Show)
instance DA.FromJSON Textcolor
instance DA.ToJSON Textcolor where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data ReplyCode = ReplyCode{ 
                            rcmd :: TS.Text,
                            rerror :: TS.Text,
                            stdout :: TS.Text 
                          } deriving (Generic, Show)

data User = User   {uid::Int64, name::TS.Text, email::TS.Text, password::TS.Text, task::TS.Text, money::Integer} deriving (Show, Eq, Read)
data Image = Image {iid::Int64, imagename::TS.Text, uid::Int64} deriving (Show, Eq, Read)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field

instance ToRow User where
  toRow (User _uid name email password task money) = toRow (name, email, password, task, money)

instance ToRow Image where
  toRow (Image _iid imagename uid) = toRow (imagename, uid)


instance DA.FromJSON ReplyCode 
instance DA.ToJSON ReplyCode where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data CompileCode = CompileCode{
                                compiler :: TS.Text,
                                option :: TS.Text,
                                code :: TS.Text 
                              } deriving (Generic, Show)

instance DA.FromJSON CompileCode 
instance DA.ToJSON CompileCode where
    toEncoding = DA.genericToEncoding DA.defaultOptions


-- Send to client => JSON [[Integer]]
data MatInt = MatInt{name::TS.Text, matrix::[[Integer]]} deriving (Generic, Show)
instance DA.FromJSON MatInt 
instance DA.ToJSON MatInt where
    toEncoding = DA.genericToEncoding DA.defaultOptions



-- Generate HTML table in Server side
-- Send to client in JSON format [[TS.Text]]
-- Client can display it on Browser
--
-- Send to client => JSON [[TS.Text]]
data HTMLTable = HTMLTable{name::TS.Text, matrix::[TS.Text]} deriving (Generic, Show)
instance DA.FromJSON HTMLTable
instance DA.ToJSON HTMLTable where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data PreColor = PreColor {color::TS.Text, background::TS.Text} deriving (Generic, Show)
instance DA.FromJSON PreColor
instance DA.ToJSON PreColor where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data UpCodeBlock = UpCodeBlock{ok::String, retcmd::String, retbegt::Integer, retendt::Integer} deriving (Generic, Show)
instance DA.FromJSON UpCodeBlock
instance DA.ToJSON UpCodeBlock where
    toEncoding = DA.genericToEncoding DA.defaultOptions

updateRetcmd::String -> UpCodeBlock -> UpCodeBlock
updateRetcmd s u = u { retcmd = s}

updateOk::String -> UpCodeBlock -> UpCodeBlock
updateOk s u = u { ok = s }
               
-- NOTE: 02-11-2020
-- Move to AronModule
-- data UpdateCodeBlock = UpdateCodeBlock{pid::Integer, newcode::String, begt::Integer, endt::Integer} deriving (Generic, Show)
-- instance DA.FromJSON UpdateCodeBlock
-- instance DA.ToJSON UpdateCodeBlock where
    -- toEncoding = DA.genericToEncoding DA.defaultOptions


instance DA.FromJSON Block 
instance DA.ToJSON Block where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = DA.genericToEncoding DA.defaultOptions


instance DA.FromJSON MBlock 
instance DA.ToJSON MBlock where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = DA.genericToEncoding DA.defaultOptions

-- | Person to Json object
data Person =
  Person 
    { personId   :: Int64
    , personName :: TS.Text
    , personAge  :: TS.Text
    } deriving (Eq,Read,Show)

-- | define record for all the code blocks
--  can not define [TS.Text] => sqlite3 does not support [TS.Text]
-- data CodeBlock = 
--    CodeBlock 
--    { codeblockId        :: Int64
--    , header    :: TS.Text
--    , codeblock :: TS.Text
--    } deriving (Eq, Read, Show)


-- instance FromRow CodeBlock where
--  fromRow = CodeBlock <$> field <*> field <*> field

-- What is 'Only'
-- https://hackage.haskell.org/package/postgresql-simple-0.4.9.0/docs/Database-PostgreSQL-Simple.html#t:ToRow
-- instance ToRow CodeBlock where
--   toRow (CodeBlock _pId pHeader pCode) = toRow (pHeader, pCode)


{-| 
    === create UserInput table in Sqlite
    * login database
    * sqite3 /Users/cat/myfile/bitbucket/testfile/userinput.db
    * cmdId = pid
    * xcmd = input command, e.g. "c ls"
-} 
data UserInput =
  UserInput 
    { cmdId :: Int64
    , xcmd :: TS.Text
    } deriving (Eq,Read,Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

instance FromRow UserInput where
  fromRow = UserInput <$> field <*> field

-- when inserting a new Person, ignore personId. SQLite will provide it for us.
instance ToRow Person where
  toRow (Person _pId pName pAge) = toRow (pAge, pName)

-- http://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html#v:toRow
instance ToRow UserInput where
  toRow (UserInput _cmdId md) = toRow (Only md)

updir = "/Users/cat/myfile/bitbucket/haskellwebapp2/uploaddir/"

hiddenLATEXCODE = "latexcode_replace314"
hiddenCOMPILESAVE = "hidden_compile_save"



dbname = "webappdb"
configFile = "./config.txt"

lookupJust s m = fromJust $ M.lookup s m

confMap::FilePath -> IO (M.HashMap String String)
confMap fp = do
  os <- getOS
  configMap <- readConfig fp
  return $ lookupJust os configMap
 where
   lookupJust s m = fromJust $ M.lookup s m

getHostName::IO String
getHostName = do
  osMap <- confMap configFile
  let host = lookupJust "host" osMap
  let portStr = lookupJust "port" osMap
  return $ host ++ ":" ++ portStr

{-|
    === KEY: get the full rootdir

    @
      "/Users/cat/myfile/bitbucket/haskellwebapp2"
      "/Users/cat/myfile/mybin/haskellwebapp2Bin"
    @
-}
getRootDirFull::IO String
getRootDirFull = do
  osMap <- confMap configFile
  home <- getEnv "HOME"
  let rootdir = lookupJust "rootdir" osMap
  return $ home </> rootdir
      
styleChar::String->String->Char->Char->String->String
styleChar l r a b s = foldr(\x' y' -> x' ++ y') [] $ map(\x -> if x == a || x == b then l ++ (x:[]) ++ r else (x:[])) s

-- | -------------------------------------------------------------------------------- 
-- | Thu Nov 15 13:18:04 2018 
-- | Simple web server for request and response
-- | Handle search code snippet
-- | -------------------------------------------------------------------------------- 
-- run.sh => ./wai 
-- ghc -i/$b/haskelllib --make wai.hs -o wai
                      
-- [file path] [desc] [image src]
href::String->String->String->String
href p n m = [r|<a href='|] <> p <> [r|'>|] <> (baseName p) <> 
             [r|<img src='|] <> m <> [r|' width="10%"  height="10%" /></a>|] 

changeSymbol::String -> String
changeSymbol str = s1 
    where
        s = splitStr "<-" str  -- ["dog", "cat"]
        ss = if takeEnd 1 s  == [""] then init s else init s  -- ["dog", "cat"]
        s1 = (concat (map(\x -> x ++ "<span style=\"color:red;\">&lt;-</span>") ss)) ++ la
        la = if takeEnd 1 s == [""] then "" else last s

-- | Convert [[String]] to ByteString
listToByteStr::[[String]]->BS.ByteString
listToByteStr s = toSBS $ L.foldr(\x y-> x ++ "<br>" ++ y) [] $ L.foldr(\x y -> x ++ ["<br>"] ++ y) [] s

cssStrong::String->String
cssStrong s = if len > 2 then "<strong>" ++ s ++ "</strong>" else s
            where 
                list = splitRegex(mkRegex ":") s
                len = length list


cssHead::[[String]]->[[String]]
cssHead = map(\x -> let 
                        len = length $ splitRegex(mkRegex ":") (head x) 
                        in if len > 2 then ("<span style=\"color:gray;\">" ++ head x ++ "</span>"): drop 1 x else x)

htmlLess::[[String]]->[[String]]
htmlLess = (map . map)(\x -> (subRegex r x) "&lt;")
        where
            r = mkRegex "<"

htmlGreater::[[String]]->[[String]]
htmlGreater = (map . map)(\x -> (subRegex r x) "&gt;")
        where
            r = mkRegex ">"

keyWord::[[String]]->[[String]]
keyWord = (map . map)(\x -> (subRegex r x) "<span style=\"color:green;\">\\0</span>")
        where
            r = mkRegex "CTRL[a-zA-Z_-]*"

-- latex: \begin{document} \end{document}
keyWord1::[[String]]->[[String]]
keyWord1 = (map . map)(\x -> (subRegex r x) "<span style=\"color:green;\">\\0</span>")
        where
            r = mkRegex "\\\\[a-zA-Z0-9]+{[^}]+}"

keyDash::[[String]]->[[String]]
keyDash = (map . map)(\x -> (subRegex r x) "<span style=\"color:red;\">\\0</span>")
        where
            r = mkRegex "[-+]{10,}"

--keySymbol1::[[String]]->[[String]]
--keySymbol1 s = (map . map)(\x -> (subRegex r x) "<span style=\"color:blue;\">\\0</span>")  s
--        where
--            r = mkRegex "=>|=="

keySymbol1::[[String]]->[[String]]
keySymbol1 = (map . map)(\x -> changeSymbol x)

--keyName::[[String]]->[[String]]
--keyName s = (map . map)(\x -> (subRegex r x) "<span style=\"color:pink; background:#CCF7F7;\">\\0</span>")  s
--        where
--            r = mkRegex "buffer|while|if|endif|Emacs|split|goto"

keyName::[[String]]->[[String]]
keyName s = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<where\>|\<let\>):?///<span style="color:blue;">${adr}</span>|]) s

specialName::[[String]]->[[String]]
specialName = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<new\>|::|\<sizeof\>):?///<span style="color:red;">${adr}</span>|])

javaClassName::[[String]]->[[String]]
javaClassName = (map . map)(\x -> x *=~/ [ed|${adr}(\<interface\>|\<abstract\>|\<implements\>|\<class\>|\< = \>):?///<span style="color:#ef82ee;">${adr}</span>|])

        -- let s1 = "mydog dog dog (dog)" ?=~/ [ed|${adr}(\<dog\>):?///< div class="dog">${adr}< /div> |]
-------------------------------------------------------------------------------- 
-- Use following package lang extension and package for word boundary 
-- search and replacement
-- {-# LANGUAGE QuasiQuotes       #-}
-- import Text.RE.TDFA.String

-- add more ClassName here
javaFunClass::[[String]]->[[String]]
javaFunClass = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\< Vector \>|\< List \>|\< Set \>|\< HashSet \>|\< HashMap \>|\< ArrayList \>|\< Integer \>|\< String \>):?///<span style="color:#218e2b;">${adr}</span>|])
    -- it is too slow [ed|${adr}(\<[A-Z][a-z_0-9]*\>):?///<span style="color:#218e2b;">${adr}</span>|]) s
-------------------------------------------------------------------------------- 
javaKeyWords::[[String]]->[[String]]
javaKeyWords = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\< abstract \>|\< assert \>|\< boolean \>|\< break \>|\< byte \>|\< case \>|\< catch \>|\< char \>|\< class \>|\< const \>|\< continue \>|\< default \>|\< do \>|\< double \>|\< else \>|\< enum \>|\< extends \>|\< final \>|\< finally \>|\< float \>|\< for \>|\< goto \>|\< if \>|\< implements \>|\< import \>|\< instanceof \>|\< int \>|\< interface \>|\< long \>|\< native \>|\< new \>|\< package \>|\< private \>|\< protected \>|\< public \>|\< return \>|\< short \>|\< static \>|\< strictfp \>|\< super \>|\< switch \>|\< synchronized \>|\< this \>|\< throw \>|\< throws \>|\< transient \>|\< try \>|\< void \>|\< volatile \>|\< while \>):?///<span style="color:#f50a93;">${adr}</span>|])

-------------------------------------------------------------------------------- 
javaCmdKeyWords::[[String]]->[[String]]
javaCmdKeyWords = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\< java \>|\< javac \>|\< javadoc \>|\< jar \>):?///<span style="color:#35A993;">${adr}</span>|])

-------------------------------------------------------------------------------- 

mysqlKeyWords::[[String]]->[[String]]
mysqlKeyWords = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\< insert \>|\< create \>|\< from \>|\< select \>|\< table \>|\< into \>):?///<span style="color:#FF69B4;">${adr}</span>|])
-------------------------------------------------------------------------------- 

-- [[:graph:]] - ASCII char excluding space
-- match URL
keyURL::[[String]]->[[String]]
keyURL = (map . map)(\x -> (subRegex r x) "<a href=\"\\1\">\\1</a>")
        where
            r = mkRegex "(https?://[[:graph:]]+)"

spChar::[[String]]->[[String]]
spChar = (map . map)(\x -> styleChar l r a b x)
        where
            l = "<span style=\"color:red;\">"
            r = "</span>"
            a = '{' 
            b = '}' 

bracketChar::[[String]]->[[String]]
bracketChar = (map . map)(\x -> styleChar l r a b x)
        where
            l = "<span style=\"color:blue;\">"
            r = "</span>"
            a = '(' 
            b = ')' 

sbChar::[[String]]->[[String]]
sbChar = (map . map)(\x -> styleChar l r a b x)
        where 
            l = "<span style=\"color:#e012cd;\">"
            r = "</span>"
            a = '[' 
            b = ']' 

-- compose all Regex subRegex
-- transform = id
transform = 
             cssHead.
             spChar.
             bracketChar.
             sbChar.
             specialName.
             javaClassName.
             javaFunClass.
             javaKeyWords.
             javaCmdKeyWords.
             mysqlKeyWords.

             keyURL.
             keyDash.
             keyName.
             (htmlLess.htmlGreater)



wsApp :: WS.ServerApp
wsApp pending = do
  forever $ do
    conn <- WS.acceptRequest pending
    msg <- WS.receiveData conn :: IO TS.Text
    putStrLn $ show msg
    putStrLn ("weApp"::String)
    WS.sendTextData conn msg
    
type Client = (Int, WS.Connection)

broadcast :: TS.Text -> [Client] -> IO ()
broadcast msg = mapM_ (flip WS.sendTextData msg) . map snd

addClient :: WS.Connection -> [Client] -> ([Client], Int)
addClient conn cs = let i = if null cs then 0 else maximum (map fst cs) + 1
                    in  ((i, conn):cs, i)

removeClient :: Int -> [Client] -> ([Client], ())
removeClient i cs = (filter (\c -> fst c /= i) cs, ())

chat :: IORef [Client] -> WS.ServerApp
chat ref pending = do
    conn <- WS.acceptRequest pending
    identifier <- atomicModifyIORef ref (addClient conn)
    flip EXP.finally (disconnect identifier) $ forever $ do
        msg <- WS.receiveData conn
        -- putStrLn msg
        putStrLn $ show msg
        conns <- readIORef ref
        broadcast msg conns
    where
    disconnect identifier = atomicModifyIORef ref (removeClient identifier)
       
{-|
     === Fake optional parameter

     > alternateLineColor2 []
     > alternateLineColor2 [("background", "green"), ("background", "cyan")]
-}                 
alternateLineColor2::[A.CSSPro] -> [[String]] -> [[String]]
alternateLineColor2 cs cx = case len cs of
                    0 -> let style  = H2.style_ [("color", "#AAAAAA")] 
                             style' = H2.style_ [("color", "white")]
                         -- in map(\row -> map(\(x, n) -> if (mod n 2) == 0 then H2.span_ style x else H2.span_ style' x ) $ zip row [0..]) cx
                         in map(\row -> zipWith (\x n -> if even 2 then H2.span_ style x else H2.span_ style' x) row [0..]) cx
                    2 -> let style1 = H2.style_ $ init cs
                             style2 = H2.style_ $ tail cs
                         -- in map(\row -> map(\(x, n) -> if (mod n 2) == 0 then H2.span_ style1 x else H2.span_ style2 x ) $ zip row [0..]) cx
                         in map(\row -> zipWith(\x n -> if even 2 then H2.span_ style1 x else H2.span_ style2 x) row [0..]) cx
                    _ -> error [r|Invalid input: => alternateLineColor[("background", "green"), ("background", "cyan")]|]
                 where
                   (+) = (++)

blockId::Integer -> String
blockId n = "t" ++ show n


{-| 
    === Hide all the data in TextArea

    @
    <form action="serverFun.hs" name="someName" method="POST">
    <textarea id="ttId" class="text" cols="86" rows ="20" name="textName"></textarea>

    <input type="submit" value="Email" class="submitButton">
    </form>

    <textarea cols="20" rows="20" id="textArea" style="display:none;font-size:18px;" class="hide"></textarea>

    <textarea autofocus="true" onfocus="textAreaAdjust(this);"></textarea>
    @

    IN USE

    We update the codeblock according to "header"(use ID?)
    

    TODO1: use Ajax to update codeblock in database
    See aronlib.js requestPreFromRedis

    @
    data CodeBlock = 
    CodeBlock 
    { id        :: Int64
    , header    :: TS.Text
    , codeblock :: TS.Text
    } deriving (Eq, Read, Show)
    @
    pid => id => from CodeBlock table from sqlite3

<input type="button" name="Add" onclick="insertCodeBlock('|]        <> (sw pid) <>[r|');"  value="myadd">


    <button onclick="insertCodeBlock('|] <> (sw pid) <> [r|');" value="myadd" id="search-button">
    <svg id="search-icon" class="search-icon" viewBox="0 0 24 24">
      <svg xmlns='http://www.w3.org/2000/svg' class='ionicon' viewBox='0 0 512 512'>
	<title>Add Circle
	</title>
	<path d='M448 256c0-106-86-192-192-192S64 150 64 256s86 192 192 192 192-86 192-192z' fill='none' stroke='currentColor' stroke-miterlimit='10' stroke-width='32'/>
	<path fill='none' stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' stroke-width='32' d='M256 176v160M336 256H176'/>
      </svg>
    </svg>
    </button>

       <input type="button" name="Delete" onclick="deleteCodeBlock('|] <> (sw pid) <>[r|');"  value="delete">
    <input type="button" name="Update" onclick="updateCodeBlock('|] <> (sw pid) <>[r|');"  value="update">

    :DATE: 26-10-2020
    :NOTE: USE IT NOW
    :FIXME: Click hide/show DOES NOT WORK, the click location is off
    :IMG: file:///Users/cat/myfile/bitbucket/image/clickhide.png
-} 
hiddenForm2::Integer -> String -> String  
hiddenForm2 pid s = [r|<form action="/update" name="Update" class="hf" id='|] <> cid "f" pid <>[r|'|] <>
    [r| method="POST"><textarea name="header" id='|] <> sw pid <> [r|' rows="20" class="hide">|] <> sw pid <> [r|</textarea>|]<> 
    [r|<textarea name="myblock" spellcheck="false" autofocus="true" onfocus="textAreaAdjust(this);" id= '|] <> eleIdCodeBlock <> (sw pid) <> [r|' |] <> 
    [r|class="hide">|]<>s<>[r|</textarea>                            
<button type="button" onclick="updateCodeBlock('|] <> sw pid <> [r|');" value="update" id="search-button">
<svg id="search-icon" class="search-icon" viewBox="0 0 24 24">
      <svg xmlns='http://www.w3.org/2000/svg' class='ionicon' viewBox='0 0 512 512'>
      <title>Arrow Up</title>
      <path fill='none' stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' stroke-width='48' d='M112 244l144-144 144 144M256 120v292'/>
      </svg>
      </svg
      </button>
<button type="button" onclick="insertCodeBlock22('|] <> (sw pid) <> [r|');" value="add" id="search-button">
    <svg id="search-icon" class="search-icon" viewBox="0 0 24 24">
      <svg xmlns='http://www.w3.org/2000/svg' class='ionicon' viewBox='0 0 512 512'>
	<title>Add Circle</title>
	<path d='M448 256c0-106-86-192-192-192S64 150 64 256s86 192 192 192 192-86 192-192z' fill='none' stroke='currentColor' stroke-miterlimit='10' stroke-width='32'/>
	<path fill='none' stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' stroke-width='32' d='M256 176v160M336 256H176'/>
      </svg>
    </svg>
    </button>
<button type="button" onclick="deleteCodeBlock('|] <> sw pid <> [r|');" value="delete" id="search-button">
<svg id="search-icon" class="search-icon" viewBox="0 0 24 24">
   <svg xmlns='http://www.w3.org/2000/svg' class='ionicon' viewBox='0 0 512 512'>
   <title>Trash</title>
   <path d='M112 112l20 320c.95 18.49 14.4 32 32 32h184c17.67 0 30.87-13.51 32-32l20-320' fill='none' stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' stroke-width='32'/>   <path stroke='currentColor' stroke-linecap='round' stroke-miterlimit='10' stroke-width='32' d='M80 112h352'/>
   <path d='M192 112V72h0a23.93 23.93 0 0124-24h80a23.93 23.93 0 0124 24h0v40M256 176v224M184 176l8 224M328 176l-8 224' fill='none' stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' stroke-width='32'/>
   </svg>
   </svg>
   </button>
   </div></form>|]
    where
      sw = show
      -- cid s n = show $ s ++ show n
      cid s n = s ++ show n



-- In Java
-- Function f = x -> x + 1
-- BiFunction f = (x, y) -> x + y
-- 
-- 
-- foldr(\x y -> [div] ++ x ++ [cdiv] ++ brr + y) (0, []) zhtml
-- 
-- The id can be used to for TextArea editor
-- e.g.
--  <TextArea onclick="editfun()" ></TextArea>
-- 
-- <script>
-- function editfun(){
-- 
-- }
-- 
-- </script>
-- 
-- See file gf: /Users/cat/myfile/bitbucket/html/showTextAreaOnClick.html
-- 
-- <div id=\"3\" style=\"kk\"> code1 </div> 
-- <div id=\"4\" style=\"kk\"> code2 </div> 
-- 
-- ([[String]] -> [[String]]) 
-- stylish allBlock
--
-- Mon Dec  2 12:55:08 2019 
-- Fixex issue inside 'stylish allBlock', apply <br> to stylish allBlock instead of zhtml
--
-- TODO1
-- foldListList ::([([String], Integer)]->[([String], Integer)])->[([String], Integer)]->String
--
foldListList::([[String]]->[[String]])->[[String]]->String
foldListList stylish allBlock = L.foldr(+)[] $ map (concatStr' []) zhtml 
               where
                -- flip does not?
                concatStr' x y  = concatStr y x
                -- Add PID in allBlock?
                code = zip ((map . map)(+ br) $ stylish allBlock) allBlock -- code => stylish code
                -- n    => [1..] 
                -- code => [(x, b)] => [([String], [String])]
                -- zhtml = [[String]]
                zhtml = zipWith(\n (x, b) ->[hiddenForm2 n (unlines b)] +
                                 [preT $ (H1.ondblclick_ $ fun "showandhide" (ts n)) + (H1.class_ $ "co" +| n) + (H1.id_ $ "c" +| n)] +
                                 [div_ ac] + x + [cdiv] + [cpre] + [toClipboard n]) [1..] code
                
                br          =  "<br>"
                cdiv        =  "</div>"
                cpre        =  "</pre>"
                preT s      =  "<pre " <> s <> " >"
                ao          =  "<"
                ac          =  ">"
                divo        =  "<div "
                div_ s      =  "<div " + s
                ts          =  intToString
                (+)         =  (++)
                (+|) s n    =  s + (ts n)
                fun s arg   =  s + "(" + arg + ")"
                -- toClipboard n    = [r|<div class="butcen"><input type="button" class="butcopy" onClick="clip(document.getElementById('|] <> "c" <> (show n) <> [r|'));" name="cp" value="copy" ></div>|]
                toClipboard n    = [r|<div class="butcen"><input type="button" class="butcopy" onClick="copyToClipboardFromTextArea('|] <> eleIdCodeBlock <> (show n) <> [r|');" name="cp" value="copy" ></div>|]
                inputNum n  = [NI.text|<div class="butcen"><input type="button" onClick="clip(document.getElementById('c${n}'));" name="cp" value="copy" ></div>|] 

                    

foldListList2::([[String]]->[[String]])->[([String], Integer)]->String
foldListList2 stylish allBlock = L.foldr(+)[]  zhtml3
               where
                -- flip does not?
                concatStr' x y  = concatStr y x
                -- Add PID in allBlock?
                allBlock' = map fst allBlock
                -- code = zip ((map . map)(\x -> x + br) $ stylish allBlock') allBlock -- code => stylish code
                code1 = zipWith (\s1 s2 -> (s1, fst s2, snd s2)) ((map . map) (\x -> x + br) $ stylish allBlock') allBlock
                -- Wed 27 May 01:28:32 2020 
                -- TODO1: Add id here to delete it
                zhtml3 = map(\(x, b, n) -> div_ "" (hiddenForm2 n (unlines b) +
                                           (
                                             H2.pre_ (concatStr [H2.ondblclick_ [fun "showandhide" (ts n)],
                                                                 " ",
                                                                 (H2.class_ ["co" ∅ 0]), " ",
                                                                 -- (H2.class_ ["co" ∅ n]), " ",
                                                                 (H2.id_ ["c" ∅ n])   -- HTML element CAN NOT have multiple id =>  <pre id='s1' id='s2'>
                                                                ] []
                                                     )
                                                    (H2.div_ [] (concatStr x [])) + 
                                                    toClipboard n
                                           ))
                            ) code1
                         
                br          =  "<br>"
                ts          =  intToString
                (+)         =  (++)
                (∅) s n     =  s + ts n               
                (+|) s n    =  s + ts n
                fun s arg   =  s + "(" + arg + ")"
                -- toClipboard n    = [r|<div class="butcen"><input type="button" class="butcopy" onClick="clip(document.getElementById('|] <> "c" <> (show n) <> [r|'));" name="cp" value="copy" ></div>|]
                toClipboard n    = [r|<div class="butcen"><input type="button" class="butcopy" onClick="copyToClipboardFromTextArea('|] <> eleIdCodeBlock <> (show n) <> [r|');" name="cp" value="copy" ></div>|]
                inputNum n  = [NI.text|<div class="butcen"><input type="button" onClick="clip(document.getElementById('c${n}'));" name="cp" value="copy" ></div>|] 
                    



-- myfun  var  = [text|dog_cat_${var}_pig|] 

myfun name = [NI.text|this_could_be_'${name}'_long_identifier|]
fun4 name = toStr [NI.text|${name}|]

foldListListTxt::[[String]]->String
foldListListTxt allBlock = L.foldr(\x y -> x ++ "\n" ++ y) []  
                           $ L.foldr(\x y -> x ++ ["\n"] ++ y) [] allBlock    -- f s => [[String]]
                           
foldListListTxt2::[([String], Integer)] -> String
foldListListTxt2 allBlock = L.foldr(\x y -> x ++ "\n" ++ y) [] $
                              L.foldr(\x y -> x ++ ["\n"] ++ y) [] allBlock'
                         where
                            allBlock' = map fst allBlock




-- /Library/WebServer/Documents/zsurface/pdf
pdfname   = "Very Important File"
img     = "img.png"
pdfPath = "/Library/WebServer/Documents/zsurface/pdf"
docRoot = "/Library/WebServer/Documents/zsurface"
doc     = ""
cmdLog  = "/Users/cat/myfile/bitbucket/testfile/waiCmdLog.txt"

currCmdFile = "/Users/cat/myfile/bitbucket/testfile/currCmd.txt"

logCurrCmd::[String] -> IO()
logCurrCmd = writeToFile currCmdFile

readCurrCmd::IO String
readCurrCmd = readFileLatin1 currCmdFile


              
type HMap2 = M.HashMap String [([String], Integer)]
          
type HMap = M.HashMap String [[String]] 

type PDFMap = M.HashMap String String

-- Response html, css, js, pdf from Server
type RespMap = M.HashMap String String

genePDF::String->IO() 
genePDF p = do 
    f <- A.lsFile p 
    -- mapM print f
    A.fl
    let list = map(\x -> href (doc </> x) pdfname img ++ "<br>")  f
    -- mapM print list
    A.writeToFile "./pdf.html" list 


{-| 
    === Main Application entry

    @
    type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    @

    * Add *src/aronlib.js* as *Javascript* library which includes all javascript functions
    * Copy to clipboard still not working so far.

    <http://localhost/html/indexWhatIdidtoday.html#orgc0b84d7 Here_is_Why>

    :NOTE: USE IT
-} 
-- app2::Ghci -> Connection -> IORef HMap2->Application
app2::Connection -> IORef HMap2 -> IORef PDFMap -> RespMap -> Application
app2 conn1 ref pdfMapRef rmap request respond = do
  let x = 100
  let s = "a"
  case pathInfo request of 
   ("test":_)       -> respond $ responseNothing "test nothing"
   ("raw":_)        -> respond plainIndex
   ("up":_)         -> respond uploadPage
   ("insertinfo":_) -> respond insertinfo
   ("listPage":_)   -> listPage conn1 request respond
   ("insertUser":_) -> respond insertUser
   ("login":_)      -> respond loginHtml
   ("genepdf":_)    -> respond $ responseGenePDFHtml conn1
   ("pdf":fn:_)     -> do
                         let fname = let p = rawPathInfo request 
                                     in last $ filter(\x -> BS.length x > 0 ) $ A.splitBS (c2w_ '/') p  
                         print fname
                         respond $ pdfSent $ strictTextToStrictByteString fn 
   ("loginCheck":_)      -> loginCheck conn1 request respond
   ("insertUserDB":_)    -> insertUserDB conn1 request respond
   ("insert":_)          -> insertDatabase conn1 request respond
   ("upload":_)          -> upload updir request respond
   ("getjson":_)         -> upload updir request respond
   ("snippet":_)         -> respond $ anyRoute2 conn1 ref request   -- anyRoute => Response, respond (Response) => IO ResponseReceived

   -- See $b/jslib/aronlib.js, send Ajax to server
   -- url = "http://localhost:8080/json"; in postMatrix.html
   -- geneRectMat::Application
   ("json":_)            -> geneRectMat request respond
   -- test json
   ("testjson":_)         -> do
                               jsonFile <- datadirFull "indexEditorACE" EJSON
                               mjson <- jsonToRecord jsonFile :: IO (Maybe EditorCode)
                               pre mjson
                               case mjson of
                                 Just record -> respond $ responseJSON record
                                 Nothing -> respond $ responseHelp
   
   ("htmltable":_)       -> geneHTMLTable request respond
   ("updatebackground":_) -> updateBackground request respond  -- change background color
   -- ("updatetextcolor":_) -> updateTextColor request respond    -- change text color

   ("editor":_)          -> respond replyEditor
   ("search":_)          -> respond searchUI
   ("wordcount":_)       -> respond replyCssButton
   ("wordcount_reply":_) -> respond wordcountReply
   -- ("matrix":_)          -> respond matrixReply
   ("matrix":_)          -> respondMatrix conn1 ref request respond
   ("htmltablecmd":_)    -> respond sendHTMLTableCmd
   ("compiler":_)        -> receiveCode request respond
   ("editcode":_)        -> respond $ responseHtml "compileCode.html"      -- haskellwebapp2/compileCode.html
   ("getcolor":_)        -> getPreFromRedis request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
   ("updatecode":_)      -> updateCodeBlock conn1 ref request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
   ("insertcode":_)      -> insertCodeBlock conn1 ref request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
   ("editordata":_)      -> receiveEditorData conn1 ref pdfMapRef request respond -- Receive Latex code from ACE editor, ace editor
   
   -- ("aceeditor":_)       -> respond $ responseHtml "indexEditorACE.html" -- Receive Latex code from ACE editor, ace editor
   --
   --                        NOTE: xfido.com/aceeditor?id=try919591
   --                        If try919591 is in the Redis db, return try919591.html else return default responseHelp
   --
   ("apijson":_)         -> let query = queryString request :: [(BS.ByteString, Maybe BS.ByteString)]
                                idParam = join $ lookup "id" query :: Maybe BS.ByteString
                            in case toStr <$> idParam of
                                 -- => try919591.html
                                 Just s  -> do
                                   redisValue <- redisGet s
                                   case redisValue of  -- indexEditorACEtry919591.html
                                        Just v -> do
                                          jsonFile <- datadirFull v EJSON
                                          jstr <- readFileStr jsonFile
                                          let decodeStr = DA.decode (toLBS jstr) :: Maybe EditorCode
                                          pre decodeStr
                                          case decodeStr of
                                            Nothing  -> return ()
                                            Just x -> logFileNoName [show x]  -- write to "/tmp/x.x"
                                          respond responseHelp
                                        _      -> respond responseHelp
                                 Nothing  -> respond responseHelp

   ("aceeditor":_)       -> let query = queryString request :: [(BS.ByteString, Maybe BS.ByteString)]
                                idParam = join $ lookup "id" query :: Maybe BS.ByteString
                            in case toStr <$> idParam of
                                 -- => try919591.html
                                 Just s  -> do
                                   redisValue <- redisGet s
                                   case redisValue of  -- indexEditorACEtry919591.html
                                     -- Just v -> respond $ responseHtml $ v ++ ".html" -- Receive Latex code from ACE editor, ace editor
                                     --  src/datadir/latex/try919591/try919591.html
                                     Just v -> do
                                       -- TODO:
                                       -- load src/datadir/latex/try919591/try919591.json
                                       jsonFile <- datadirFull v EJSON
                                       jstr <- readFileStr jsonFile
                                       let decodeStr = DA.decode (toLBS jstr) :: Maybe EditorCode
                                       case decodeStr of
                                         Nothing  -> return ()
                                         (Just x) -> logFileNoName [show x]  -- write to "/tmp/x.x"
   
                                       htmlFile <- datadirFull v EHTML
                                       respond $ responseHtml htmlFile  --  src/datadir/latex/try919591/try919591.html
                                     _      -> respond responseHelp
   -- 
   --                            xfido.com/aceeditor
   --                            return default latex pdf file according to indexEditorACE.html
                                 Nothing -> do
                                              ran <- randomName
                                              let pdfName = ran ++ ".pdf"
                                              fullrootdir <- getRootDirFull
                                              let name = (dropExt "indexEditorACE.html") ++ ran ++ ".html"  -- indexEditorACEtry919591.html
                                              let fullName = fullrootdir </> name
                                              -- FIXME: 
                                              copyFile (fullrootdir </> indexEditorHTML) fullName
                                              logFileNoName [fullrootdir </> indexEditorHTML]  -- write to "/tmp/x.x"
                                              
                                              -- mayEditorCode <- jsonToRecord "/dog" :: IO (Maybe EditorCode)
                                              mayEditorCode <- jsonToRecord (fullrootdir </> indexEditorJSON) :: IO (Maybe EditorCode)
                                              let jeditorcode = case mayEditorCode of
                                                    Just x -> x
                                                    Nothing -> error "Invalid JSON file: EditorCode"
                                              let clientCode = editorcode jeditorcode
                                              fl
                                              pre clientCode
                                              fl
                                              let hiddenHtml = [r|<input  type="hidden" id='idlatex' name="myname" value="|] <> pdfName <> [r|" /> |]
                                              -- KEY: video http://xfido.com/song/haskellwebapp2_help.mp4 
                                              -- let hiddenCompileOrSave = [r|<input  type="hidden" id='hidden_compile_save' name="compilesave" value="|] <> "savepage" <> [r|" /> |]
                                              let hiddenCompileOrSave = [r|<input  type="hidden" id='compilesaveID' name="compilesave" value="|] <> "compilepage" <> [r|" /> |]
                                                                            
                                              writeFileStr "/tmp/xx.x" clientCode
                                              let str = strWithSlash $ clientCode
                                              replaceFileLineNoRegex fullName hiddenLATEXCODE clientCode

                                              -- target='_blank' => open a new tab
                                              let hiddenPDF = [r|<a href="|] <> pdfName <> [r|" target='_blank' onclick='promptPDFName()'>PDF</a> |]
                                              replaceFileListStr [("hidden123", hiddenHtml), ("hidden444", hiddenPDF), (hiddenCOMPILESAVE, hiddenCompileOrSave)] fullName
                                                    
                                              logFileNoName ["aceeditor:_ newName => " ++ fullName]  -- write to "/tmp/x.x"
                                              redisSet ran ran
                                              respond $ responseHtml fullName -- Receive Latex code from ACE editor, ace editor
                                                    
   ("commandservice":_)  -> commandService conn1 ref request respond   
   ("deletecode":_)      -> deleteCodeBlock conn1 ref request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
   []                    -> respond responseHelp
   _                     -> do
                              let pdfFile = toStr $ head $ pathInfo request
                              let mayFile = M.lookup pdfFile rmap

                              -- type RespMap = M.HashMap String String
                              -- RespMap : rmap contains all *.css *.pdf *.js *.html files
                              case mayFile of
                                   Just x -> case takeExt x of  -- MultiWayIf
                                                    var | var == ".css"  -> respond $ responseCSS x
                                                        | var == ".pdf"  -> respond $ responsePDF x
                                                        | var == ".js"   -> respond $ responseJavascript x
                                                        | var == ".html" -> respond $ responseHtml x
                                                        | otherwise      -> respond $ responseHelp
                                   _      -> do
                                               fl
                                               pre rmap
                                               fl
                                               pre mayFile
                                               -- pdfRef => M.HashMap String String
                                               --        => ("try919591" "try919591")
                                               pdfmap <- readIORef pdfMapRef
                                               let mls = M.toList pdfmap

                                               let queryId = dropExt pdfFile
                                               logFileNoName [queryId]  -- write to "/tmp/x.x"
                                               logFileNoName $ map show mls -- write to "/tmp/x.x"
                                               osMap <- confMap configFile
                                               let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
                                               -- => xfido.com/aceeditor?id=try919591
                                               --   => try919591.html
                                               -- DONE: TODO:   => try919591.pdf  => src/latex/try919591.pdf

                                               -- TODO: add following => src/latex/try919591/try919591.pdf
                                               --  => datadirlatex </> v </> v ++ ".pdf"
                                               redisValue <- redisGet queryId
                                               case redisValue of
                                                 Just v -> respond $ responsePDF $ datadirlatex </> v </> v ++ ".pdf"
                                                 _      -> respond responseHelp



resourceList::IO [(String, String)]
resourceList = do
  fullrootdir <- getRootDirFull
  osMap <- confMap configFile
  let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
  let f n = if let s1 = containStr "src/css" n
                   s2 = containStr "src/js" n
                   s3 = containStr datadirlatex n
                   s4 = containStr "ace/theme" n
                   s5 = containStr "ace/mode" n
                   s6 = containStr "ace/build" n
                   s7 = matchTest (mkRegex "/.*\\.html") n
               in s1 || s2 || s3 || s4 || s5 || s6 || s7 then return [n] else return []
  ls <- dirWalk fullrootdir f
  fl
  -- pre ls
  fl
  let matchFileExt x = elem (takeExt x) [".css", ".js", ".pdf", ".html"]
  
  let fls = map(\x -> (takeName x,  dropPath 1 $ concat $ dropWhile (not . containStr "haskellwebapp2")  $ splitPath x)) $ filter (\x -> matchFileExt x) ls
  return fls

plainIndex::Response
plainIndex = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing

pdfFile::Response
pdfFile = responseFile
    status200
    [("Content-Type", "text/html")]
    "pdf.html"
    Nothing
    
-- can not open pdf from browser
pdfSent::BS.ByteString -> Response
pdfSent fn = responseFile
    status200
    [("Content-Type", "application/pdf"),
     ("Content-Disposition", "inline;filename=" <>  fn)]
    (BU.toString $ pdfdir <> fn)
    Nothing
    where
        pdfdir = "pdf/"

insertinfo::Response
insertinfo = responseFile
    status200
    [("Content-Type", "text/html")]
    "insert.html"
    Nothing

insertUser::Response
insertUser = responseFile
    status200
    [("Content-Type", "text/html")]
    "insertUser.html"
    Nothing

loginHtml::Response
loginHtml = responseFile
    status200
    [("Content-Type", "text/html")]
    "login.html"
    Nothing

searchUI::Response
searchUI = responseFile
    status200
    [("Content-Type", "text/html")]
    "searchUI.html"
    Nothing


notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"


notFoundStr::IN.ByteString->Response
notFoundStr s = responseLBS
    status404
    [("Content-Type", "text/plain")]
    s 

-- let path = "/Users/cat/myfile/bitbucket/snippets/snippet_test.m
snippetP = "myfile/bitbucket/snippets/snippet_test.hs"

-- snippetP = "myfile/bitbucket/snippets/snippet.hs"


{-| 
    === BUG: what if two keys are the same?

    See 'insertAppend'
-} 
insertAll::[(String, [[String]])] -> HMap -> HMap
insertAll [] m = m 
insertAll (x:cx) m = insertAll cx (insertAppend (fst x) (snd x) m)

mapClear::[String] -> HMap -> HMap
mapClear cx m = foldl (flip M.delete) m cx

mapClear2::[String] -> HMap2 -> HMap2
mapClear2 cx m = foldl (flip M.delete) m cx


{-| 
    === Append value to [[String]] if there is a key in the map, otherwise just insert

    type HMap = M.HashMap String [[String]] 

    @
        hmap = M.HashMap "k" [["dog"]]
        hmap = "k" => [["dog"]]

        insertAppend "k" [["cat"]] hmap 
        hmap => "k" -> [["dog"], ["cat"]]
    @
-} 
insertAppend::String -> [[String]] -> HMap -> HMap
insertAppend k ls m = M.insert k (ls ++ rls) m
      where 
          rls = fromMaybe [] (M.lookup k m)

insertAll2::[(String, [([String], Integer)])] -> HMap2 -> HMap2
insertAll2 [] m = m 
insertAll2 (x:cx) m = insertAll2 cx (insertAppend2 (fst x) (snd x) m)
          
insertAppend2::String -> [([String], Integer)] -> HMap2 -> HMap2
insertAppend2 k ls m = M.insert k (ls ++ rls) m
      where 
          rls = fromMaybe [] (M.lookup k m)
          
          
{-| 
    === read snippet file
    __NOTE__ The code can be speed up a bit, change [String] [[String]

    >type HMap = M.HashMap String [[String]] => type HMap = M.HashMap String (Set [String])
-} 
listToPrefixMapOld::[([String], [String])] -> IORef HMap -> IO ()
listToPrefixMapOld pplist ref = do
        -- let path = "/Users/cat/myfile/bitbucket/snippets/snippet_test.hs"
        -- let path = "/Users/cat/myfile/bitbucket/snippets/snippet.hs"

        -- readSnippet::FilePath->IO [([String], [String])]
        -- pplist <- readSnippet path
        -- Data.Bifunctor.first 
        let keylist = L.map (DB.first (concatMap prefix)) pplist 
        let mymap = map(\cx -> [(x, y) | x <- fst cx, y <- [snd cx]]) keylist              
        let lmap = foldr(++) [] mymap                                                      
        let sortedList = qqsort(\x y -> f x y) lmap                                        
              where f x y = fst x > fst y                                
        let mmap = M.fromList lmap                                                         
        let group= groupBy(\x y -> f x y) sortedList                                       
              where f x y = fst x == fst y                                 
        
        --
        -- unzip::[("dog", "dogs"), ("cat", "cats")] => (["dog", "cat"], ["dogs", "cats"])
        let uzip = map(\x -> unzip x) group

        -- fix bug: unique $ snd x => remove duplicated values
        -- cause duplicated blocks: let tupleList = map(\x -> (head . fst $ x, snd x)) uzip
        -- tupleList => [("haskell", [["dog", "line1"], ["cat", "line2"]])]
        -- tupleList => [(String, [[String]])
        let tupleList = map(\x -> (head . fst $ x, unique $ snd x)) uzip
        -- pre tupleList

        modifyIORef ref (insertAll tupleList)
        hmap <- readIORef ref
        fw "hmap beg" 
        -- pre hmap
        fw "hmap end" 
        return ()


{-|
    == Generate 'HMap2' from a list of codeblock

    >Data HMap  = M.HashMap String [[String]]  
    >Data HMap2 = M.HashMap String [([String], Integer)]
-}
listToPrefixMap::[([String], [String], Integer)] -> IORef HMap2 -> IO ()
listToPrefixMap pplist ref = do
        -- let path = "/Users/cat/myfile/bitbucket/snippets/snippet_test.hs"
        -- let path = "/Users/cat/myfile/bitbucket/snippets/snippet.hs"

        -- readSnippet::FilePath->IO [([String], [String])]
        -- pplist <- readSnippet path 
        let keylist = L.map(\x -> 
                                (foldr(++) [] $ L.map(\y -> prefix y) (t1 x),
                                 (t2 x, t3 x)
                                )
                                
                            ) pplist 


        let mymap = map(\cx -> [(x, y) | x <- fst cx, y <- [snd cx]]) keylist
        let lmap = foldr(++) [] mymap
        -- pre $ typeOf lmap
        -- sort x of [(x, y, n)]
        let sortedList = qqsort(\x y -> f x y) lmap                                        
              where f x y = fst x > fst y
        -- convert list [(x, y)] to map
        let mmap = M.fromList lmap                                                         
        let group= groupBy(\x y -> f x y) sortedList                                       
              where f x y = fst x == fst y                                 
        
        --
        -- unzip::[("dog", "dogs"), ("cat", "cats")] => (["dog", "cat"], ["dogs", "cats"])
        let uzip = map(\x -> unzip x) group

        -- Fixed bug: unique $ snd x => remove duplicated values
        -- cause duplicated blocks: let tupleList = map(\x -> (head . fst $ x, snd x)) uzip
        -- tupleList => [("haskell", [["dog", "line1"], ["cat", "line2"]])]
        -- tupleList => [(String, [[String]])
        let tupleList = map(\x -> (head . fst $ x, unique $ snd x)) uzip
        -- pre tupleList

        -- modifyIORef::IORef a -> (a -> a) -> IO()
        modifyIORef ref (insertAll2 tupleList)
        hmap <- readIORef ref
        -- pre hmap
        return () 
                
{-|
    * http://localhost:8000/snippet?id=keyinput
    * Conver ByteString to String or vice versa 
    * span block code: <span>text</span>
    * type in search key
    * transform a block of code => colourful block of code
    * each block of code => key -> "block of code"
    * return a block of colourful code
-} 
spanBlock::HMap->(Maybe BS.ByteString)->String
spanBlock hmap mKey = foldListList f $ case (M.lookup (toStr $ fromJust mKey) hmap) of 
                                     Just s -> s 
                                     _      -> [["span Block: nothing"]]
                                where
                                    f = transform -- f = id => if we don't have any style 
                                    
{-| 
    === Say sth 

    >spanBlockX transform hmap (Just (toSBS (drop 2 sCmd)))
-} 
spanBlockX::([[String]]->[[String]])-> HMap->(Maybe BS.ByteString)->String
spanBlockX f hmap mKey = foldListList f $ case (M.lookup (toStr $ fromJust mKey) hmap) of 
                                     Just s -> s
                                     _      -> [["span Block: nothing"]] -- Just s -> [["my cool code", "my nice code"]]
                         
spanBlockX1::([[String]]->[[String]])-> HMap2 ->(Maybe BS.ByteString)->String
spanBlockX1 f hmap mKey = foldListList2 f $ case (M.lookup (toStr $ fromJust mKey) hmap) of 
                                     Just s -> s
                                     _      -> [(["span Block: spanBlockX1 => nothing"], 0)] -- Just s -> [["my cool code", "my nice code"]]


spanBlockFunc::([[String]]->[[String]])-> [[String]]->String
spanBlockFunc f codeblock = foldListList f codeblock

                         
spanBlockXX_dep::HMap->(Maybe BS.ByteString)->String
spanBlockXX_dep hmap mKey = foldListListTxt $ case (M.lookup (toStr $ fromJust mKey) hmap) of 
                                     Just s -> s 
                                     _      -> [["spanBlockXX: nothing Txt"]]
                        
spanBlockXX2::HMap2 ->(Maybe BS.ByteString)->String
spanBlockXX2 hmap mKey = foldListListTxt2 $ case (M.lookup (toStr $ fromJust mKey) hmap) of 
                                     Just s -> s
                                     _      -> [(["spanBlockXX2: nothing Txt"], 0)]
(∘) = (++)



      
{-| 
    === user input autocomplete

    * search field, search input, search form

    <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/color change background color Javascript>

    * search form has moved to htmlBody.htmlbs

    submitUpdateBackground => aronlib.js
-} 
searchForm::String -> String  -- USE IT NOW
searchForm s = [r| 
             <div style="text-align:center;">
             <form action="/snippet" method="get" target=""> 
             <input type="text" style="font-size:18pt;height:50px;width:400px;" id="inputid" value="s van" oninput="searchChange(this);" name="id" list="autocomplete">  
                 <datalist id="autocomplete">" |] <> s <> [r| </datalist><br>  
             </form> 
             </div> 
                <div>
                
                <input id='bgidcolor' type="color" onchange="submitUpdateBackground('background-color:' + this.value)" name="colorname"
                value="#e66465">
                <label for="body_background">Background</label>
                
                <input id='textcolorid' type="color" onchange="submitUpdateTextColor('color:' + this.value)" name="textcolorname"
                value="#e66465">
                <label for="text_color">TextColor</label>
                <svg width="20" height="20">
                  <rect width="20" height="20" style="fill:rgb(10,10,255);stroke-width:3;stroke:rgb(0,0,0)" />
                </svg>
                
                <svg width="20" height="20">
                  <rect width="20" height="20" style="fill:rgb(0,33,55);stroke-width:3;stroke:rgb(0,9,20)" />
                </svg>
                </div>
             |]

-- <input type="color" onchange="changeColor(this.value)" id="colorid" name="colorname"
-- value="#e66465">
                 
htmlPre::String -> String
htmlPre s = [r| <pre style="font-size:29px;white-space: pre-wrap;" id="id00"> |] <> s <> [r| </pre> |]
  

{-|
    === htmlBody $ (searchForm s) ++ (htmlPre s1)

    * htmlBody $ searchForm listCmd $ htmlPre retStr 
    * deprecated
    * use: bs <- replaceByteStringFile "src/htmlBody.htmlbs" "replacekey00" $ toSBS (searchForm (optionHtml autoList))

    :NOTE: htmlBody.html is NOT USED so far.
-} 
htmlBodyH::String -> String
htmlBodyH s  = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE> 
            <LINK rel="stylesheet" type="text/css" href="css/mystyle.css?rnd=132">
            <LINK rel="stylesheet" type="text/css" href="css/modifycolor.css?rnd=132">
            <LINK rel="stylesheet" type="text/css" href="css/svgbutton.css?rnd=132"> 
            <script src="js/aronlib.js" defer></script>
            </HEAD>
            <BODY>|] <> s <> [r|<div id="searchdata">no data</div></BODY></HTML>|]
            -- 'searchdata' is used in aronlib.js function delayFun(value)
                  
{-|
    == User input, autocomplete, search field
-}
replyHtml::String->String->String
replyHtml s listCmd = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE>
            <LINK rel="stylesheet" type="text/css" href="css/mystyle.css"> 
            <script src="js/aronlib.js"></script>
            <!--
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            -->
            </HEAD>
            <BODY> 
            |] <> s <> [r| </BODY></HTML> |]

{-| 
    snippet?id=queryStr
    S8.unpack: ByteString to String
    type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    anyRoute => Response

    :NOTE: Use 
-}                 
anyRoute2::Connection -> IORef HMap2 -> Request-> Response
anyRoute2 conn ref req =
    -- get query from client
    -- look up the value of id, e.g. snippet?id=value
    -- Maybe s 
    -- search s from the HMap
    -- replace the format html if any value is found
    -- Otherwise, reply "nothing"
    let query = queryString req :: [(BS.ByteString, Maybe BS.ByteString)]
        idParam = join $ lookup "id" query :: Maybe BS.ByteString
    in case toStr <$> idParam of  
            -- responseBuilder :: Status -> ResponseHeaders -> Builder -> Response
            Just s -> do 
                      -- record command and write to file
                      -- store s in Redis here
                      case s of
                           var | len var > 3 -> case take 2 s of
                                 var | var == "c " -> responseCmd conn s     -- Shell commands
                                     | var == "j " -> responseJavaHtml s     -- Java AronLib.java with Html, CSS.
                                     | var == "h " -> responseHaskellHtml s  -- Haskell AronModule.hs with HTML, CSS.
                                     | var == "k " -> queryLibHaskell s    -- Haskell AronModule.hs No HTML
                                     | var == "i " -> queryLibJavaPackage "Aron." s       -- Java $b/javalib/AronLib.java
                                     | var == "e " -> queryRedisSnippet s
                                     | var == "p " -> queryLibJavaPackage "Print." s -- Java $b/javalib/Print.java
                                     | var == "n " -> responseSnippetTxt2 s ref  -- Snippet with No HTML, CSS.
                                     | var == "s " -> responseSnippetHTML2 conn s ref -- Snippet with Html,No Search field
                                     | otherwise   -> responseNothing ""  -- responseSearch conn "search 1"
                               | otherwise   -> responseNothing "" -- responseSearch conn "search 2" 
            _      -> responseSearch conn "search 3"

    
    

-- | http://localhost:8000/up/
-- | NOTE: file => /upload dir
-- | Plz see uploadPage.html 
-- | /Users/cat/myfile/bitbucket/haskellwebapp2/uploadPage.html
-- | responseFile :: H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> Response
uploadPage::Response
uploadPage = responseFile
    status200
    [("Content-Type", "text/html")]
    "uploadPage.html"
    Nothing

data SearchType = CmdT | JavaT | HaskellT | SnippetT

readCmd::FilePath -> SearchType -> IO String
readCmd fn t = do
               -- cmdList  <- readFileToList fn
               cmdList  <- readFileLatin1ToList fn
               -- filter out "va" from list and apply f to each element
               let sortedList = groupCountFilter cmdList 
               let tupList = show <$> groupCount sortedList
               -- writeToFile "/tmp/aa.x" tupList
               -- let sortedList = ["33", "11", "22", "bb", "aa"]
               let htmlStr = concat $ map(\x -> let left = "<option value=\""; 
                                                    right = "\">" 
                                                in case t of 
                                                        CmdT     -> left ++ x ++ right
                                                        JavaT    -> left ++ x ++ right
                                                        HaskellT -> left ++ x ++ right
                                                        SnippetT -> left ++ x ++ right
                                         ) $ sortedList 
               return htmlStr 


{-| 
    === Filter some commands out from a list.
-} 
groupCountFilter::[String] -> [String]
groupCountFilter cs = fst <$> groupCount (let f::String -> Maybe String
                                              f "va" = Nothing
                                              f s    = Just s
                                          in filtermap ( f . trim) cs)
optionHtml::[String] -> String
optionHtml cs = concat $ map(\x -> [r| <option value="|] <> x <> [r|">|]) cs 

--optionHtml::[String] -> String
--optionHtml cs = htmlStr 
--            where
--               sortedList =  fst <$> groupCount (let f::String -> Maybe String
--                                                     f "va" = Nothing
--                                                     f s    = Just s
--                                                 in filtermap ( f . trim) cs)
--               htmlStr = concat $ map(\x -> [r| <option value=" |] <> x <> [r|">|])  sortedList 

responseNothing::String -> Response                                                    
responseNothing s = responseStream                                                   
              status200                                                            
              [(hContentType, "text/html")] $ \write flush -> do                   
              write $ byteString $ toSBS ("responseNothing : " ++ s)

responseNothingBS::BS.ByteString -> Response                                               
responseNothingBS bs = responseStream                                                   
              status200                                                            
              [(hContentType,  "application/json")] $ \write flush -> do                   
              write $ byteString bs

{-|
    === Response JSON, record to client side
-}
responseJSON::(DA.ToJSON a) => a -> Response
responseJSON rd = responseJSONBS $ (toSBS . DA.encode) rd
              
responseJSONBS::BS.ByteString -> Response
responseJSONBS bs = responseStream
              status200
              [(hContentType,  "application/json")] $ \write flush -> do                   
              write $ byteString bs  


responseNothingTest::Response                                               
responseNothingTest = responseStream                                                   
              status200                                                            
              [(hContentType,  "application/pdf"),
               ("Content-Disposition", "inline;filename=kkk.pdf")] $ \write flush -> do                   
              write $ byteString $ toSBS ("dog"::String)

replyTaskHtml::BS.ByteString -> BS.ByteString -> BS.ByteString 
replyTaskHtml url s = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE> 
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            </HEAD>
            <BODY> 
            <div style="text-align:center;">
            <br>
            <p> |] <> s <> [r|</p><br><a href= |] <> url <> [r|>Back</a></div></BODY></HTML> |]


            -- <p> |] <> s <> [r|</p><br><a href="http://localhost:8000">Back</a></div></BODY></HTML> |]

              
listPageHtml::BS.ByteString -> BS.ByteString 
listPageHtml s = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE> 
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            </HEAD>
            <BODY> 
            <div style="text-align:center;">
            <br>
            <p>|] <> s <> [r|</div></BODY></HTML>|]

--listPage::BS.ByteString -> Response                                               
--listPage bs = responseStream                                                   
--              status200                                                            
--              [(hContentType,  "text/html")] $ \write flush -> do                   
--              write $ byteString bs  

listPage::Connection -> Application
listPage conn req response = do
              userList <- query_ conn "SELECT uid, name, email, password, task, money FROM user" :: IO [User]
              let listTask = BS.concat $ map (\x -> [r|<div>|] <> (t2b $ task x) <> [r|</div><br>|]) userList -- => TS.Text
              -- response $ responseTaskBS (task (head userList))
              hostURL <- getHostName >>= return . toSBS
              response $ responseTaskBS $ replyTaskHtml hostURL listTask 
        where
            t2b = strictTextToStrictByteString

responseTaskBS::BS.ByteString -> Response                                               
responseTaskBS bs = responseStream                                                   
               status200                                                            
               [(hContentType,  "text/html")] $ \write flush -> do                   
               write $ byteString bs  



--writeSB::String -> Builder
--writeSB = write $ byteString $ strToStrictByteString 

  -- write $ byteString $ toSBS $ replyHtml (escapeHtml retStr) listCmd 

{-|
   === Response output from shell command
   1. The maximum number of lines are 200, " | head -200"

   2. Exit code can not be checked, ExitSuccess
   
   Wed May  8 23:10:41 2019 
   3 Error can be checked in stderr  
   >(e, so, si) <- A.runSh $ toSText (drop 2 ncmd) 
   > if si is NOT empty then there is stderr

   >type streamBody = (Builder -> IO()) -> IO() -> IO()
 -} 
responseCmd::Connection -> String -> Response
responseCmd conn cmd = responseStream 
              status200
              [(hContentType, "text/html")] $ \write flush -> do
              -- code <- A.run (cmd ++ "\n")
              -- There is problem with top 500 lines
              -- It seems to be working with 200 lines 
              let ccmd = trim cmd
              let ncmd = ccmd ++ topN
                  
              -- Log the current user input. 
              -- logCurrCmd [ccmd]
              -- runSh :: TS.Text -> IO (ExitCode, TS.Text, TS.Text)    
              (e, so, si) <- runSh $ toSText (drop 2 ncmd) 
              -- ExitCode can not capture exit code of cmd1 in "cmd1 | cmd2"
              -- Wed May  8 23:27:04 2019 
              -- Fixed Error: 
              -- If there is error, si will NOT be empty String
              -- Otherwise, there is NO error.
              let ok = isOk si 
              let shellRet = if ok then (toStr so) else ("Invalid Shell Command:" ++ ncmd)
              -- if ok then writeToFileAppend cmdLog [ccmd] else return () 
              if ok then do
                  -- NOT USED
                  sortList <- queryUserInput conn ccmd
                  -- execute_ conn "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
                  -- execute conn "INSERT INTO userinput (xcmd) VALUES (?)" (UserInput 0 (toSText ccmd))
                  -- cmdsql <- query_ conn "SELECT id, xcmd FROM userinput"::IO [UserInput]
                  -- let cmdList = map toStr (map (xcmd) cmdsql::[TS.Text]) -- [UserInput] => [Text] => [String]
                  -- pa cmdList 
                  -- let sortList = groupCountFilter cmdList
                  -- FIXME: replace htmlBody with htmlBody.htmlbs
                  -- FIXME: replace searchForm with searchForm.htmlbs
                  -- logFile2 "/tmp/bb1.txt" [htmlBodyH $ (searchForm (optionHtml sortList)) ∘ (htmlPre shellRet)]
                  -- write $ byteString $ toSBS $ htmlBodyH $ (searchForm (optionHtml sortList)) ∘ (htmlPre shellRet)
                  write $ byteString $ toSBS $ htmlPre shellRet
              else flush
        where
            topN = " | head -200"
            isOk si = (toStr si) == ""

queryUserInput::Connection -> String -> IO [String]
queryUserInput conn cmd = do
  execute_ conn "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
  execute conn "INSERT INTO userinput (xcmd) VALUES (?)" (UserInput 0 (toSText cmd))
  cmdsql <- query_ conn "SELECT id, xcmd FROM userinput"::IO [UserInput]
  let cmdList = map toStr (map (xcmd) cmdsql::[TS.Text]) -- [UserInput] => [Text] => [String]
  -- pa cmdList 
  return $ groupCountFilter cmdList
  
responseJavaHtml::String -> Response
responseJavaHtml cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd                  -- " j  list " => "j  list"
              let hKey  = trim $ drop 2 tcmd       -- "j  list"   => "list"
              let jCmd = redisKey hKey             -- "list"      => "Aron.list"
              ls <- A.run $ query_redis ++ jCmd   
              let lsEscapeHtml = map(\x -> escapeHtml x) ls
              let ls2 = map(\s -> [s]) lsEscapeHtml 
              -- let repStr = foldr(\x y -> x ++ "<br>" ++ y) [] code
              let repStr = H1.table ls2
              -- FIXME: take cmd from database
              -- writeToFileAppend cmdLog [tcmd] 
              -- listCmd <- readCmd cmdLog JavaT
              write $ byteString $ toSBS $ spanBlockFunc (alternateLineColor2 [("color", "#AAAAAA"), ("color", "white")] . transform) [ls] 
              flush
              where
                redisKey s = "Aron." ++ s
              
responseGenePDFHtml::Connection -> Response                                                 
responseGenePDFHtml conn = responseStream                                                
              status200                                                              
              [("Content-Type", "text/html")] $ \write flush -> do
              bs <- PDF.pdfMain conn "pdf/"
              write $ byteString bs  
              flush                                                                  

  
responseHaskellHtml::String -> Response
responseHaskellHtml cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              -- append "AronModule." Reddis for Haskell lib
              let tcmd = trim cmd
              let hKey = redisKey $ trim $ drop 2 tcmd  -- "h list" => AronModule.list
              code <- A.run $ query_redis ++ hKey   
              let codeEsc = map(\x -> escapeHtml x) code -- ["code"]
              let repStr = foldr(\x y -> x ++ "<br>" ++ y) [] codeEsc -- ["code']
              pre repStr
              -- writeToFileAppend cmdLog [tcmd] 
              -- listCmd <- readCmd cmdLog HaskellT 
              -- write $ byteString $ toSBS $ replyHtml repStr ""
              write $ byteString $ toSBS $ spanBlockFunc (alternateLineColor2 [("color", "#AAAAAA"), ("color", "white")] . transform) [code]
              flush
              where
                redisKey s = "AronModule." ++ s
              
queryRedisSnippet::String -> Response
queryRedisSnippet cmd = responseStream
                        status200
                        [("Content-Type", "text/html")] $ \write flush -> do
                        let tcmd = trim cmd
                        -- AronModule. ++ "h writeToFile" => AronModule.writeToFile
                        let qstr = drop 2 tcmd -- "h list" => "list"
                        let hKey = preKey ++ (trim $ qstr)   
                        pre hKey
                        code <- A.run $ query_redis ++ hKey   
                        pre code  
                        if len code > 0 
                          -- then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
                        then let repStr = unlines code in write $ byteString $ toSBS repStr 
                        else write $ byteString emptySBS 
                        flush
                        where
                            preKey = "snippet."


{-| 
    === Run ghci_stack.sh as a process, return some info
-} 
ghcidRun::Ghci -> String -> Response
ghcidRun ghci cmd = responseStream 
        status200
        [("Content-Type", "text/html")] $ \write flush -> do
        let tcmd = drop 2 $ trim cmd
        putStrLn tcmd
        putStrLn cmd
        -- exec :: Ghci -> String -> IO [String]
        let executeStatement = exec ghci 
        sbs <- getGhciInfo tcmd executeStatement
        write $ byteString sbs 
        flush
 where
  f::Stream -> String -> IO()
  f a b = return ()

  getGhciInfo cmd f = do
       -- f str >>= print . head
       ls <- f cmd
       pre ls
--        let str = if (len ls > 0) then (let list = map (\x -> lastNon $ filterNonEmpty $ splitRegex (mkRegex "\\.") x) ls
--                                       in unlines list
--                                      ) else ""
       return $ toSBS $ unlines ls 

  lastNon::[String] -> String
  lastNon cx = if len cx > 0 then last cx else ""

{-| 
    === query function info from redis without Html

    >query_redis = "/Users/cat/myfile/symbin/RedisQuery "

    >preKey = "AronModule."  AronModule.hs
    >preKey = "Aron."        Aron.java
-}
queryLibHaskell::String -> Response
queryLibHaskell cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd
              putStrLn cmd

              -- AronModule. ++ "h writeToFile" => AronModule.writeToFile
              let qstr = drop 2 tcmd -- "h list" => "list"
              let hKey = preKey ++ (trim $ qstr)   
              pre hKey
              code <- A.run $ query_redis ++ hKey   
              fw "beg"
              pre code
              fw "end"
              pp $ "tcmd=" <<< tcmd

              let tcode = sortedByMatchedStrIndex qstr code
              fw "tcode beg"                           
              pre tcode
              fw "tcode end"
              if len tcode > 0 
              -- then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
              then let repStr = unlines tcode in write $ byteString $ toSBS repStr 
              else write $ byteString emptySBS 
              flush
              where
                preKey = "AronModule."

                -- sorted string according to index of search term.
                -- gx file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/sorted_haskell_function.html
                sortedByMatchedStrIndex qstr code = qqsort(\a b -> let la = matchIndex qstr $ head $ splitStr "::" a 
                                                                       lb = matchIndex qstr $ head $ splitStr "::" b 
                                                                   in  la < lb ) $ map trim code

                matchIndex q s = case matchAny q s of
                                   Just x -> fst x
                                   _      -> 10 

{-|
	=== Should use 'queryLibJavaPackage'

        NOT BEEN USED
-} 
queryLibJava::String -> Response
queryLibJava cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd
              putStrLn cmd
              let hKey = preKey ++ (trim $ drop 2 tcmd)
              code <- A.run $ query_redis ++ hKey   
              let tcode = map trim code
              if len tcode > 0 
              then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
              else write $ byteString emptySBS 
              flush
              where
                preKey = "Aron."

{-|
=== Get java $jlib/AronLib.java without HTML
-} 
queryLibJavaPackage::String -> String -> Response
queryLibJavaPackage preKey cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd
              putStrLn cmd
              -- preKey = Print.  tcmd = "p list"
              -- => hKey = Print.list
              let hKey = preKey ++ (trim $ drop 2 tcmd)
              code <- A.run $ query_redis ++ hKey   
              let tcode = map trim code
              if len tcode > 0 
              then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
              else write $ byteString emptySBS 
              flush
              where
                -- preKey = "Aron."



{-| 
    === Get user input: cmd = "s java regex", autocomplete commands

   TODO1 add host name here
   name <- run "uname"
   ls <- run "uname" >>= \x -> if len x > 0 then return head x else []

   :NOTE: USE in anyRoute2

   @
   data Response
       = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
       | ResponseBuilder H.Status H.ResponseHeaders Builder
       | ResponseStream H.Status H.ResponseHeaders StreamingBody
       | ResponseRaw (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()) Response
     deriving Typeable

   responseStream::Status -> ResponseHeaders -> StreamingBody -> Response
   type StreamingBody = ( Builder -> IO() ) -> IO () -> IO ()
   @


-} 
responseSearch::Connection -> String -> Response                                                    
responseSearch conn s = responseStream                                                   
              status200  -- Status     ↓---- ResponseHeaders                                                        
              [(hContentType, "text/html")] $ \write flush -> do                   
              --                                ↑     ↑
              --                                |     --------> IO ()
              --                                --------------> Builder -> IO()
              -- create table if table: userinput does not exist
              execute_ conn sql_create_table 
              cmdsql <- query_ conn sql_select ::IO [UserInput]
              let cmdList = let ls = map (xcmd) cmdsql::[TS.Text] in map toStr ls::[String]
              let sortList = groupCountFilter cmdList
              -- limit the number of commands
              let autoList = take 20 sortList
              -- bs <- readFileRepPat "src/htmlBody.htmlbs" "replacekey00" $
              -- bs <- (readFileRepPat "src/searchForm.html" "replaceSearchForm" $ toSBS $ optionHtml autoList)
                     -- >>= readFileRepPat "src/htmlBody.html" "replacekey00" 
              -- let htmlByteStr = toSBS $ htmlBodyH  $ (searchForm (optionHtml autoList))
              -- ls <- runCmd "uname"
              -- let osName = if len ls > 0 then head ls else []
              -- <input  type='hidden' id='osid' name="myname" value="Bjner Stroustrup Cheetah Chateau">
              -- let divStr = [r|<input type='hidden' id='osid' value='|] <> osName <> [r|' />|]
              -- let divSBS = toSBS divStr
              -- let osidStr = "hiddenosid"::BS.ByteString
              -- let bb = searchReplaceAnySBS bs osidStr divSBS
                  
              rls <- runCmd "uname"
              let osName = if len rls > 0 then head rls else []
              bb <- searchMainHtml autoList osName    
              writeFileListBS "/tmp/bs1.html" [bb]
              -- write $ byteString $ BS.concat ([bs] ++ [divSBS])
              write $ byteString $ BS.concat [bb]
              -- ↑            ↑
              --              --- byteString::ByteString -> Builder
              -- write::Builder -> IO()
              -- 
          where 
            sql_create_table = "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
            sql_select = "SELECT id, xcmd FROM userinput"


searchMainHtml::[String] -> String -> IO BS.ByteString
searchMainHtml autoList osName = do
  bs <- (readFileRepPat "src/searchForm.html" "replaceSearchForm" $ toSBS $ optionHtml autoList)
                     >>= readFileRepPat "src/htmlBody.html" "replacekey00" 
  let divStr = [r|<input type='hidden' id='osid' value='|] <> osName <> [r|' />|]
  let divSBS = toSBS divStr
  let osidStr = "hiddenosid"::BS.ByteString
  let bb = searchReplaceAnySBS bs osidStr divSBS
  ls <- getPreStyle
  let s = concatStr (map cssToStr ls) []
  let sub = toSBS $ "<style>.co0{" + s + "}</style>"
  let pat1 = "replacestylecolor"::BS.ByteString
  let b1 = searchReplaceAnySBS bb pat1 sub
  return b1
  where
    (+) = (++)
            
{-| 
    === Get user input: cmd = "s java regex", autocomplete commands

    1. remove spaces from cmd
    2. insert cmd to table: userinput if userinput exists, otherwise create table: userinput
        1. sorted all cmd and create Html form with all cmd
        2. create Html output from cmd query.

        + store user input commands, autocomplete commands in a table: userinput
        + if table does not exist, create one, otherwise insert data to table: userinput
        + autocomplete, query commands from sqlite table

    3. No Search field, only query blocks data

    :NOTE: USE in anyRoute2
-}               
responseSnippetHTML2::Connection -> String -> IORef HMap2-> Response
responseSnippetHTML2 conn cmd ref = responseStream
              status200
              [("Content-Type", "text/html"), ("Access-Control-Allow-Origin", "*")] $ \write flush -> do
              let sCmd = (trim cmd)

              -- store user input commands, autocomplete commands in a table: userinput
              -- if table does not exist, create one, otherwise insert data to table: userinput
              -- autocomplete, query commands from sqlite table
              -- logCurrCmd [sCmd]
              -- create table if table: userinput does not exist
              execute_ conn sql_create_table 
              execute conn sql_insert (UserInput 0 (toSText cmd))
              cmdsql <- query_ conn sql_select ::IO [UserInput]
              let cmdList = let ls = map (xcmd) cmdsql::[TS.Text] in map toStr ls::[String]
              -- pa cmdList 

              let sortList = groupCountFilter cmdList
              -- limit the number of commands
              let autoList = take 20 sortList
              -- pa sortList 
              -- writeToFileAppend cmdLog [sCmd] 
              -- listCmd <- readCmd cmdLog SnippetT 
              -- write $ byteString $ toSBS $ replyHtml (spanBlock hmap (Just (toSBS (drop 2 sCmd)) )) listCmd 
              hmap <- readIORef ref
              pre sCmd
              fw "--"
              -- pre $ spanBlockX transform hmap (Just (toSBS (drop 2 sCmd)))
              -- let htmlByteStr = toSBS $ spanBlockX transform hmap (Just (toSBS (drop 2 sCmd)))
              -- let htmlByteStr = toSBS $ spanBlockX1 transform hmap (Just (toSBS (drop 2 sCmd)))
              let htmlByteStr = toSBS $ spanBlockX1 transform hmap (Just (toSBS (drop 2 sCmd)))

              -- following line causes the server error
              -- writeFileBS "/tmp/b.html" htmlByteStr
              -- response html byte string
              -- byteString::ByteString -> Builder
              write $ byteString htmlByteStr 

              flush
              where 
                sql_create_table = "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
                sql_insert = "INSERT INTO userinput (xcmd) VALUES (?)" 
                sql_select = "SELECT id, xcmd FROM userinput"
              
              

{-| 
    === query snippet from HMap2 without Html

    :NOTE: USE in anyRoute2
-}            
responseSnippetTxt2::String -> IORef HMap2 -> Response
responseSnippetTxt2 cmd ref = responseStream
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let sCmd = trim cmd
              putStrLn cmd
              -- store command to log file
              -- TODO: Write cmd to database
              -- writeToFileAppend cmdLog [sCmd] 
              -- Get the HMap from IO reference
              hmap <- readIORef ref 
              -- drop 2 sCmd : "n java" => "java"
              -- key = "java" => use the key in hmap 
              -- response the byteString to client
              write $ byteString $ toSBS $ spanBlockXX2 hmap (Just (toSBS (drop 2 sCmd)))
              flush              

{-|
    === Generate matrix from Json javascript XMLHttpRequest

    * See *haskellwebapp2/postMatrix.html*
    * <file:///Users/cat/myfile/bitbucket/haskellwebapp2/postMatrix.html postMatrix.html>
    * <http://localhost:8080/matrix Link To Matrix>
    *
    * Receive request from client
    * Decode body
    * Pattern matching Record Maybe 'GeneMatrix'
    * Construct record 'MatInt' if ncol and nrow are in 'GeneMatrix', otherwise create an empty 'MatInt'

    @
      MatInt{name="", matrix=[]}
    @
-} 
geneRectMat::Application
geneRectMat req response = do 
        str <- getRequestBodyChunk req
        
        let may = DA.decode $ toLBS str :: Maybe GeneMatrix 
        fw "may"
        print may
        let matJson = case may of 
                    (Just x) -> x 
                    _        -> GeneMatrix{cmd = "", ncol = 0, nrow=0} 
        fw "matJson"
        print matJson
        -- get ncol and nrow from client, otherwise set MatInt{name="", matrix=[]} 
        let gmatrix = case (cmd matJson) of 
                                -- genematrix is from file => haskellwebapp2/postMatrix.html
                                "genematrix" -> let nc = (ncol matJson) 
                                                    nr = (nrow matJson)
                                                in MatInt{name = "genemat", matrix = geneMatMN nc nr}
                                _            -> MatInt{name = "genemat", matrix = []} 
        let gbs = (toSBS . DA.encode) gmatrix
        fw "gbs"
        pre $ toStr gbs 
        let json = (toSBS . DA.encode) GeneMatrix{cmd = "mycmd", ncol=3, nrow=4}
        fw "str"
        S8.putStrLn str
        fw "response gbs"
        response $ responseNothingBS gbs

        
{-|
    === Generate HTML Table
-}
geneHTMLTable::Application
geneHTMLTable req response = do 
        str <- getRequestBodyChunk req        
        let may = (DA.decode . toLBS) str :: Maybe GeneMatrix 
        fw "may"
        print may
        let htabJson = case may of 
                    (Just x) -> x 
                    _        -> GeneMatrix{cmd = "", ncol = 0, nrow = 0} 
        fw "htabJson"
        print htabJson
        -- get ncol and nrow from client, otherwise set MatInt{name="", matrix=[]} 
        let htmlTable = case (cmd htabJson) of 
                                -- genematrix is from file => haskellwebapp2/postMatrix.html
                                "htmltable"  -> let nc = (ncol htabJson) 
                                                    nr = (nrow htabJson)
                                                in HTMLTable{name = "htmltable", matrix = htmlTableRowColSText nc nr}
                                _            -> HTMLTable{name = "htmltable", matrix = []} 
        let htab = (toSBS . DA.encode) htmlTable
        fw "htab"
        pre $ toStr htab
        fw "htmltable"
        pre htmlTable
        fw "str"
        S8.putStrLn str
        fw "response htab"
        response $ responseJSON htmlTable

       
{-|
	=== Get text color and background color from Redis

        * The function will be called from Ajax in aronlib.js
        * Click on the background/color => make a Ajax call

        @
         ("getcolor":_)        -> getPreFromRedis request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
        @

        > aronlib.js => Ajax => getPreFromRedis

        > getPreStyle::IO [(String, String)]

        * send JSON to client

        > data PreColor = PreColor{color::TS.Text, background::TS.Text}

        > redis-cli
        > keys 'HTMLPre.color'
        > keys 'HTMLPre.background-color'

        * Redis KEYS 'color'
        * Redis KEYS 'background-color'
-}
getPreFromRedis::Application
getPreFromRedis req response = do
                   styleList <- getPreStyle
                   let preColor = if len styleList == 2
                         then let m = M.fromList styleList
                              in  PreColor{color = case (M.lookup "color" m) of
                                                               Just x -> toSText x
                                                               _      -> ""
                                          ,
                                           background = case (M.lookup "background-color" m) of
                                                               Just x -> toSText x
                                                               _      -> ""
                                          }
                         else PreColor{color = "", background = ""}
                   -- Form a JSON object PreColor and send it back to client side
                   fw "preColor"
                   pre preColor
                   
                   when (color preColor == "") $ print "Redis has no key 'color'"
                   when (background preColor == "") $ print "Redis has no key 'background'" >> return ()
                   if (color preColor == "") then print "empty" else print "not empty"
                   response $ responseJSON preColor

-- data UpdateCodeBlock = UpdateCodeBlock{pid::Integer, newcode::String} deriving (Generic, Show)
updateCodeBlock::Connection -> IORef HMap2 -> Application
updateCodeBlock conn ref req response = do
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe UpdateCodeBlock
  fw "may"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> UpdateCodeBlock{pid = 0, newcode="no code", begt=0, endt=0} 
  fw "codeJson"
  pre codeJson
  updateDatabaseNewCodeTable conn (pid codeJson) (toSText $ newcode codeJson)
  -- if update ok, then send back "ok"
  let begtClient = begt codeJson
  let upcodeblock = UpCodeBlock{ok = "True", retcmd = "update", retbegt = begtClient, retendt = 0}
  newList <- readDatabaseCodeBlock3 conn 
  -- pre newList
  -- read the map out from ref
  -- conver all the keys to list of keyssnippetMap::[([String], [String])] -> IORef HMap -> IO ()
  -- rehash the map
  -- type HMap = M.HashMap String [[String]] 
  -- IORef HMap => ref
  updatePrefixMap newList ref
    
--   hmap <- readIORef ref 
--   let keys = M.keys hmap
--   modifyIORef ref (mapClear2 keys)
--   listToPrefixMap newList ref
  response $ responseJSON upcodeblock

{-|
    === KEY: validate snippet format

    @
    snippet:*: code
    line 1
    @
-}
validateFormat::String -> Bool
validateFormat s = len ls > 1 && (len . sp . head) ls > 2
  where
    ls = lines s
    sp e = splitStr ":" e



{-|
    === KEY: insert code block

    * It supports command line and Emacs

    @

    @
-}
insertCodeBlock::Connection -> IORef HMap2 -> Application
insertCodeBlock conn ref req response = do
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe UpdateCodeBlock
  fw "may"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> UpdateCodeBlock{pid = 0, newcode = "no code", begt = 0, endt = 0} 
  fw "codeJson"
  pre codeJson
  let code = newcode codeJson
  let begtClient = begt codeJson
  let upcodeblock = UpCodeBlock{ok = "True", retcmd = "add", retbegt = begtClient, retendt = 0}
  if validateFormat $ toStr code
    then do
    insertDatabaseNewCodeTable conn (pid codeJson) (toSText code)
    -- if update ok, then send back "ok"
    newList <- readDatabaseCodeBlock3 conn 
    -- pre newList
    -- read the map out from ref
    -- conver all the keys to list of keyssnippetMap::[([String], [String])] -> IORef HMap -> IO ()
    -- rehash the map
    -- type HMap = M.HashMap String [[String]] 
    -- IORef HMap => ref
    updatePrefixMap newList ref
    -- pre newList
    
    --   hmap <- readIORef ref 
    --   let keys = M.keys hmap
    --   modifyIORef ref (mapClear2 keys)
    --   listToPrefixMap newList ref
    response $ responseJSON upcodeblock
    else do
    let upcodeblock' = updateOk "False" upcodeblock
    response $ responseJSON upcodeblock'


data EditorCode = EditorCode{
  editorbeg::Integer,
  editorend::Integer,
  editorfile::String,
  editorcmd::String,
  editorcode::String,
  editortheme::String,
  editormode::String
  } deriving (Generic, Show)

instance DA.FromJSON EditorCode
instance DA.ToJSON EditorCode where
    toEncoding = DA.genericToEncoding DA.defaultOptions
      
data EditorCodeReply = EditorCodeReply{
  replybeg::Integer,
  replyend::Integer,
  ret::String,
  replydata::String,
  replyfname::String,
  replytheme::String,
  replymode::String
  } deriving (Generic, Show)

instance DA.FromJSON   EditorCodeReply
instance DA.ToJSON     EditorCodeReply where
    toEncoding = DA.genericToEncoding DA.defaultOptions

-- Lens implementation for EditorCodeReply
type MyLens a b = (a -> b, b -> a -> a)

-- BEG12 ret => field
getRet::EditorCodeReply -> String
getRet rd = ret rd

setRet::String -> EditorCodeReply -> EditorCodeReply
setRet s rd = rd {ret = s}

getReplytheme::EditorCodeReply -> String
getReplytheme rd = replytheme rd

setReplytheme::String -> EditorCodeReply -> EditorCodeReply
setReplytheme s rd = rd {replytheme = s}
-- END12

              
getL :: MyLens a b -> a -> b
getL (g, _) = g  -- getL (g, _) a = g a

setL :: MyLens a b -> b -> a -> a
setL (_, h) = h  --  setL (_, h) b a = h b a

modL :: MyLens a b -> (b -> b) -> a -> a
modL l f a = setL l (f (getL l a)) a

ret'::MyLens EditorCodeReply String
ret' = (getRet, setRet)

replytheme'::MyLens EditorCodeReply String
replytheme' = (getReplytheme, setReplytheme)
       
(^.)::a -> MyLens a b -> b
a ^. l = getL l a

(^=)::MyLens a b -> b -> a -> a
(l ^= b) a = setL l b a  -- (^=) = setL
       
       
                
-- instance Default EditorCodeReply where
--   def = EditorCodeReply 0 0 "" "" ""
      

data ProcLatex = ProcLatex {x1cmd :: String,
                            x1opt :: [String],
                            x1cwd :: String} deriving (Show)

myloop::String -> IO Int
myloop f = do
      putStrLn "loop it"
      fExist f >>= \b -> if b then return 1 else myloop f            

loopDelay::Int -> String -> IO Int
loopDelay max f = do
      threadDelay 1000000
      putStrLn $ "max=" ++ show max
      if max > 8000000 then return 0
        else fExist f >>= \b -> if b then return 1 else loopDelay (max + 1000000) f            

--  
-- list 
-- runOnExternalProgram :: Int -> String -> IO String
runOnExternalProgram :: Int -> String -> FilePath -> IO (Either String String)
runOnExternalProgram n fLatexName outdir = 
    -- convert the input to a parameter of the external program
    let x = show $ n + 12
    -- bracketOnError:: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    in bracketOnError
        -- (createProcess (proc "ls" []){std_in = CreatePipe
                                         -- ,std_out = CreatePipe
                                         -- ,std_err = Inherit})
        -- (createProcess (proc "/opt/local/bin/pdflatex" ["-output-directory", outdir, fLatexName </> "latex.tex"]){ cwd = Just fLatexName
        (createProcess (proc "/opt/local/bin/pdflatex" ["-halt-on-error", "-output-directory", outdir,  fLatexName]){ cwd = Just (dropName fLatexName)
                                                    ,std_in = CreatePipe
                                                    ,std_out = CreatePipe
                                                    ,std_err = Inherit})
        (\(Just inh, Just outh, _, pid) -> terminateProcess pid >> waitForProcess pid)
        (\(Just inh, Just outh, _, pid) -> do

          -- fork a thread to consume output
          -- http://neilmitchell.blogspot.com/2012/06/flavours-of-mvar_04.html
          -- let loop f = do
                -- putStrLn "loop it"
                -- fExist f >>= \b -> if b then return 1 else loop f            
          mkdir $ outdir </> "myaaa"
          now <- timeNowSecond
          writeFileListAppend "/tmp/f.x" ["mkdir=>" ++ show now]
          let lo bo = if bo then bo else lo bo    
          output <- hGetContents outh
          outMVar <- newEmptyMVar
          -- forkIO $ evaluate (
            -- len ("a"::String)
            -- length output
            -- lo True
            -- myloop "/tmp/file.x" >>= \x -> return x
            --- length output
            -- ) >>= putMVar outMVar
          
          forkIO $ evaluate (
            do
            n <- loopDelay 1000000 fLatexName
            -- let ln = len output
            -- putMVar outMVar (n + ln)
            return n
            -- lo True
            -- myloop "/tmp/file.x" >>= \x -> return x
            --- length output
            ) >>= \x -> x >>= putMVar outMVar
          
          -- no input in this case
          hClose inh

          -- KEY: thread block, polling, wait on output, blocking until there is value in outMVar
          takeMVar outMVar
          now <- timeNowSecond
          writeFileListAppend "/tmp/f.x" ["after takeMVar=>" ++ show now]
          -- hClose outh

          -- wait for process
          ex <- waitForProcess pid

          case ex of
            ExitSuccess -> do
              -- convert the output as needed
              let verboseAnswer = "External program answered: " ++ output
              -- return verboseAnswer
              return $ Right verboseAnswer
            ExitFailure r -> do
              -- ioError (mkIOError OtherError ("spawned process exit: " ++ show r) Nothing Nothing) )
              return $ Left ("spawned process exit: " ++ show r))


data LatexFilePath = LatexFilePath{xHtmlPath::String, xLatexPath::String, xPDFPath::String} deriving (Generic, Show)

randomName::IO String
randomName = ((++) "try") <$> (show <$> randomInt 100000 1000000)

data EFileType = EHTML | EPDF | EJSON

-- TODO: add root directory config file?
datadirFull::String -> EFileType ->IO String
datadirFull tryRandom ftype = do
  osMap <- confMap configFile
  home <- getEnv "HOME"
  let rootdir      = lookupJust "rootdir" osMap   -- "myfile/mybin/haskellwebapp2Bin"
  let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
  let fullPath = home </> rootdir </> datadirlatex
  let ext = case ftype of
        EHTML -> ".html"
        EPDF  -> ".pdf"
        EJSON -> ".json"
  return $ fullPath </> tryRandom </> tryRandom ++ ext -- src/datadir/latex/try1515




{-|
click on "compile"
 goto aronlib.js
  getElementsById("editor")
   get latex source code
    form JSON object => "compile"
                     => latex source code
    send to Server

 Server Side:
  ("editordata") ->
             receiveEditorData
                decode JSON object
                 => "compile"
                 => latex source code
                  => write latex source code to file $b/"latex.tex"
                  => pass path $b/latex.tex to
                       runOnExternalProgram
                         pdflatex compile latex.tex
                          => latex.pdf
       Either (String String) <= return from runOnExternalProgram

       Response to Client side in JSON
       => ret => "True" => Compile => OK
          ret => "False" => Compile => Error
          => On Client side aronlib.js
             => If ret == "True"
                  
           
-}
receiveEditorData::Connection -> IORef HMap2 -> IORef PDFMap -> Application
receiveEditorData conn ref pdfMapRef req response = do
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe EditorCode
  fw "Receive data:"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> EditorCode{editorbeg = 0, editorend = 0, editorfile = "", editorcmd = "", editorcode = "no data from editor", editortheme = "", editormode = ""} 
  fw "codeJson"
  pre codeJson
  let editBeg   = editorbeg   codeJson                                               
  let editEnd   = editorend   codeJson                                               
  let editFile  = editorfile  codeJson -- editorfile => "try919591.pdf"
  let editCmd   = editorcmd   codeJson -- editorcmd  => "compile" or "save"
  let editCode  = editorcode  codeJson -- editorcode  => "latex source code" 
  let editTheme = editortheme codeJson                                               
  let editMode  = editormode  codeJson                                               

  let ls = splitStrChar "[/]" editMode

  -- if len ls == 0 then error "Error: editormode codeJson" else do
  --  logFile2 "/tmp/x.x" ["show codeJson"]
  osMap <- confMap configFile
  let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
  
  mathPath <- getEnv "m"
  fullrootdir <- getRootDirFull
  
  ran <- randomName -- try919591
    
  if editCmd == "save" then do
    threadDelay 1000000
    -- ran <- randomName -- try919591

    -- IF user input from browser "Save", then use input name
    --  ELSE use random name => ranTexFile
    logFileNoName ["editFile =>" ++ editFile]  -- write to "/tmp/x.x"
    let tryRandom = if (not . null) editFile then dropExt editFile else ran
        
    --  flatexFile = bitbucket/math/try919591.tex
    let flatexFile =  mathPath </> tryRandom ++ ".tex"
    writeFile flatexFile editCode
        
    --  outdirSave = haskellwebapp2/src/datadir/latex/try919591
    let outdirSave = fullrootdir </> datadirlatex </> tryRandom
    mkdir $ datadirlatex </> tryRandom

    htmlFile <- datadirFull tryRandom EHTML  -- "src/datadir/latex/try919591/try919591{.html, .pdf...}
    -- generate (random key, fileName.html, randomLatex.tex)
    copyFile (fullrootdir </> indexEditorHTML) htmlFile

    -- LIO.writeFile "/tmp/json.json" (encodeToLazyText codeJson)
    
    logFileNoName ["copy " ++  (fullrootdir </> indexEditorHTML) ++ " => " ++ htmlFile] -- write to "/tmp/x.x"
    pre htmlFile
      

    -- TODO: FIXME: \\ => \
    -- ls <- readFileListStrict htmlFile
    -- let ss = splitListWhen (\x -> (trim x) == "replace123" ) ls
    -- let ln = lines editCode  -- it seems to me lines ESCAPE \\ to \\\\ ?
    -- -- let ln = map(\x -> dropEnd 1 $ drop 1 $ show x) $ lines editCode
    -- let repls = (head ss) ++ ln ++ (last ss)
    -- writeFileList htmlFile repls

    -- replaceFileLineEscapeStrict htmlFile "replace123" editCode
    replaceFileLineNoRegex htmlFile hiddenLATEXCODE editCode
      
    -- replaceFileWithStr "replace123" editCode htmlFile
    let pdfName = dropExt (takeName htmlFile) ++ ".pdf"
    let hiddenHtml = [r|<input  type="hidden" id='idlatex' name="myname" value="|] <> pdfName <> [r|" /> |]
    let hiddenPDF = [r|<a href="|] <> pdfName <> [r|" onclick='promptPDFName()'>PDF</a> |]
    -- replaceFileWithStr "hidden123" hiddenHtml htmlFile
    -- KEY: video http://xfido.com/song/haskellwebapp2_help.mp4 
    let hiddenCompileOrSave = [r|<input  type="hidden" id='compilesaveID' name="compilesave" value="|] <> "savepage" <> [r|" /> |]
    replaceFileListWord [("hidden123", hiddenHtml), ("hidden444", hiddenPDF), (hiddenCOMPILESAVE, hiddenCompileOrSave)] htmlFile
    -- TODO: Fixed use random number => random file name
    -- TODO:           random number => user input name
    -- modifyIORef pdfMapRef $ M.insert ran ran
    -- modifyIORef pdfMapRef $ M.insert ran tryRandom
    modifyIORef pdfMapRef $ M.insert tryRandom tryRandom
    redisSet tryRandom tryRandom
    mkdir outdirSave
    logFileNoName ["mkdir => " ++ outdirSave]
    hostURI <- getHostName  -- http://xfido.com or localhost
    let replyURL = hostURI ++ "/aceeditor?id=" ++ tryRandom
    let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = replyURL, replyfname = takeName flatexFile, replytheme = "", replymode = ""}
    -- Compile math/try919591.tex => outdirSave = src/latex/datadir/try919591/{try919591.pdf, try919591.log try919591.aux}
        
    -- FIXME: if file is Javascript, then the line can be ignored
    mayei <- timeout (5*1000000) $ runOnExternalProgram 3 flatexFile outdirSave
    case mayei of
        Just ei -> case ei of
                     Right x -> do
                       putStrLn x
                       -- Save editor theme, mode, language
                       let jsonFile = outdirSave </> tryRandom ++ ".json"
                       LIO.writeFile jsonFile (encodeToLazyText codeJson)
                       -- response $ responsePDF $ dropExtension flatexFile ++ ".pdf"
                       -- hostURI <- getHostName  -- http://xfido.com or localhost
                       -- let replyURL = hostURI ++ "/aceeditor?id=" ++ ran
                       -- let replyURL = hostURI ++ "/aceeditor?id=" ++ tryRandom
                       -- logFile2 "/tmp/x.x" [tryRandom]
                       -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = replyURL, replyfname = takeName flatexFile, replytheme = "", replymode = ""}
                       response $ responseJSON upcodeblock
                     Left  e -> do
                       putStrLn e
                       -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "False", replydata = "save => Error", replyfname = takeName flatexFile}
                       let upcodeblock' = (ret' ^= "False") upcodeblock
                       response $ responseJSON upcodeblock'
        Nothing -> do
          let upcodeblock' = (ret' ^= "False") upcodeblock
          response $ responseJSON upcodeblock'
          -- ↑
          -- - - response = (Response -> IO ResponseReceived)
    
  else if editCmd == "compile" then do
    bitbucket <- getEnv "b"
    mathPath <- getEnv "m"
    -- let latexFile = "latex.tex"
    let latexName = dropExt editFile
    logFileNoName [editFile]  -- write to "/tmp/x.x"
    logFileNoName [latexName] -- write to "/tmp/x.x"
    let latexFile = if (not . null) editFile then latexName ++ ".tex" else error "editorfile => editFile CAN NOT BE EMPTY"
    let flatexFile = mathPath </> latexFile
    writeFile flatexFile editCode
    fl
    pre flatexFile
    fl
    pre editCode

    -- 
    -- file:///Users/cat/myfile/bitbucket/image/4Tile.svg
    --
    -- runOnExternalProgram::Int -> String -> FilePath -> IO (Either String String)
    --                                                               ↑
    --                    ↓-------------------------------------------
    -- timeout::Int -> IO a -> IO (Maybe a)
    --                      => IO Maybe (Either String String)
    -- TODO: here
    let outdirCompile = fullrootdir </> datadirlatex </> latexName
    mkdir outdirCompile
    let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = "compile => OK", replyfname = takeName flatexFile, replytheme = "", replymode = ""}
    logFileNoName [outdirCompile] -- write to "/tmp/x.x"
    mayei <- let sec = 1000000 in timeout (5*sec) $ runOnExternalProgram 3 flatexFile outdirCompile
    case mayei of
        Just ei -> case ei of
                     Right x -> do
                       putStrLn x
                       osMap <- confMap configFile
                       let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"

                       -- jsonFile <- datadirFull latexName EJSON
                       -- json <- jsonToRecord jsonFile :: IO (Maybe EditorCodeReply)
                       -- pre json
                       -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = "compile => OK", replyfname = takeName flatexFile, replytheme = "", replymode = ""}
                       response $ responseJSON upcodeblock
                     Left  e -> do
                       putStrLn e
                       let upcodeblock' = (ret' ^= "False") upcodeblock
                       response $ responseJSON upcodeblock'
        Nothing -> do
                     -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "False", replydata = "mayei => Nothing", replyfname = takeName flatexFile, replytheme = "", replymode = ""}
                     let upcodeblock' = (ret' ^= "False") upcodeblock
                     response $ responseJSON upcodeblock'
  else do
    let upcodeblock = EditorCodeReply{replybeg   = editBeg,
                                      replyend   = editEnd,
                                      ret        = "False",
                                      replydata  = editCmd ++ " : compile or save. Unknown option",
                                      replyfname = "",
                                      replytheme = "",
                                      replymode  = ""}
    response $ responseJSON upcodeblock


-- read postMatrix.html and replace osname with osName=Dawin, Linux, FreeBSD whatever it is
-- 
respondMatrix::Connection -> IORef HMap2 -> Application
respondMatrix _ _ req response = do
              str <- getRequestBodyChunk req
              ls <- runCmd "uname"
              let osName = if len ls > 0 then head ls else []
              bs <- readFileRepPat "postMatrix.html" "osname" $ toSBS osName
              pre bs
              -- https://hackage.haskell.org/package/bytestring-0.11.0.0/docs/Data-ByteString-Builder.html
              -- byteString :: ByteString -> Data.ByteString.Builder
              -- https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html
              response $ responseLBS
                status200
                [("Content-Type", "text/html")]
                (toLBS bs)

            -- Nothing -> response $ responseLBS
            -- status400
            -- [("Content-Type", "text/plain; charset=utf-8")]
            -- "No file parameter found"
            
data CommandService = CommandService{cmdServ::String, paramArg::String, inputData::String} deriving (Generic, Show)
instance DA.FromJSON CommandService
instance DA.ToJSON CommandService where
    toEncoding = DA.genericToEncoding DA.defaultOptions
      
{-|
    === KEY: support services from outside browser
    @
    @
-}
commandService::Connection -> IORef HMap2 -> Application
commandService conn ref req response = do
  let cmdAlignment = "alignment"
  let cmdCommentCode = "commentcode"
  let cmdUncommentCode = "uncommentcode"
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe CommandService
  fw "may"
  pre may
  let cmdService = case may of 
        (Just x) -> x 
        _        -> CommandService{cmdServ = "", paramArg="", inputData="no data"} 
  fw "cmdService"
  pre cmdService
  let cmd      = cmdServ   cmdService
  let cmdParam = paramArg  cmdService
  pp $ "cmdParam=" ++ cmdParam
  let strData  = inputData cmdService
  let upcodeblock = UpCodeBlock{ok = "False", retcmd = "runSh Alignment.sh", retbegt = 0, retendt = 0}

  pre cmdService

  if | cmd == cmdAlignment   -> do
       let cmdStr = "Alignment.sh"::String
       (e2, so, si2) <- runSh $ toSText (if cmdParam == "" then cmdStr else cmdStr ++ " " ++ cmdParam)
       if e2 == ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdout=so} 
                            in response $ responseJSON rcode
       else do
           let upcodeblock' = updateRetcmd "runSh Alignment.sh" upcodeblock
           response $ responseJSON upcodeblock'
     | cmd == cmdCommentCode -> do
       let cmdStr = "CommentCode.sh"::String
       (e2, so, si2) <- runSh $ toSText (if cmdParam == "" then cmdStr else cmdStr ++ " " ++ cmdParam)
       if e2 == ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdout=so} 
                            in response $ responseJSON rcode
       else do
           let upcodeblock' = updateRetcmd "runSh CommentCode.sh" upcodeblock
           response $ responseJSON upcodeblock'

     | cmd == cmdUncommentCode -> do
       let cmdStr = "UncommentCode.sh"::String
       (e2, so, si2) <- runSh $ toSText (if cmdParam == "" then cmdStr else cmdStr ++ " " ++ cmdParam)
       if e2 == ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdout=so} 
                            in response $ responseJSON rcode
       else do
           let upcodeblock' = updateRetcmd "runSh UncommentCode.sh" upcodeblock
           response $ responseJSON upcodeblock'

     | otherwise             -> do
       let rcode = ReplyCode{rcmd="", rerror = "", stdout=""} 
       response $ responseJSON rcode

 
deleteCodeBlock::Connection -> IORef HMap2 -> Application
deleteCodeBlock conn ref req response = do
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe UpdateCodeBlock
  fw "may"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> UpdateCodeBlock{pid = 0, newcode="no code", begt=0, endt=0} 
  fw "codeJson"
  pre codeJson
  deleteDatabaseNewCodeTable conn (pid codeJson) (toSText $ newcode codeJson)
  -- if update ok, then send back "ok"
  let begtClient = begt codeJson
  let upcodeblock = UpCodeBlock{ok = "true", retcmd="delete", retbegt = begtClient, retendt = 0}
  newList <- readDatabaseCodeBlock3 conn 
  -- pre newList
  -- read the map out from ref
  -- conver all the keys to list of keyssnippetMap::[([String], [String])] -> IORef HMap -> IO ()
  -- rehash the map
  -- type HMap = M.HashMap String [[String]] 
  -- IORef HMap => ref
  updatePrefixMap newList ref
    
  --   hmap <- readIORef ref 
  --   let keys = M.keys hmap
  --   modifyIORef ref (mapClear2 keys)
  --   listToPrefixMap newList ref
  response $ responseJSON upcodeblock

    
updatePrefixMap::[([String], [String], Integer)] -> IORef HMap2 -> IO()
updatePrefixMap ls ref = do
  hmap <- readIORef ref
  let keys = M.keys hmap
  modifyIORef ref (mapClear2 keys)
  listToPrefixMap ls ref
  return ()

    
splitWhenTwo::(a -> Bool) -> [a] -> ([a], [a])
splitWhenTwo f cs = (takeWhile (not . f) cs, dropWhile (not . f) cs)
             
{-|
    === redis get, redis set, background color, color

    @
    Redis color/background DB
    HTMLPre.color:#333333
    HTMLPre.background:#AAAAAA
    @

    'getPreStyle'

    @
     [txtColor, bgColor]
     =>
     [
         ( "color"
         , "#8c9172"
         )
     ,
         ( "background-color"
         , "#3c6358"
         )
     ]
    @

-}
getPreStyle::IO [(String, String)]
getPreStyle = do
        let keyTxtColor = "color"
        txtColor <- redisGetPre keyTxtColor
        let keyBgColor = "background-color"
        bgColor <- redisGetPre keyBgColor
        return [txtColor, bgColor]
 where
   redisGetPre k = do
                   let key = pre ++ k
                   may <- redisGet key  -- key = "HTMLPre.color", "HTMLPre.background"
                   let color = case may of
                                 Just x -> let ls = splitWhen (== ':') x in (head ls, last ls)
                                 _      -> ("#333333", "#AAAAAA")
                   return color
              where
                pre = "HTMLPre."
        
{-|
   === Update background color from user

   * See modifycolor.css  submitUpdateBackground

   * Receive background color from client and decode the JSON string

  src/mystyle.css
  pre {
    display: block;
    font-family: monospace;
    font-size: 14pt;
    white-space: pre;
    margin-top: 1px;
    /* margin-right: 1px; */
    margin-bottom: 1px;
    /* margin-left: 4px; */
    background: #6b695869;  <-- change color
    border-style: outset;
    border-width: thin;
  }

  data R1 = R1{name::String}
  data R2 = R2{name::String}

  data RR1 = RR1 Maybe R1
  data RR2 = RR2 Maybe R2

  @
    pre{color:#90996c;
       background-color:#000000;
    }
  @

  'getPreStyle'

  @
   ------------------------------------styleList-----------------------------------
   [
       ( "color"
       , "#8c9172"
       )
   ,
       ( "background-color"
       , "#3c6358"
       )
   ]
  @
-} 
updateBackground::Application 
updateBackground req response = do
        str <- getRequestBodyChunk req
        let may = (DA.decode . toLBS) str :: Maybe Bgcolor
        fw "updateBackground may"
        pre may
        case may of 
          (Just x) -> if | hasPrefix "color:" str -> redisSet (prefix + "color") str  -- redisSet "HTMLPre.color" "#333"
                         | hasPrefix "background-color:" str -> redisSet (prefix + "background-color") str  -- redisSet "HTMLPre.background-color" "#444"
                         | otherwise -> error $ "Invalid CSS Style=[" + str + "]"
                   where
                     str = toStr $ colorname x
          _        -> return ()
        styleList <- getPreStyle
        fw "styleList"
        pre styleList
        let ls = formCSS styleList
        fw "ls"
        pre ls
        let bigstr = "pre{" + unlines ls + "}"
        -- let newcolor = [r| pre { background: |] <> colorname colorJson <> [r|;} |]
        TIO.writeFile cssfile (toSText bigstr)
        response $ responseCSS cssfile
        -- response $ responseNothingBS "updateBackground nothing"
   where
     prefix = "HTMLPre."
     formCSS ls = map(\x -> (fst x) + ":" + (snd x) + ";") ls
     cssfile = "src/css/modifycolor.css"
     (+) = (++)



{-|
	=== Update pre text color

	* Receive text color from client and decode the JSON string

        * See src/aronlib.js

        See 'updateBackground'

        NOT USED NOW
-}
updateTextColor::Application   
updateTextColor req response = do
        str <- getRequestBodyChunk req
        -- data Textcolor = Textcolor = {textcolor :: TS.Text } deriving (Generic, Show)
        let may = (DA.decode . toLBS) str :: Maybe Textcolor
        fw "Maybe Textcolor"
        pre may
        let colorJson = case may of 
                    (Just x) -> x 
                    _        -> Textcolor{textcolor = ""} 
        fw "Textcolor matJson"
        pre colorJson
        redisSet (prefix ++ "color") (toStr $ textcolor colorJson)
        fw "Textcolor response gbs"
        let newcolor = [r| pre { color: |] <> textcolor colorJson <> [r|;} |]
        fw "newcolor"
        pre newcolor
        style <- getPreStyle
        fw "style"
        pre style
        if checkCSSColorFormat (textcolor colorJson) then TIO.writeFile "src/css/modifycolor.css" newcolor else return ()
        response $ responseNothingBS "updateTextColor nothing"
   where
     prefix = "HTMLPre."
                                                                                                            
-- checkCSSColorFormat::TS.Text -> Bool
-- checkCSSColorFormat s = TS.length s <= 7 && TS.head s == '#' && (isHexStr (toStr s'))
--     where 
--         s' = TS.drop 1 s
              
receiveCode::Application
receiveCode req response = do 
        str  <- getRequestBodyChunk req
        let may = DA.decode $ toLBS str :: Maybe CompileCode 
        fw "may"
        print may
        let ccode = case may of 
                       (Just x) -> x 
                       _       -> CompileCode{compiler = "", option = "", code= ""} 
        fw "cool" 
        fw "cool" 
        let firstLine = head $ lines $ toStr (code ccode)
        let lang = last $ splitStr "[[:space:]]+" firstLine
        if | lang == "cpp" -> TIO.writeFile "/tmp/code.cpp" (code ccode)
           | lang == "haskell" -> TIO.writeFile "/tmp/code.hs" (code ccode)
           | otherwise -> return () 
        pp lang 
        let cmd = if | lang == "cpp" -> "g++ -o out /tmp/code.cpp && ./out" :: String
                     | lang == "haskell" -> "runh2 /tmp/code.hs && /tmp/code" :: String
                     | otherwise -> "" :: String
        (e2, so, si2) <- runSh $ toSText cmd
        if e2 /= ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdout=si2} 
                                  in response $ responseJSON rcode 
        else do
            pp so     
            let replyCode = ReplyCode{rcmd="", rerror="", stdout= so} 
            response $ responseJSON replyCode

receiveCode2::Application                                                                            
receiveCode2 req response = do                                                                       
        str <- getRequestBodyChunk req                                                                      
        let may = DA.decode $ toLBS str :: Maybe CompileCode                                
        fw "may"                                                                                    
        print may                                                                                   
        let ccode = case may of                                                                     
                       (Just x) -> x                                                                
                       _       -> CompileCode{compiler = "", option = "", code= ""}                 
        fw "cool"                                                                                   
        fw "cool"                                                                                   
        let firstLine = head $ lines $ toStr (code ccode)                                 
        let lang = last $ splitStr "[[:space:]]+" firstLine                                         
        if | lang == "cpp" -> TIO.writeFile "./cppcode.cpp" (code ccode)                            
           | lang == "haskell" -> TIO.writeFile "./code.hs" (code ccode)                         
           | otherwise -> return ()                                                                 
        pp lang                                                                                     
        let cmd = if | lang == "cpp" -> "g++ -o cppout ./cppcode.cpp && ./cppout"                         
                     | lang == "haskell" -> "runh2 ./code.hs && /tmp/code"                       
                     | otherwise -> ""                                                              
        sout <- A.run cmd                                              
        let rcode = ReplyCode{rcmd="", rerror = "", stdout= (toSText $ head sout)}          
        response $ responseJSON rcode




responseEditor:: Response
responseEditor = responseFile
    status200
    [("Content-Type", "text/html")]
    "compileCode.html"
    Nothing

{-|
   === response javacript file function

   * response aronlib.js to client
-} 
responseJavascript::FilePath -> Response
responseJavascript fname = responseFile
  status200
  [(hContentType, "text/javascript")]
  fname
  Nothing

responseCSS::FilePath -> Response
responseCSS fname = responseFile
  status200
  [(hContentType, "text/css")]
  fname
  Nothing

  
-- pdfSent::BS.ByteString -> Response
responsePDF::FilePath -> Response
responsePDF fname = responseFile
  status200
  [(hContentType, "application/pdf"),
   (hContentDisposition, "inline;filename=" <> toSBS fname),
   (hCacheControl, "no-cache")
  ]
  fname
  Nothing

responseHtml::FilePath -> Response
responseHtml fname = responseFile
  status200
  [(hContentType, "text/html")]
  fname
  Nothing

{-| 
    @
    <form action="/upload" method="POST" enctype="multipart/form-data">
    Upload File: <input type="file" name="file"><br>
    <input type="submit" value="submit">
    </form> 
    @

    * Handle file uploads, storing the file in the current directory
    * upload :: Application
-} 
updateMap::Connection -> IORef HMap -> Application
updateMap conn ref req response = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (params, files) <- parseRequestBody lbsBackEnd req
    str <- getRequestBodyChunk req
    case requestMethod req of
        "POST" -> do 
              case lookup "myblock" params of 
                   Just code  -> do 
                      -- read snippet and update the new block
                      home <- getEnv "HOME"
                      hostURL <- getHostName
                      -- pplist <- readSnippet (home </> snippetP) 
                      -- let block = lines $ toStr code
                      -- LA.writeFile "/tmp/b1.x" (toLBS code)
                      -- receiveCodeHeaderSBS is used as Id for each codeblock
                      -- 'update' use receiveCodeHeaderSBS to remove codeblock and insert new codeblock to database 'CodeBlock'
                      -- See: updateDatabaseCodeBlockTable
    
                      -- TODO1: use pid as header
                      case lookup "header" params of 
                       Just receiveCodeHeaderSBS -> do 
                          -- receiveCodeHeaderSBS: the first line of code block.
                          -- receiveCodeHeaderSBS is as ID for each code block
                          -- receiveCodeHeaderSBS is not modified for any of 'delete', 'add', 'update'
                          --
                          -- read snippet and update the new block
                          -- filter out the edited block based on the header which is hidden in TextArea from client side
                          -- replace '\r\n' with '\n' using Text.PortableLines.lines and unlines
                          -- concat the new block to new pplist

                          -- Should read from database: CodeBlock table
                          -- pplist <- readSnippet (home </> snippetP) 
                          pplist <- readDatabaseCodeBlock2 conn


                          --    method="POST"><textarea name="header" rows="20" class="hide"> 
                          --    </textarea><textarea name="myblock" spellcheck="false" autofocus="true" onfocus="textAreaAdjust(this);" 
                          --    class="hide">  
                          --    </textarea><div class="butcen">
                          --    <input type="submit" name="update" value="update" id= |] <> cid "b" n <> [r| class="submitButton"> 
                          --    <input type="submit" name="add"    value="add"    id= |] <> cid "a" n <> [r| class="submitButton"> 
                          --    <input type="submit" name="delete" value="delete" id= |] <> cid "d" n <> [r| class="submitButton"> 
                          -- In hiddenForm2
                          -- </textarea><div class="butcen">
                          -- <input type="submit" name="update" value="update" id= |] <> cid "b" n <> [r| class="submitButton"> 
                          -- <input type="submit" name="add"    value="add"    id= |] <> cid "a" n <> [r| class="submitButton"> 
                          -- <input type="submit" name="delete" value="delete" id= |] <> cid "d" n <> [r| class="submitButton"> 
                          -- </div></form>
                          --
                          -- gx file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/jupyterlab.html
                          -- [ 
                          --    ( 
                          --        [ "pre_line2" 
                          --        , "fox pig" 
                          --        , "cow rat" 
                          --        ] 
                          --    , 
                          --        [ " pre_line2 :*.hs: fox pig, cow rat" 
                          --        , " this is line 2 " 
                          --        ] 
                          --    ) 
                          -- ]
                          -- params "add"    -> Maybe String
                          -- params "delete" -> Maybe String
                          -- params "update" -> Maybe String
                          let checkParams str = isJust $ lookup str params
                          -- filter out "updated" and "delete", do nothing if "add"
                          -- receiveCodeHeaderSBS is from click button 'update', 'delete', 'add'
                          -- 
                          -- if "add" cb will all code blocks
                          -- if "delete" cb will remove the 
                          -- head x => " pre_line2 :*.hs: fox pig, cow rat"
                          let cb = map (\x -> BU.fromString <$> snd x) $ filter(\(_, b) ->
                                    if | checkParams "add"    -> True    -- nothing to filter
                                       | checkParams "delete" -> isMatchedHeader b receiveCodeHeaderSBS -- remove matched header from blocks
                                       | checkParams "update" -> isMatchedHeader b receiveCodeHeaderSBS -- remove matched header from blocks
                                       | otherwise      -> error "Invalid field: 'add', 'delete' and 'update'"
                                       ) pplist -- [([String], [String])]
                                    where isMatchedHeader b h = trimLBS (toLBS $ head b) /= trimLBS (toLBS h)
                                         
                          pp $ typeOf cb -- [[ByteString]]
                          -- LA.writeFile "/tmp/h1.x" $ toLBS receiveCodeHeaderSBS
                          -- modifiedCode is the modified block from client
                          -- plines
                          -- http://localhost/htmlhaskelldoc/PortableLines.html
--                          let modifiedCode = if click "add" then "" else
--                                unlines $ map(\x -> if len (trim x) > 0 then trimEnd x else x) $ let ln = plines $ toStr code in (trim $ head ln) : tail ln
                          -- if "delete" => modifiedCode = "" => concat listBlock and empty
                          -- if "add"    => modifiedCode contains code from client side
                          -- if "update" => modifiedCode contains modified code from client side
                          let modifiedCode = if | checkParams "delete" -> [] 
                                                | otherwise -> unlines $ map(\x -> if len (trim x) > 0 then trimEnd x else x) $ let ln = plines $ toStr code in (trim $ head ln) : tail ln


                          let newSnippet =  home </> "myfile/bitbucket/snippets/snippet_new.hs"
                          let mvSnippet  =  home </> "myfile/bitbucket/snippets/snippet_mv.hs"
                          -- listBlock is all the blocks excluding the modified block
                          let listBlock = BS.concat $ map(\x -> BS.cons (BI.c2w '\n') $ BS.concat $ map (BS.cons (BI.c2w '\n')) x) cb 

                          -- updateDatabaseCodeBlockTable conn ["hi"]
                          let cstext = map toSText $ plines $ toStr code
                          if | checkParams "update"   -> do
                                      -- update database table with updated codeblock
                                      let header = let ln = plines $ toStr code in toSText receiveCodeHeaderSBS 
                                      updateDatabaseCodeBlockTable conn header (toSText code)
                                      -- does not do any sth the following line
                                      codels <- readDatabaseCodeBlock2 conn
                                      pp "done"
                             | checkParams "add" -> do
                                      let header = let ln = plines $ toStr code in toSText $ head ln
                                      addCodeBlockTable conn header (toSText code) 

                             | checkParams "delete" -> do
                                      -- remove codeblock from db using receiveCodeHeaderSBS
                                      let header = let ln = plines $ toStr code in toSText receiveCodeHeaderSBS 
                                      deleteDatabaseCodeBlockTable conn header 
                             | otherwise            -> return ()

                          pp "update"
                          LA.writeFile newSnippet $ toLBS $ BS.concat [listBlock, BS.cons (BI.c2w '\n') $ BS.cons (BI.c2w '\n') $ (toSBS . toLBS) modifiedCode]
                          (e2, so, si2) <- runSh $ toSText ("mv " ++ newSnippet ++ " " ++ (home </> snippetP))

                          if e2 /= ExitSuccess || False then error (toStr si2) 
                            else do
                            -- Reload data from database
                            -- Clear the hmap
                            -- Reload data to hmap
                            
                            -- TODO: read from database
                            -- newList <- readSnippet (home </> snippetP) 
                            newList <- readDatabaseCodeBlock2 conn 
                            -- pre newList
                            -- read the map out from ref
                            -- conver all the keys to list of keyssnippetMap::[([String], [String])] -> IORef HMap -> IO ()
                            -- rehash the map
                            -- type HMap = M.HashMap String [[String]] 
                            -- IORef HMap => ref
                            hmap <- readIORef ref 
                            let keys = M.keys hmap
                            modifyIORef ref (mapClear keys)
                            fw "newList beg" 
                            -- pre newList
                            fw "newList end"
                            listToPrefixMapOld newList ref
                            pp "dog"
                      -- response $ responseNothing "post nothing"
                      response =<< let Just uri = parseURI (hostURL ++ "/snippet") in redirect' status302 [] uri 
        _   -> do 
               LA.writeFile "/tmp/b2.x" "no post" 
               response $ responseNothing "b2.x"
               
--    case lookup "id" params of
--        Nothing -> undefined
--        Just x -> undefined

-- http://localhost:8000/up/
-- | NOTE: file => /upload dir
-- | Plz see uploadPage.html 
replyEditor :: Response
replyEditor = responseFile
    status200
    [("Content-Type", "text/html")]
    "indexEditor.html"
    Nothing

responseHelp :: Response
responseHelp = responseFile
    status200
    [("Content-Type", "text/html")]
    "help.html"
    Nothing

replyCssButton :: Response
replyCssButton = responseFile
    status200
    [("Content-Type", "text/html")]
    "cssButton.html"
    Nothing

wordcountReply :: Response
wordcountReply = responseFile
    status200
    [("Content-Type", "text/html")]
    "wordcountReply.html"
    Nothing

matrixReply :: Response
matrixReply = responseFile
    status200
    [("Content-Type", "text/html")]
    "postMatrix.html"
    Nothing

sendHTMLTableCmd :: Response
sendHTMLTableCmd = responseFile
    status200
    [("Content-Type", "text/html")]
    "sendHTMLTableCmd.html"
    Nothing

replyJS :: Response
replyJS = responseFile
    status200
    [("Content-Type", "text/javascript")]
    "ace/build/src/ace.js"
    Nothing


{-| 
    === Insert name and age to MySqlite-simple file-based database.

    http://localhost:8000/insert/

    File: insert.html
    <form action="/insert" method="POST" enctype="multipart/form-data">
      Name <input type="text" name="name"><br>
      Age <input type="text" name="age"><br>
      <input type="submit" value="submit">
    </form> 

    >insert data to table: people
    >"INSERT INTO people (name, age) VALUES (?,?)" 
-} 
insertDatabase::Connection -> Application
insertDatabase conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let name = case lookup "name" params of 
                                Just name -> name 
                                _         -> "name nothing"
              let age = case lookup "age" params of 
                                Just age  -> age 
                                _         -> "age nothing" 


              execute_ conn "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age TEXT)"
              execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 (toSText name) (toSText age))
              people <- query_ conn "SELECT id, name, age from people" :: IO [Person]
              pre people
              hostURL <- getHostName
              response =<< let Just uri = parseURI (hostURL ++ "/insertinfo/") in redirect' status302 [] uri 
              -- response $ responseNothing $ b2s $ BS.concat [name, age]
        _      -> response $ responseNothing "post nothing from insertDatabase"


      
--readDatabaseCodeBlock::Connection -> IO [([String], [String])]
--readDatabaseCodeBlock conn = do
--              mycode <- query_ conn "SELECT id, header, codeblock from CodeBlock" :: IO [CodeBlock]
--              fl
--              pre mycode
--              fl
--              let codels = map (\x -> let h = head $ lines $ strictTextToStr x 
--                                      in (removeIndex 1 $ splitStrChar "[:]" h, lines $ strictTextToStr x)) $ map (\x -> codeblock x) mycode 
--              return  codels
--    where 
--        b2s = strictTextToStr . strictByteStringToStrictText
--        toSText = strictByteStringToStrictText

{-| 
    @
    data CodeBlock = 
    CodeBlock 
    { id        :: Int64
    , header    :: TS.Text
    , codeblock :: TS.Text
    } deriving (Eq, Read, Show)
    @
-} 
readDatabaseCodeBlock2::Connection -> IO [([String], [String])]
readDatabaseCodeBlock2 conn = do
                      mycode <- query_ conn "SELECT id, header, codeblock from CodeBlock" :: IO [CodeBlock]
                      fw "mycode beg"
                      -- pre mycode
                      fw "mycode end"
                      -- only codeblocks
                      let list = map (\x -> (toStr . codeblock) x) mycode 
                      -- let ll = filter(\x -> length x > 0) $ splitWhen(\x -> (length $ trim x) == 0) list
                      let ll = map plines list
                      -- let plist = map(\x -> ((splitStrChar "[,:]" $ head x), x) ) ll
                          
                      -- 1. take the first line of codeblock
                      -- 2. splitStrChar "[:]" =>    mycode : *.hs : Just Bieber Constable, the governer of royal castle
                      -- 3. removeIndex 1 => [mycode] [Just Bieber Constable, the governer of royal castle]
                      -- 4. plist = [[mycode] [Just Bieber Constable, the governer of royal castle]]
                      let tupleList = map(\x -> ((removeIndex 1 $ splitStrChar "[:]" $ (trim . head) x), x) ) ll
                          
                      let pplist = map(\k -> (
                                               uniqueOrder $ foldr(++) [] (map(\x -> map(\r -> trim r) $ splitStrChar "[,]" x) (fst k)), 
                                               snd k
                                             ) 
                                      ) tupleList
                      return pplist
                      
{-| 
    @
    -- defind in AronModule.hs
    data CodeBlock = 
    CodeBlock 
    { codeblockId  :: Int64
    , header       :: TS.Text
    , codeblock    :: TS.Text
    } deriving (Eq, Read, Show)
    @
-} 
readDatabaseCodeBlock3::Connection -> IO [([String], [String], Integer)]
readDatabaseCodeBlock3 conn = do
                      -- read CodeBlock table => [CodeBlock]::[Text]
                      mycode <- query_ conn "SELECT id, header, codeblock from CodeBlock" :: IO [CodeBlock]
                      -- mycode = [CodeBlock]::[Text]
                      fw "mycode beg"
                      -- pre mycode
                      fw "mycode end"
                      -- only codeblocks
                      -- let list = map (\x -> ( codeBlockId x, (toStr.header) x, (toStr.codeblock) x) ) mycode 
                      let list = map (\x -> let cb = (toStr . codeblock) x in ( codeBlockId x, head $ plines cb, cb) ) mycode 
                      --  list = [(id, header, codebock)]
                      fw "list"
                      -- pre list
                      -- let ll = filter(\x -> length x > 0) $ splitWhen(\x -> (length $ trim x) == 0) list
                      --
                      -- header field in CodeBlock is not used
                      let ll = map (\(n, _, c) -> (n, head $ plines c, plines c)) list
                      -- ll = [(id, header, [[line 1], [line 2]])]
                      -- let plist = map(\x -> ((splitStrChar "[,:]" $ head x), x) ) ll
                          
                      -- 1. take the first line of codeblock
                      -- 2. splitStrChar "[:]" =>  mycode : *.hs : Just Bieber Constable, the governer of royal castle
                      --                       =>  [mycode] [*.hs] [Just Bieber Constable, the governer of royal castle]
                      --    remove *.hs 
                      -- 3. removeIndex 1 => [mycode] [Just Bieber Constable, the governer of royal castle]
                      -- 4. tupleList = [[mycode] [Just Bieber Constable, the governer of royal castle]]
                      fw "ll"
                      -- pre ll
                      let tupleList = map(\(n, h, x) -> ((removeIndex 1 $ splitStrChar "[:]" h), x, n) ) ll
                      fw "tupleList"
                      -- pre tupleList
                      let pplist = map(\(hh, x, n) -> (
                                                      uniqueOrder $ foldr(++) [] $ map (splitStrCharTrim "[,]") hh, 
                                                      x, toInteger n)
                                             ) tupleList
                      fw "pplist"
                      -- pre pplist
                      -- pre $ typeOf pplist    
                      return pplist
                      
--              fl
--              pre mycode
--              fl
--              let codels = map (\x -> let h = head $ lines $ strictTextToStr x 
--                                      in (removeIndex 1 $ splitStrChar "[:]" h, lines $ strictTextToStr x)) $ map (\x -> codeblock x) mycode 
--              return  codels
--    where 
--        b2s = strictTextToStr . strictByteStringToStrictText
--        toSText = strictByteStringToStrictText

readSnippet2::FilePath->IO [([String], [String])]
readSnippet2 path = do 
            -- list <- readFileToList path;
            list <- readFileLatin1ToList path;
            let ll = filter(\x -> length x > 0) $ splitWhen(\x -> (length $ trim x) == 0) list
            -- let plist = map(\x -> ((splitStrChar "[,:]" $ head x), x) ) ll
            let plist = map(\x -> ((removeIndex 1 $ splitStrChar "[:]" $ head x), x) ) ll
            let pplist = map(\k -> (
                                       -- remove duplicated elem and keey the order
                                       -- L.nubBy (\x y -> x == y) $ foldr(++) [] (map(\x -> map(\r -> trim r) $ splitStrChar "[,]" x) (fst k)), 
                                       uniqueOrder $ foldr(++) [] (map(\x -> map(\r -> trim r) $ splitStrChar "[,]" x) (fst k)), 

                                       -- NOTE: fix bug, unique does not keep the order of elem
                                       -- unique $ foldr(++) [] (map(\x -> map(\r -> trim r) $ splitStrChar "[,]" x) (fst k)), 
                                       snd k
                                   ) 
                            ) plist
            return pplist 


              
{-| 
    === Create CodeBlock table
-} 
createCodeBlockTable::Connection -> IO() 
createCodeBlockTable conn = do
              execute_ conn "CREATE TABLE IF NOT EXISTS CodeBlock (id INTEGER PRIMARY KEY AUTOINCREMENT, header TEXT, codeblock TEXT)"
              return ()

addCodeBlockTable::Connection -> TS.Text -> TS.Text -> IO() 
addCodeBlockTable conn header text = do
              let header' = trimT header
              let text' = trimT text
              execute_ conn "CREATE TABLE IF NOT EXISTS CodeBlock (id INTEGER PRIMARY KEY AUTOINCREMENT, header TEXT, codeblock TEXT)"
              execute conn "INSERT INTO CodeBlock (header, codeblock) VALUES (?,?)" (CodeBlock 0 header' text')
              return ()

updateDatabaseCodeBlockTable::Connection -> TS.Text -> TS.Text -> IO() 
updateDatabaseCodeBlockTable conn oldHeader text = do
              let oldHeader' = trimT oldHeader 
              execute conn "DELETE FROM CodeBlock WHERE header = ? " (Only oldHeader') 
              codeblock <- query_ conn "SELECT id, header, codeblock from CodeBlock" :: IO [CodeBlock]
              -- pre codeblock 

              let newHeader = let ln = plines $ toStr text in toSText $ head ln
              let newHeader' = trimT newHeader
              let text' = trimT text
              execute_ conn "CREATE TABLE IF NOT EXISTS CodeBlock (id INTEGER PRIMARY KEY AUTOINCREMENT, header TEXT, codeblock TEXT)"
              execute conn "INSERT INTO CodeBlock (header, codeblock) VALUES (?,?)" (CodeBlock 0 newHeader' text)
              rowId <- lastInsertRowId conn
              let myhead = "hi"
              -- execute conn "DELETE FROM CodeBlock WHERE id = ? " (Only rowId) 
              -- TODO:
              -- oldHeader need to be cleanup a bit to compare the origin header
              fw "oldHeader beg"
              pre oldHeader
              fw "oldHeader end"
              return ()
{-|
-- data CodeBlock = 
--    CodeBlock 
--    { codeblockId        :: Int64
--    , header    :: TS.Text
--    , codeblock :: TS.Text
--    } deriving (Eq, Read, Show)
-}              
updateDatabaseNewCodeTable::Connection -> Integer -> TS.Text -> IO()
updateDatabaseNewCodeTable conn pid ucode = do
  let mycode = "hi"::TS.Text
  let pidInt = fromIntegral pid
  -- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html
  -- executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
  let header = head $ linesSText ucode
  executeNamed conn "UPDATE CodeBlock SET header = :header , codeblock = :codeblock WHERE id = :id " [":header" := header,  ":codeblock" := ucode, ":id" := (pidInt::Int64)]

{-|
   KEY: insert code to database
-- data CodeBlock = 
--    CodeBlock 
--    { codeblockId        :: Int64
--    , header    :: TS.Text
--    , codeblock :: TS.Text
--    } deriving (Eq, Read, Show)
-} 
insertDatabaseNewCodeTable::Connection -> Integer -> TS.Text -> IO()
insertDatabaseNewCodeTable conn pid ucode = do
  let mycode = "hi"::TS.Text
  let pidInt = fromIntegral pid -- pidInt is not used here
  let header = toSText $ head $ plines $ toStr ucode
  execute conn "INSERT INTO CodeBlock (header, codeblock) VALUES(?, ?)" (CodeBlock 0 header ucode)
    
deleteDatabaseNewCodeTable::Connection -> Integer -> TS.Text -> IO()
deleteDatabaseNewCodeTable conn pid ucode = do
  let pidInt = fromIntegral pid
  execute conn "DELETE FROM CodeBlock WHERE id = ? " (Only (pidInt::Int64))       

deleteDatabaseCodeBlockTable::Connection -> TS.Text -> IO() 
deleteDatabaseCodeBlockTable conn header = do
  let header' = trimT header
  execute conn "DELETE FROM CodeBlock WHERE header = ? " (Only header') 
  codeblock <- query_ conn "SELECT id, header, codeblock from CodeBlock" :: IO [CodeBlock]
  pre codeblock 
  return ()

loginCheck::Connection -> Application
loginCheck conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let email_ = case lookup "email" params of 
                                Just email -> toSText email 
                                _          -> "email nothing" 

              let password_ = case lookup "password" params of 
                                -- Just password -> BS.takeWhile (not.DW.isSpace) $ BS.dropWhile (DW.isSpace) password 
                                Just password -> toSText password 
                                _             -> "password nothing" 
              print email_
              print password_ 
              -- row <- queryNamed conn "SELECT * FROM user WHERE uid = :uid" [":uid" := uid] :: IO [User]
              row <- queryNamed conn "SELECT * FROM user WHERE email = :email AND password = :password" [":password" := password_, ":email" := email_] :: IO [User]
              print row
              response $ responseNothing "row nothing"
        _      -> response $ responseNothing "user nothing"


{-| 
    validate user input and santize me
-} 
securityValidate:: BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   Bool
securityValidate name email password task money = nameBool && passwordBool && emailBool
        where
            nameT = toSText name
            passwordT = toSText password
            nameBool = if (TS.length nameT) > 0 && (TS.length nameT) < 40 && TS.all (isAlphaNum) nameT then True else False
            passwordBool = if TS.all (isAlphaNum) passwordT then True else False
            emailBool = EM.isValid email

insertUserDB::Connection -> Application
insertUserDB conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let name_ = case lookup "name" params of    -- ByteString
                          Just name -> name 
                          _         -> "name nothing"
              let email_ = case lookup "email" params of 
                           Just email -> email 
                           _          -> "email nothing" 

              let password_ = case lookup "password" params of 
                              Just password -> password 
                              _             -> "password nothing" 

              let task_ = case lookup "task" params of 
                          Just task -> task 
                          _         -> "task nothing" 

              let money_ = case lookup "money" params of 
                           Just money -> money 
                           _         -> "money nothing" 

              -- validate user input
              -- formatValidate::User -> Bool
              execute_ conn "CREATE TABLE IF NOT EXISTS user (uid INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, email TEXT, password TEXT, task TEXT, money INTEGER)"
              row <- queryNamed conn "SELECT * FROM user WHERE email = :email" [":email" := (toSText email_)] :: IO [User]
              if len row == 0 then do
                  execute conn "INSERT INTO user (name, email, password, task, money) VALUES (?,?,?,?,?)" (User 0 (toSText name_) (toSText email_) (toSText password_) (toSText task_) (b2Integer money_))
                  changeRow <- changes conn
                  print $ "changeRow=" ++ (show changeRow)
                  if changeRow == 1 then do
                      userList <- query_ conn "SELECT uid, name, email, password, task, money FROM user" :: IO [User]
                      mapM_ print userList
                      -- let listTask = TS.concat $ map (\x -> [r|<div>|] <> (t2b $ task x) <> [r|</div><br>|]) userList -- => TS.Text
                      -- response $ responseTaskBS (task (head userList))
                      hostURL <- getHostName >>= return . toSBS
                      response $ responseTaskBS $ replyTaskHtml hostURL task_ 
                  else do
                      response $ responseTaskBS "Insect task field" 
                      -- response =<< let Just uri = parseURI "http://localhost:8000/insertUser/" in redirect' status302 [] uri 
              else do
                  response $ responseNothing "email exists"
        _      -> response $ responseNothing "no POST"

    where 
        b2Integer = fi . b2i
        b2i = stringToInt . strictTextToStr . toSText
        

-- | -------------------------------------------------------------------------------- 
-- | Wed Dec  5 15:06:00 2018 
--   Sat Jun  8 23:42:18 2019 
-- | upload with following POST to upload file to server
-- | -------------------------------------------------------------------------------- 
-- <form action="/upload" method="POST" enctype="multipart/form-data">
--  Upload File: <input type="file" name="file"><br>
--  <input type="submit" value="submit">
-- </form> 
-- | -------------------------------------------------------------------------------- 
--   http://localhost:8000/up/
-- | File is uploaded to => haskell_web/uploaddir 
upload::String -> Application
upload updir req response = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (params, files) <- parseRequestBody lbsBackEnd req
--    case lookup "id" params of
--        Nothing -> undefined
--        Just x -> undefined

    -- Look for the file parameter called "file"
    case lookup "file" files of
        -- Not found, so return a 400 response
        Nothing -> response $ responseLBS
            status400
            [("Content-Type", "text/plain; charset=utf-8")]
            "No file parameter found"
        -- Got the file 
        -- take the file name
        -- grab the content
        -- write the file to filesystem
        Just file -> do
            let
                -- Determine the name of the file to write out
                name = takeFileName $ toStr $ fileName file
                -- and grab the content
                content = fileContent file
            -- Write it out
            writeToFile "/tmp/f1.x" [name]
            LA.writeFile (updir ++ name) content
            response $ replyCssButton 

searchMap:: Application
searchMap req response = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (_params, files) <- parseRequestBody lbsBackEnd req

    -- Look for the file parameter called "file"
    case lookup "post" _params of
        -- Not found, so return a 400 response
        Nothing -> response $ responseLBS
            status400
            [("Content-Type", "text/plain; charset=utf-8")]
            "No post"
        -- Got it!
        Just "post" -> response $ responseLBS
            status200
            [("Content-Type", "text/text")]
            "Just post" 
