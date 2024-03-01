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
module WaiLibSub where

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

import qualified Data.Word8                     as DW
import Data.Text (Text)  -- strict Text
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
import qualified AronModule                 as A

import qualified Turtle as TUR -- (empty, shellStrictWithErr, ExitCode)
-- import Data.Text.Lazy -- lazy Text

import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Util
import Network.URI
import Network.HTTP.Types.Status

import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

-- {-# LANGUAGE QuasiQuotes       #-}
import Text.RawString.QQ (r)         -- Need QuasiQuotes too 

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

hiddenLATEXCODE = "latexcode_replace314"
hiddenCOMPILESAVE = "hidden_compile_save"

dbname = "webappdb"
configFile = "./config.txt"

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

type HMap2 = M.HashMap String [([String], Integer)]
          
type HMap = M.HashMap String [[String]] 

type PDFMap = M.HashMap String String

-- Response html, css, js, pdf from Server
type RespMap = M.HashMap String String
             
jsonFile = "src/datadir/json/EditorCode.json"
           
lookupJust s m = fromJust $ M.lookup s m
                 



responseJSONBS::BS.ByteString -> Response
responseJSONBS bs = responseStream
              status200
              [(hContentType,  "application/json")] $ \write flush -> do                   
              write $ byteString bs                   
                 

responseJSON::(DA.ToJSON a) => a -> Response
responseJSON rd = responseJSONBS $ (toSBS . DA.encode) rd


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
app2::Connection -> IORef HMap2 -> Application
app2 conn1 ref request respond = do
  let x = 100
  let s = "a"
  case pathInfo request of 
   ("test":_)       -> respond $ responseNothing "test from hasekell-web-app-REST"
   ("raw":_)        -> respond plainIndex
   ("insert":_)          -> insertDatabase conn1 request respond
   ("insertcode":_)      -> insertCodeBlock conn1 ref request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
   ("json":_)       -> do
                         pre jsonFile
                         stackPro <- getEnv "sp"
                         let p = stackPro </> "haskell-web-app-REST"
                         pre $ p </> jsonFile
                         mjson <- jsonToRecord (p </> jsonFile) :: IO (Maybe EditorCode)
                         pre mjson
                         case mjson of
                           Just record -> respond $ responseJSON record
                           Nothing -> respond $ responseHelp
   []                    -> respond responseHelp
   _                     -> do
                              respond responseHelp

plainIndex::Response
plainIndex = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing
    
snippetP = "myfile/bitbucket/snippets/snippet_test.hs"

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
                                 var | var == "e " -> queryRedisSnippet s
                                     | otherwise   -> responseNothing ""  -- responseSearch conn "search 1"
                               | otherwise   -> responseNothing "" -- responseSearch conn "search 2" 
            _      -> responseNothing ""

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
    let sbsUpCodeBlockTrue = toSBS $ DA.encode upcodeblock
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
    response $ responseNothingBS sbsUpCodeBlockTrue
    else do
    let upcodeblock' = updateOk "False" upcodeblock
    let sbsUpCodeBlockFalse = (toSBS . DA.encode) upcodeblock'
    response $ responseNothingBS sbsUpCodeBlockFalse

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
       
data ProcLatex = ProcLatex {x1cmd :: String,
                            x1opt :: [String],
                            x1cwd :: String} deriving (Show)

data LatexFilePath = LatexFilePath{xHtmlPath::String, xLatexPath::String, xPDFPath::String} deriving (Generic, Show)

randomName::IO String
randomName = ((++) "try") <$> (show <$> randomInt 100000 1000000)

data EFileType = EHTML | EPDF | EJSON

datadirFull::String -> EFileType ->IO String
datadirFull name ftype = do
  osMap <- confMap configFile
  let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
  let ext = case ftype of
        EHTML -> ".html"
        EPDF  -> ".pdf"
        EJSON -> ".json"
  return $ datadirlatex </> name </> name ++ ext -- src/datadir/latex/try1515

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
  let editCode  = editorcode  codeJson -- editorcmd  => "latex source code" 
  let editTheme = editortheme codeJson                                               
  let editMode  = editormode  codeJson                                               

  logFile2 "/tmp/x.x" [show codeJson]
  osMap <- confMap configFile
  let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
  
  mathPath <- getEnv "m"
  webapp2 <- getEnv "hw"
  ran <- randomName -- try919591
      
  -- let outdir = webapp2 </> datadirlatex </> ran
    
  if editCmd == "save" then do
    -- mathPath <- getEnv "m"
    threadDelay 1000000
    -- ran <- randomName -- try919591

    -- IF user input from browser "Save", then use input name
    --  ELSE use random name => ranTexFile

    let tryRandom = if (not . null) editFile then dropExt editFile else ran
        
    --  flatexFile = bitbucket/math/try919591.tex
    let flatexFile =  mathPath </> tryRandom ++ ".tex"
        
    --  outdirSave = haskellwebapp2/src/datadir/latex/try919591
    let outdirSave = webapp2 </> datadirlatex </> tryRandom
        
    writeFile flatexFile editCode

    mkdir $ datadirlatex </> tryRandom

    htmlFile <- datadirFull tryRandom EHTML  -- "src/datadir/latex/try919591/try919591{.html, .pdf...}
    -- generate (random key, fileName.html, randomLatex.tex)
    --
    -- copyFile "indexTemp.html" htmlFile
    hw <- getEnv "hw"
    copyFile (hw </> indexEditorHTML) htmlFile

    -- LIO.writeFile "/tmp/json.json" (encodeToLazyText codeJson)
    
    logFile2 "/tmp/x.x" ["copy " ++ indexEditorHTML ++ " => " ++ htmlFile]
    pre htmlFile
      
    replaceFileLineNoRegex htmlFile hiddenLATEXCODE editCode
      
    let pdfName = dropExt (takeName htmlFile) ++ ".pdf"
    modifyIORef pdfMapRef $ M.insert tryRandom tryRandom
    redisSet tryRandom tryRandom
    mkdir outdirSave
    hostURI <- getHostName  -- http://xfido.com or localhost
    let replyURL = hostURI ++ "/aceeditor?id=" ++ tryRandom
    let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = replyURL, replyfname = takeName flatexFile, replytheme = "", replymode = ""}
    let jsonFile = outdirSave </> tryRandom ++ ".json"
    LIO.writeFile jsonFile (encodeToLazyText codeJson)
    -- response $ responsePDF $ dropExtension flatexFile ++ ".pdf"
    -- hostURI <- getHostName  -- http://xfido.com or localhost
    -- let replyURL = hostURI ++ "/aceeditor?id=" ++ ran
    -- let replyURL = hostURI ++ "/aceeditor?id=" ++ tryRandom
    -- logFile2 "/tmp/x.x" [tryRandom]
    -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = replyURL, replyfname = takeName flatexFile, replytheme = "", replymode = ""}
    let sbsUpCodeBlock = (toSBS . DA.encode) upcodeblock
    response $ responseNothingBS sbsUpCodeBlock
    
  else do
    bitbucket <- getEnv "b"
    mathPath <- getEnv "m"
    -- let latexFile = "latex.tex"
    let latexName = dropExt editFile
    logFile2 "/tmp/x.x" [editFile]
    logFile2 "/tmp/x.x" [latexName]
    let latexFile = if (not . null) editFile then latexName ++ ".tex" else error "editorfile => editFile CAN NOT BE EMPTY"
    let flatexFile = mathPath </> latexFile
    writeFile flatexFile editCode
    let outdirCompile = webapp2 </> datadirlatex </> latexName
    mkdir outdirCompile
    let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = "compile => OK", replyfname = takeName flatexFile, replytheme = "", replymode = ""}
    logFile2 "/tmp/x.x" [outdirCompile]
    let sbsUpCodeBlock = (toSBS . DA.encode) upcodeblock
    response $ responseNothingBS sbsUpCodeBlock
 
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
  let sbsUpCodeBlock = (toSBS . DA.encode) upcodeblock
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
  response $ responseNothingBS sbsUpCodeBlock
    
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
              print people
              response =<< let Just uri = parseURI "http://localhost:8000/insertinfo/" in redirect' status302 [] uri 
              -- response $ responseNothing $ b2s $ BS.concat [name, age]
        _      -> response $ responseNothing "post nothing"



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
