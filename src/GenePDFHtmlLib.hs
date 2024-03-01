-- {{{ begin_fold
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes       #-} -- RawString QQ 

module GenePDFHtmlLib where
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
-- import Turtle
-- echo "turtle"
-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Data.Char
import Data.Default
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS
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
import Data.IORef 
import Control.Monad (unless, when)
import Control.Concurrent 

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text                 as TS
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import qualified Text.Regex.TDFA as TD

-- {-# LANGUAGE QuasiQuotes       #-} -- RawString QQ, Quasi 
import Text.RawString.QQ          -- QuasiQuotes needs for Text.RawString.QQ 

import AronModule 
import AronHtml
import AronHtml2
       
-- |  end_fold ,}}}


-- zo - open
-- za - close


{-|
    === KEY:

    15-02-2021

    * Too many open files error when inserting Latex files to database
    * See $b/database/haskellwebapp2_sqlite3.db
    * Open db file

    @
    sqlite3 $b/database/haskellwebapp2_sqlite3.db

    -- Show all table
    -- .table
    -- table name => pdftable
    @
-}

data LHead = LHead {xtitle::String, desc::String} deriving(Show, Eq)

instance Default LHead where
   def = LHead{xtitle=""::String, desc=""::String}

data PDFInfo = PDFInfo 
    { pdfId :: Int64
    , title :: Text
    , pdesc :: Text
    , path  :: Text
    } deriving (Eq,Read,Show)

{-| 
    === Sqlite toRow and fromRow

    <https://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html Sqlite toRow and fromRow>

    <https://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html#t:SQLData SQLData>

    <https://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/src/Database.SQLite.Simple.FromRow.html#fromRow fromRow>

    @
    class ToRow where
        toRow:: a -> [SQLData]

    class FromRow where
        fromRow:: RowParser a

    class Functor f where
        fmap (a -> b) -> f a -> f b
    \<$> = fmap 

    (+1) \<$> Just [1, 2] => Just [2, 3]

    class Functor f => Applicative f where
        \<*> :: f (a -> b) -> f a -> f b

    Just (+1) \<*> Just [1, 2] => Just [2, 3]
    @
-} 
instance FromRow PDFInfo where
  fromRow = PDFInfo <$> field <*> field <*> field <*> field

instance ToRow PDFInfo where
  toRow (PDFInfo _pId title pdesc path) = toRow (title, pdesc, path)


-- test.tex
-- read the first two lines from a Latex file
-- % title : nice title
-- % desc : nice description
-------------------------------------------------------------------------------

{-| 
    === Parse first two lines inside .tex file

    * If title and desc are found, return an titleDesc with the title and desc
    * Otherwise return an default titleDesc which is empty title and empty desc.
    * Example inside latex file.
    
    @
    % title : This is an awesome.
    % desc  : The document is doing some fantastic thing. 
    @
-} 
titleDesc::[String] ->  LHead 
titleDesc [] = def LHead
titleDesc cx = he 
        where
         ls = map (splitStrChar "[:]") cx  -- " title : awesome doc " => ["title", "awesome doc"]
         t = if (containStr "title" ((head.head) ls)) then (last.head) ls else xtitle (def LHead) 
         d = if (containStr "desc" ((head.last) ls))  then (last.last) ls else desc  (def LHead) 
         he = LHead{xtitle = trim t, desc = trim d}

hempty = LHead {xtitle="", desc=""}
_PDFTABLE = "pdftable"


-- toT = strToStrictText
-- toS = strictTextToStr 


{-|
  @
   let link = "http://xfido.com"
   let imglink = "/a.png"
   href link (img imglink  styleTag_ [("width", "100px"), ("height", "100px")]) 
  @
-}

                                                
-- <embed src="files/Brochure.pdf" type="application/pdf" width="100%" height="600px" />              
embed::TS.Text -> TS.Text
embed link = [r|<embed src="|] <> link <> [r|" type="application/pdf" width="200px" height="200px" /> |]
                                                

td_::TS.Text -> TS.Text -> TS.Text
td_ att a = [r|<td |] <> att <> [r|>|] <> a <> [r|</td>|]

-- td_ f d = [r|<td id="notme"><a href="|] <> f <> [r|">|] <> d <> [r|</a></td> |]


trT::TS.Text -> TS.Text
trT s        =  [r|<tr>|] <> s <> [r|</tr>|]

styleT cs    =  " style='" <> (TS.concat cs) <> "' "

tableACT::TS.Text -> TS.Text -> TS.Text
tableACT a s  =  [r|<table |] <> a <> [r|>|] <> s <> [r|</table>|]

tableAA::[[[TS.Text]]] -> TS.Text 
tableAA cs = rc 
    where
        rc = tableACT (styleT st) 
                     (TS.concat $ map(\r -> trT $ TS.concat $ map(\x -> let h = baseNameT $ x ! 2 
                                                                            p = x ! 2
                                                                        in  td_ att (href_ p h)
                                                                 ) r) cs) 
        att = [r| id='notme' |] :: TS.Text
        (!) = (!!)
        st = ["table-layout:auto;", "width:100%;", "font-size:18px;", "background:#444A3F;"]::[TS.Text]

href_::TS.Text -> TS.Text -> TS.Text
href_ link desc = [r|<a href='|] <> link <> [r|'>|] <> desc <> [r|</a>|]

{-|

    === Generate html table "genepdf.html"
  @
  <table style=''>
    <tr><td><a href='pdf/f.pdf'><img src='pdfimage/f-0.png'></td>
    </tr>
  </table>
  @

  (title, baseNameT or pdesc, path to PDF file)

  XXX
-}
tableA_::[[(TS.Text, TS.Text, TS.Text)]] -> TS.Text 
tableA_ cs = rc 
    where
        -- Sat 22 Oct 23:30:35 2022 
        -- BUG: Safari does not show image but there is NOTHING to do with ("height", "")
        -- img_src png = img png  (tt (style_ [("width", "260px"), ("height", "100px")]))
        --
        -- <img src='pdfimage/f-0.png' width='260px' height=''>
        --               ↑ 
        --              src
        img_src::TS.Text -> TS.Text 
        img_src src = img src $ (toSText . style_) [("width", "260px"), ("height", "")] <> let name = TS.pack $ concatStr' " " $ (splitStr "-|_") $ (TS.unpack . baseNameT) src 
                                                                                           in [r|<br><span>|] <> name <> [r|</span>|]
                        where 
                          x = 3
                          concatStr'::String -> [String] -> String
                          concatStr' s cs = concatStr cs s

        splitPDFName::TS.Text -> TS.Text
        splitPDFName src = let name = TS.pack $ concatStr' " " $ (splitStr "-|_") $ (dropEnd 2 . TS.unpack . baseNameT) src 
                           in [r|<br><span>|] <> name <> [r|</span>|]
                            where 
                              concatStr'::String -> [String] -> String
                              concatStr' s cs = concatStr cs s

        imgTag::TS.Text -> TS.Text
        imgTag src = [r|<div style='display:grid;'><img src='|] <> src <>[r|' width='260px' height=''><span style='color:red;'>|] <> txt <> [r|</span></div>|] <> [r|<br><textarea rows=2 cols=10>|] <> txt <> [r|</textarea> |]
            where
              txt = splitPDFName src
        
        rc = tableACT (styleT st) 
              (TS.concat $ map(\r -> trT $ TS.concat $ map(\(a, b, c) -> let h = ""
                                                                         in  td_ att (href_ c (imgTag (imgPath c)))
                                                          ) r) cs) 
        att = [r| id='notme' |] :: TS.Text
        (!) = (!!)
        st = ["table-layout:auto;", "width:100%;", "font-size:18px;", "background:#444A3F;"]::[TS.Text]
        img::TS.Text -> TS.Text -> TS.Text
        img src style = [r|<img src='|] <> src <> [r|' |] <> style <> [r|>|]

        -- pdf/f.pdf => pdfimage/f-0.png
        imgPath::TS.Text -> TS.Text
        imgPath s = dir <> name <> png
          where
            png = "-0.png"::TS.Text
            name = baseNameT s
            dir = "pdfimage/" 

{-|
   === KEY: Generate png file format from pdf file

  @
    -- first page of pdf file
    /pdf/file.pdf => /pdf/file-0.png
  @
-}

-- <embed src="files/Brochure.pdf" type="application/pdf" width="100%" height="600px" />              
tableA2_::[[(TS.Text, TS.Text, TS.Text)]] -> TS.Text 
tableA2_ cs = rc 
    where
        rc = tableACT (styleT st) 
                     (TS.concat $ map(\r -> trT $ TS.concat $ map(\(a, b, c) -> let h = baseNameT c 
                                                                                 in  td_ att (embed c)
                                                                 ) r) cs) 
        att = [r| id='notme' |] :: TS.Text
        (!) = (!!)
        st = ["font-size:18px;", "background:#444A3F;"]::[TS.Text]              
                
{-| 
    === Insert tex file 'PDFInfo' to database

    * read file with readFileLatin1ToList
    * read the first two lines from a tex file
    * create 'PDFInfo' record
    * insert into database
    * 
    * texPath = $HOME </> "myfile/bitbucket/math"                              
-} 
insertTexToTable::Connection -> [String] -> IO() 
insertTexToTable conn allTexFile = do
        -- read the first two lines from a tex file
        -- allheads = [(first two lines, full path tex file)]
        allheads <- mapM(\x -> do 
                                ls <- readFileList x
                                let t = take 2 ls
                                return (t, x)
                                ) allTexFile 
        let texfile = filter (\x -> (titleDesc $ fst x) /= hempty) allheads 
        mapM_(\x -> let title = toSText $ (head . fst) x
                        pdesc = toSText $ (last . fst) x
                        path  = toSText $ snd x
                    in execute conn ([r| INSERT INTO |] <> _PDFTABLE <> [r| (title, pdesc, path) VALUES (?,?,?) |]) (PDFInfo 0 title pdesc path)
                       ) texfile
        print "add tex done." 


{-| 
    === Create new table '_PDFTABLE' 
-} 
dropTableSqlite::Connection -> Query -> IO()
dropTableSqlite conn fname = do
        -- let tableQuery = [r| DROP TABLE |] <> _PDFTABLE
        let tableQuery = Query ([r| DROP TABLE |] <> (fromQuery fname)) 
        execute_ conn tableQuery 
        -- print $ "drop table:" <> _PDFTABLE


{-| 
    === Check whether a table exist in Sqlite3

    * sqlite_master schema

    @
    CREATE TABLE sqlite_master(
      type text,
      name text,
      tbl_name text,
      rootpage integer,
      sql text
    );
    @
-}
doesTableExistSqlite::Connection -> Query -> IO Bool
doesTableExistSqlite conn tableName = do 
        -- let query = Query([r| SELECT * FROM sqlite_master WHERE type='table' AND name='pdftable' |])  
        let query = Query([r| SELECT * FROM sqlite_master WHERE type='table' AND name='|] <> (fromQuery tableName) <>[r|'|])  
        listTab <- query_ conn query :: IO [SqliteMaster]
        print listTab
        pp $ "len listTab=" <<< len listTab
        return $ len listTab > 0
    

{-| 
    === Create new table '_PDFTABLE' IF NOT EXISTS

    >_PDFTABLE = "pdftable"
-} 
createNewTable::Connection -> Query -> IO()
createNewTable conn query = do
        let tableQuery = Query ([r| CREATE TABLE IF NOT EXISTS |] <> (fromQuery query) <> [r| (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, pdesc TEXT, path TEXT) |])
        execute_ conn tableQuery 
        print "done" 
 
{-| 
    === Insert PDF file to database

    >_PDFTABLE = "pdftable"
-} 
insertPDFToTable::Connection -> [String] -> IO() 
insertPDFToTable conn allPDFfile = do
        -- read the first two lines from a tex file
        -- allheads = [(first two lines, full path tex file)]
        -- conn <- open dbstr 
        mapM_(\x -> let title = "" 
                        pdesc  = "" 
                        path  = toSText x
                        -- path  = toT x 
                    in execute conn ([r| INSERT INTO |] <> _PDFTABLE <> [r| (title, pdesc, path) VALUES (?,?,?) |]) (PDFInfo 0 title pdesc path)
                       ) allPDFfile 
        print "done" 


  
{-|
   === KEY: Query PDF files name from table:[_PDFfile = pdffile] and create html page genepdf.html => send html page to client

   ⇒ read PDF file name from table _PDFfile = pdffile
   ⇒ create html page, (html table)
     → * use 'tableA_'

   ⇒ send page to client

   >_PDFTABLE = "pdftable"

   1. Connection - db connection
   2. Query      - table name
   3. String     - absolute path location to pdf files 
   4. return     - Html page in BS.Bytestring

   @
    data PDFInfo = PDFInfo 
      { pdfId :: Int64
      , title :: Text
      , pdesc :: Text
      , path  :: Text
      } deriving (Eq,Read,Show)
   @
-}
queryDBToHtml::Connection -> Query -> String -> IO BS.ByteString
queryDBToHtml conn tableName dir = do
        pdfQuery <- query_ conn ([r| SELECT id, title, pdesc, path from |] <> tableName) :: IO [PDFInfo]
        -- numCol = 3, number of column one page
        let plist = partList numCol pdfQuery where numCol = 6  -- [[PDFInfo]]
        let pdfLL = map (\r -> map(\c -> let t = if (TS.length $ trimT $ title c) == 0 then toSText ("no title"::String) else trimT $ title c
                                             -- d = if (TS.length $ trimT $ pdesc c) == 0 then       baseNameT $ path c else pdesc c
                                             d = "pdfimage" 
                                             p =                                           (toSText dir) <> takeFileNameT (path c) 
                                         in (t, d, p)  -- (title, baseNameT or pdesc, path to PDF file)
                       ) r) plist

        -- let tablea = tableAA pdfLL
        -- let tablea = tableA_ pdfLL
        -- tableA_::[[(TS.Text, TS.Text, TS.Text)]] -> TS.Text 
        let tablea = tableA_ pdfLL
        -- print pdfLL
        getPwd >>= \curr -> putStrLn $ "Current dir=" ++ curr
        putStrLn $ "Number of PDF files => len=" ++ (show $ len pdfLL)
        putStrLn "Generate genepdf.html file"
        writeFileBS "genepdf.html" (toSBS tablea)
        return $ toSBS tablea

{-| 
    === Currently, the table need to be deleted manually in order to get new pdf files from $www/pdf

    See @config.txt@

    @
    sqlite3 $b/database/haskellwebapp_sqlite3.db

    .table
    .schema pdftable
    SELECT path FROM pdftable WHERE path LIKE '%haskell%';

    $b/database/haskellwebapp_sqlite3_test.db
    $b/database/haskellwebapp_sqlite3.db

    _PDFTABLE = "pdftable"

    * Rename table:
    ALERT TABLE old_table RENAME TO new_table

    * Restart the service
      ps aux | grep 'haskellwebapp2' | awk '{print $2}' | line 'x -> kill -9 x'
      haskellwebapp2.sh

    @

    * TODO: 
    * Better approach is to insert pdf file to database externally 
    * Create a monitor to the pdf folder and run the code to insert pdf file to table(pdftable)

    @
    dropTableSqlite conn _PDFTABLE 
    foundTable <- doesTableExistSqlite conn _PDFTABLE 
    createNewTable conn _PDFTABLE 
    insertTexToTable conn utexFile 
    insertPDFToTable conn updfFile 
    @

    * USED: WaiLib.hs => PDF.pdfMain

    * Currently, use all the PDF under $b/math

    @
    haskellwebapp2/pdf -> /Library/WebServer/Documents/xfido/pdf
    @
-} 
pdfMain::Connection -> String -> IO BS.ByteString
pdfMain conn pdfFilePath = do
        texMathFile <- getEnv "HOME" >>= \x -> return $ x </> "myfile/bitbucket/math"
        foundTable <- doesTableExistSqlite conn _PDFTABLE
        fw "foundTable"
        logFileG [pdfFilePath]
        pp $ "foundTable=" <<< foundTable
        if not foundTable then do
          logFileG ["pdfMain: " ++ (toStr _PDFTABLE) ++ " is NOT FOUND"]
          -- NOTE: 15-02-2021
          -- disable reading "\\.tex$" file, openFile => resource exhausted, too many open files
          -- allTexFile <- lsRegexFull texMathFile "\\.tex$"
          let allTexFile = []
          -- allPDFFile <- lsRegexFull texMathFile "\\.pdf$"
          let allPDFFile = []
          wwwPDFFile <- lsRegexFull pdfFilePath "\\.pdf$"
          let utexFile = unique allTexFile
          let updfFile = unique (allPDFFile ++ wwwPDFFile)
          -- dropTableSqlite conn _PDFTABLE 

          createNewTable conn _PDFTABLE 
          insertTexToTable conn utexFile 
          insertPDFToTable conn updfFile
        else do
          logFileG ["pdfMain: found Table"]
        queryDBToHtml conn _PDFTABLE "pdf/"



getPDFPath::String -> IO String
getPDFPath s = return s

{-|
   === test only, use 'getPDFPath'
-}
getPDFPath_t::String -> IO String
getPDFPath_t s = getEnv "tt" >>= \p -> return $ p </> "spdf/"

-- insertFileToDatabase::Connection -> String -> IO ()
-- insertFileToDatabase conn p = do
        -- texMathFile <- getEnv "HOME" >>= \x -> return $ x </> "myfile/bitbucket/math"
        -- let pdfFilePath = "./pdf"                              

        -- foundTable <- doesTableExistSqlite conn _PDFTABLE
        -- fw "foundTable"
        -- pp $ "foundTable=" <<< foundTable
        -- if not foundTable then do
          
          -- 15-02-2021
          -- disable reading "\\.tex$" file, openFile => resource exhausted, too many open files
          -- allTexFile <- lsRegexFull texMathFile "\\.tex$"
          -- let allTexFile = []
          -- allPDFFile <- lsRegexFull texMathFile "\\.pdf$"
          -- let allPDFFile = []
          -- wwwPDFFile <- lsRegexFull pdfFilePath "\\.pdf$"
          -- let utexFile = unique allTexFile
          -- let updfFile = unique (allPDFFile ++ wwwPDFFile)
          -- dropTableSqlite conn _PDFTABLE 

          -- createNewTable conn _PDFTABLE 
          -- insertTexToTable conn utexFile 
          -- insertPDFToTable conn updfFile
        -- else
        -- print "found Table"

-- main = do
--     bs <- pdfMain
--     print bs
