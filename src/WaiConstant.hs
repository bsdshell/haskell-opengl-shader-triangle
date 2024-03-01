{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes       #-} -- RawString QQ, Quasi 

module WaiConstant where
import Control.Monad
import Control.Concurrent
import Data.Char 
-- import Data.List
import qualified Data.List as L
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
import Debug.Trace (trace)
import Text.Pretty.Simple (pPrint)

import Data.Array  -- TODO: change to DR
import qualified Data.Array as DR 
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as M
import qualified Text.Regex.TDFA     as TD
import qualified Text.Regex          as TR
import qualified Data.Set            as DS

import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.Char8 as LC8 
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.ByteString           as BS
import qualified Data.Text.Lazy            as TL
import qualified Data.Text                 as TS
import qualified Data.Text.Encoding        as TSE
import qualified Data.Text.Lazy.Encoding   as TLE
import qualified Data.ByteString.Char8     as S8 (putStrLn, putStr)   -- strict ?

-- {-# LANGUAGE QuasiQuotes       #-} -- RawString QQ, Quasi 
import Text.RawString.QQ          -- QuasiQuotes needs for Text.RawString.QQ 

-------------------------------------------------------------------------------- 
-- | How to build AronHtml.hs 
-- | ghc --make AronHtml.hs
-------------------------------------------------------------------------------- 

port = 8080 :: Int 
host = "http://localhost"

hostURL = "http://localhost:" ++ (show port)
