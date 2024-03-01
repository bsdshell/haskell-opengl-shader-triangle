{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module AronPoly where

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
import qualified Data.HashMap.Strict as M
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
import qualified Text.Regex as TR 
import Text.Printf
import Data.Array
import Debug.Trace (trace)
import qualified Text.Regex.TDFA as TD

import qualified Data.Set as DS
import qualified Data.Text as Text (unpack, strip, pack)
import qualified Data.Text.Lazy hiding (map) 

import qualified Data.Vector as V 

{-| 
    polynomial addition
-} 
(+:)::(Num a)=>[(a, a)] -> [(a, a)] -> [(a, a)]
(+:) [] _ = []
(+:) _ [] = []
(+:) (x:cs) (y:cy) = ((fst x) + (fst y), snd x) : (+:) cs cy 
