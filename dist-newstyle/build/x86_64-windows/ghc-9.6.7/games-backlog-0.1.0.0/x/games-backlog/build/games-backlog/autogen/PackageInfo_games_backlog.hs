{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_games_backlog (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "games_backlog"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Um simples backlog para registrar games"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
