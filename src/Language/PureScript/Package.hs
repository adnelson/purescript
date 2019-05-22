-- | Basic types for packages
{-# LANGUAGE DeriveGeneric #-}
module Language.PureScript.Package where

import Data.Aeson
import GHC.Generics (Generic)
import Prelude.Compat
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Language.PureScript.Names

data PackageRef
  = LocalPackage
  | DepPackage !PackageName
  deriving (Show, Eq, Ord, Generic)

instance ToJSON PackageRef
instance FromJSON PackageRef

prettyPackageRef :: PackageRef -> String
prettyPackageRef LocalPackage = "<<local package>>"
prettyPackageRef (DepPackage (PackageName n)) = T.unpack n

instance ToRow PackageRef where
  toRow = toRow . Only

instance ToField PackageRef where
  toField = \case
    LocalPackage -> toField ("/LOCAL/" :: T.Text)
    DepPackage pname -> toField pname

instance FromField PackageRef where
  fromField f = fromField f >>= \case
    "/LOCAL/" -> pure LocalPackage
    pname -> pure $ DepPackage $ PackageName pname
