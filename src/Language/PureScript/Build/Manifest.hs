-- | Queries etc. for the package manifest database
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PureScript.Build.Manifest where

import Prelude.Compat

import Text.RawString.QQ (r)
import Data.String (IsString, fromString)
import Database.SQLite.Simple hiding (Query)
import qualified Database.SQLite.Simple as SQLite

import Language.PureScript.Names
import Language.PureScript.Build.Types

-- | Attach a phantom type to a query to know what it takes and returns
newtype Query input output = Query {unQuery :: SQLite.Query}
  deriving (Show, Eq, IsString)

createPackageTableQ :: Query () ()
createPackageTableQ = fromString [r|
CREATE TABLE package (
  -- Name of the package. An empty string refers to the current package.
  name TEXT NOT NULL UNIQUE,
  -- TODO other table. Package signature, made of the hashes of all its modules.
  -- hash BLOB NOT NULL,
  -- Path to root source directory (optional)
  root TEXT NOT NULL
);
|]

createModuleMetaTableQ :: Query () ()
createModuleMetaTableQ =  fromString [r|
CREATE TABLE module_meta (
  name TEXT NOT NULL,
  -- Path is null if there's no source code available
  path TEXT UNIQUE,
  timestamp TEXT -- timestamp at time of hash (null if not yet loaded)
);
|]

createModuleDependsTableQ :: Query () ()
createModuleDependsTableQ = fromString [r|
CREATE TABLE module_depends (
  module_meta INT NOT NULL,
  depends INT NOT NULL,
  FOREIGN KEY (module_meta) REFERENCES module_meta(rowid) ON DELETE CASCADE,
  FOREIGN KEY (depends) REFERENCES module_meta(rowid) ON DELETE CASCADE,
  UNIQUE (module_meta, depends)
);
|]

createPackageModuleMetaTableQ :: Query () ()
createPackageModuleMetaTableQ = fromString [r|
CREATE TABLE package_module_meta (
  package INT NOT NULL,
  module_meta INT NOT NULL,
  FOREIGN KEY (package) REFERENCES package(rowid) ON DELETE CASCADE,
  FOREIGN KEY (module_meta) REFERENCES module_meta(rowid) ON DELETE CASCADE,
  UNIQUE (package, module_meta)
);
|]

createPackageRecordViewQ :: Query () ()
createPackageRecordViewQ = fromString [r|
CREATE VIEW package_dependency_record
AS SELECT
  package.name as package_name,
  package.hash,
  mm1.name,
  mm1.path,
  mm1.hash,
  dep_pkg.name,
  mm2.name
FROM
-- Start with packages
package
-- Add modules of those packages
INNER JOIN package_module_meta AS pmm ON pmm.package = package.rowid
-- Add metadata of those modules
INNER JOIN module_meta AS mm1 ON pmm.module_meta = mm1.rowid
-- Add module dependencies
INNER JOIN module_depends ON module_depends.module_meta = mm1.rowid
-- Resolve names of module dependencies
INNER JOIN module_meta AS mm2 ON module_depends.depends = mm2.rowid
-- Resolve package names of module dependencies
INNER JOIN package_module_meta AS pmm2 ON pmm2.module_meta = mm2.rowid
INNER JOIN package AS dep_pkg ON pmm2.package = dep_pkg.rowid
|]

createModulePathHashViewQ :: Query () ()
createModulePathHashViewQ = fromString [r|
CREATE VIEW module_full_meta
AS SELECT
  module_meta.rowid as module_id,
  package.name as package_name,
  module_meta.path as path,
  module_meta.timestamp as module_stamp
FROM module_meta
INNER JOIN package_module_meta AS pmm ON pmm.module_meta = module_meta.rowid
INNER JOIN package ON pmm.package = package.rowid;
|];

----------------- INSERTS


insertModuleQ :: Query (PackageRef, ModuleName, ModuleStamp) ()
insertModuleQ = fromString [r|
INSERT INTO module_meta (package, name, stamp) VALUES (
  SELECT rowid FROM package WHERE package.name = ?,
  ?, ?
)
|]

insertModuleDependsListQ :: Query (ModuleId, ModuleStamp) ()
insertModuleDependsListQ = "TODO"

insertModuleDependsQ :: Query (ModuleId, ModuleId) ()
insertModuleDependsQ = fromString [r|
INSERT INTO module_depends (module_dep_list, depends) VALUES (
  SELECT rowid FROM module_deps_lists WHERE module_meta = ?,
  ?
)
|]

insertPackageQ :: Query (PackageRef, FilePath) ()
insertPackageQ = "INSERT INTO package (name, path) VALUES (?, ?)"



----------------- GETS

-- Get the dependency list meta (TODO rename this, maybe modulesignature?)
getModuleStampQ :: Query ModuleId (Only ModuleStamp)
getModuleStampQ = "TODO"

-- Get dependencies of a module. Pairs them with timestamps
getModuleDependsQ :: Query ModuleId (ModuleId, PackageRef, ModuleName)
getModuleDependsQ = "TODO"

getPackageModulesFromIdQ :: Query PackageId (ModuleName, ModuleId)
getPackageModulesFromIdQ = "TODO"

getPackageModulesQ :: Query (Only PackageRef) (ModuleName, ModuleId)
getPackageModulesQ = "TODO"

getPackageRootQ :: Query PackageId (Only FilePath)
getPackageRootQ = "TODO"

getPackageIdQ :: Query (Only PackageRef) (PackageId, FilePath)
getPackageIdQ = "SELECT rowid, root FROM package WHERE name = ?"

getPackageRecordQ
  :: Query (Only PackageId)
       (PackageRef, PackageHash, ModuleName, FilePath,
        PackageRef, ModuleName)
getPackageRecordQ = "SELECT * FROM package_dependency_record WHERE package_id = ?"

getModuleMetaFromNameQ :: Query (Only ModuleName) (ModuleId, PackageRef, FilePath)
getModuleMetaFromNameQ = "SELECT module_id, package_name, path FROM module_full_meta WHERE name = ?"

getModuleMetaFromResolvedQ :: Query ResolvedModuleRef (Only FilePath)
getModuleMetaFromResolvedQ = fromString [r|
SELECT path FROM module_full_meta
WHERE package_name = ? AND module_name = ?
|]
