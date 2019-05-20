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
import Language.PureScript.Externs
import Language.PureScript.Build.Types

-- | Name of the file to use for the manifest SQLite database.
manifestFileName :: FilePath
manifestFileName = "ps-manifest.db"

-- | Attach a phantom type to a query to know what it takes and returns
newtype Query input output = Query {unQuery :: SQLite.Query}
  deriving (Show, Eq, IsString)

createPackageTableQ :: Query () ()
createPackageTableQ = fromString [r|
CREATE TABLE IF NOT EXISTS package (
  -- Name of the package. An empty string refers to the current package.
  name TEXT NOT NULL UNIQUE,
  -- Path to root source directory (optional)
  root TEXT NOT NULL
);
|]

createModuleMetaTableQ :: Query () ()
createModuleMetaTableQ =  fromString [r|
CREATE TABLE IF NOT EXISTS module (
  -- modules must belong to a package, this is its ID.
  package INT NOT NULL,
  -- Name of the module. E.g. 'Data.Foo'
  name TEXT NOT NULL,
  -- TODO: Path to the source code file, to support nonstandard module paths
  -- path TEXT UNIQUE NOT NULL,
  -- Latest recorded timestamp of the file path. Null if not yet loaded.
  file_stamp TEXT,
  -- Latest recorded compilation timestamp. Null if not yet compiled.
  -- A compilation stamp without an externs file indicates a failed compilation.
  externs_stamp TEXT,
  -- externs for this module (JSON). Null if not yet compiled.
  externs BLOB,
  FOREIGN KEY (package) REFERENCES package(rowid)
);
|]

createModuleDependsTableQ :: Query () ()
createModuleDependsTableQ = fromString [r|
CREATE TABLE IF NOT EXISTS module_depends (
  module INT NOT NULL,
  depends INT NOT NULL,
  FOREIGN KEY (module) REFERENCES module(rowid) ON DELETE CASCADE,
  FOREIGN KEY (depends) REFERENCES module(rowid) ON DELETE CASCADE,
  UNIQUE (module, depends)
);
|]

createModuleDependsViewQ :: Query () ()
createModuleDependsViewQ = fromString [r|
CREATE VIEW IF NOT EXISTS module_depends_view AS
SELECT
  md.module as mod_id,
  dep_mod.rowid as dep_id,
  dep_pkg.name as dep_pkg,
  dep_mod.name as dep_name
FROM module_depends as md
INNER JOIN module as dep_mod ON depends = dep_mod.rowid
INNER JOIN package as dep_pkg ON dep_mod.package = dep_pkg.rowid
|]

createPackageRecordViewQ :: Query () ()
createPackageRecordViewQ = fromString [r|
CREATE VIEW IF NOT EXISTS package_dependency_record
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
INNER JOIN package_module AS pmm ON pmm.package = package.rowid
-- Add metadata of those modules
INNER JOIN module AS mm1 ON pmm.module = mm1.rowid
-- Add module dependencies
INNER JOIN module_depends ON module_depends.module = mm1.rowid
-- Resolve names of module dependencies
INNER JOIN module AS mm2 ON module_depends.depends = mm2.rowid
-- Resolve package names of module dependencies
INNER JOIN package_module AS pmm2 ON pmm2.module = mm2.rowid
INNER JOIN package AS dep_pkg ON pmm2.package = dep_pkg.rowid
|]

createModulePathHashViewQ :: Query () ()
createModulePathHashViewQ = fromString [r|
CREATE VIEW IF NOT EXISTS module_full_meta
AS SELECT
  module.rowid as module_id,
  package.name as package_name,
  module.path as path,
  module.file_stamp as module_stamp
FROM module
INNER JOIN package_module AS pmm ON pmm.module = module.rowid
INNER JOIN package ON pmm.package = package.rowid;
|];

----------------- MODIFYING OPERATIONS


insertModuleQ :: Query (PackageRef, ModuleName) ()
insertModuleQ = fromString [r|
INSERT INTO module (package, name) VALUES (
  (SELECT rowid FROM package WHERE package.name = ?),
  ?
)
|]

insertModuleStampQ :: Query (ModuleStamp, ModuleId) ()
insertModuleStampQ = "UPDATE module SET file_stamp = ? WHERE rowid = ?"

insertModuleDependsQ :: Query (ModuleId, ModuleId) ()
insertModuleDependsQ = "INSERT INTO module_depends (module, depends) VALUES (?, ?)"

insertPackageQ :: Query (PackageRef, FilePath) ()
insertPackageQ = "INSERT INTO package (name, root) VALUES (?, ?)"

insertModuleExternsQ :: Query (Maybe ExternsFile, ExternsStamp, ModuleId) ()
insertModuleExternsQ = "UPDATE module SET externs = ?, externs_stamp = ? WHERE rowid = ?"

removeModuleDependsQ :: Query ModuleId ()
removeModuleDependsQ = "DELETE FROM module_depends WHERE module = ?"

----------------- GETS

-- Get the dependency list meta (TODO rename this, maybe modulesignature?)
getModuleStampQ :: Query ModuleId (Only (Maybe ModuleStamp))
getModuleStampQ = "SELECT file_stamp FROM module WHERE rowid = ?"

-- Get the dependency list meta (TODO rename this, maybe modulesignature?)
getModuleExternsStampQ :: Query ModuleId (Only (Maybe ExternsStamp))
getModuleExternsStampQ = "SELECT externs_stamp FROM module WHERE rowid = ?"

-- Get the dependency list meta (TODO rename this, maybe modulesignature?)
getModuleExternsQ :: Query ModuleId (Only (Maybe ExternsFile))
getModuleExternsQ = "SELECT externs FROM module WHERE rowid = ?"

-- Get dependencies of a module (enough to construct a ResolvedModuleRef)
getModuleDependsQ :: Query ModuleId (ModuleId, PackageRef, ModuleName)
getModuleDependsQ = "SELECT dep_id, dep_pkg, dep_name FROM module_depends_view WHERE mod_id = ?"

-- Get dependencies of a module, as module IDs
getModuleDependsIdQ :: Query ModuleId ModuleId
getModuleDependsIdQ = "SELECT depends FROM module_depends WHERE module = ?"

getPackageModulesFromIdQ :: Query PackageId (ModuleName, ModuleId)
getPackageModulesFromIdQ = "SELECT name, rowid FROM module WHERE package = ?"

getPackageModulesQ :: Query (Only PackageRef) (ModuleName, ModuleId)
getPackageModulesQ = fromString [r|
SELECT name, rowid FROM module
WHERE package = (SELECT rowid FROM package WHERE name = ?)
|]

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
