-- | Provides the ability to sort modules based on module dependencies
module Language.PureScript.ModuleDependencies
  ( sortModules
  , ModuleGraph
  ) where

import           Protolude hiding (head)

import           Data.Graph
import qualified Data.Set as S
import qualified Data.Map as M
import           Language.PureScript.AST
import qualified Language.PureScript.Constants as C
import           Language.PureScript.Crash
import           Language.PureScript.Errors hiding (nonEmpty)
import           Language.PureScript.Names
import           Language.PureScript.Externs

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
sortModules
  :: forall m a
   . MonadError MultipleErrors m
  => (ModuleName -> m (Maybe a))
  -> [Module]
  -> m ([Module], ModuleGraph, Map ModuleName a)
sortModules getExternalModule ms = do
    let mns = S.fromList $ map getModuleName ms
    (verts, externs) <- flip runStateT mempty $ parU ms (toGraphNode mns)
    ms' <- parU (stronglyConnComp verts) toModule
    let (graph, fromVertex, toVertex) = graphFromEdges verts
        moduleGraph = do (_, mn, _) <- verts
                         let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                             deps    = reachable graph v
                             toKey i = case fromVertex i of (_, key, _) -> key
                         return (mn, filter (/= mn) (map toKey deps))
    return (ms', moduleGraph, externs)
  where
    toGraphNode
      :: S.Set ModuleName -> Module
      -> StateT (Map ModuleName a) m (Module, ModuleName, [ModuleName])
    toGraphNode mns m@(Module _ _ mn ds _) = do
      let deps = ordNub (mapMaybe usedModules ds)
      locals <- fmap catMaybes $ parU deps $ \(dep, pos) -> do
        case S.member dep mns || dep `elem` C.primModules of
          True -> pure $ Just dep -- defined in local modules, or primitive
          False -> M.member dep <$> get >>= \case
            True -> pure Nothing
            False -> lift (getExternalModule dep) >>= \case
              Just externs -> modify (M.insert dep externs) >> pure Nothing
              _ -> throwError
                . addHint (ErrorInModule mn)
                . errorMessage' pos
                $ ModuleNotFound dep
      pure (m, getModuleName m, locals)

-- | Calculate a list of used modules based on explicit imports and qualified names.
usedModules :: Declaration -> Maybe (ModuleName, SourceSpan)
-- Regardless of whether an imported module is qualified we still need to
-- take into account its import to build an accurate list of dependencies.
usedModules (ImportDeclaration (ss, _) mn _ _) = pure (mn, ss)
usedModules _ = Nothing

-- | Convert a strongly connected component of the module graph to a module
toModule :: MonadError MultipleErrors m => SCC Module -> m Module
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC ms) =
  case nonEmpty ms of
    Nothing ->
      internalError "toModule: empty CyclicSCC"
    Just ms' ->
      throwError
        . errorMessage'' (fmap getModuleSourceSpan ms')
        $ CycleInModules (map (someModuleNamed . getModuleName) ms)
