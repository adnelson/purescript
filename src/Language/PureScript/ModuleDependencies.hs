-- | Provides the ability to sort modules based on module dependencies
module Language.PureScript.ModuleDependencies
  ( sortModules
  , ModuleGraph
  ) where

import           Protolude hiding (head)

import           Data.Graph
import qualified Data.Set as S
import           Language.PureScript.AST (Module(..), SourceSpan(..), Declaration(..), SimpleErrorMessage(..), getModuleName, getModuleSourceSpan, getModuleDeclarations, ErrorMessageHint(..))
import qualified Language.PureScript.Constants as C
import           Language.PureScript.Crash
import           Language.PureScript.Errors (MultipleErrors, errorMessage', errorMessage'', parU, addHint)
import           Language.PureScript.Names

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

sortModules
  :: forall m
   . MonadError MultipleErrors m
  => [Module]
  -> m ([Module], ModuleGraph)
sortModules =
  sortModules'
    getModuleName
    getModuleSourceSpan
    (catMaybes . map usedModules . getModuleDeclarations)

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
sortModules'
  :: forall m a
   . MonadError MultipleErrors m
  => (a -> ModuleName)
  -> (a -> SourceSpan)
  -> (a -> [(ModuleName, SourceSpan)])
  -> [a]
  -> m ([a], ModuleGraph)
sortModules' getModuleName getModuleSourceSpan getImports ms = do
    let
      mns = S.fromList $ map getModuleName ms
      toModule :: MonadError MultipleErrors m => SCC a -> m a
      toModule (AcyclicSCC m) = return m
      toModule (CyclicSCC ms) =
        case nonEmpty ms of
          Nothing ->
            internalError "toModule: empty CyclicSCC"
          Just ms' ->
            throwError
              . errorMessage'' (fmap getModuleSourceSpan ms')
              $ CycleInModules (map getModuleName ms)

    verts <- parU ms (toGraphNode mns)
    ms' <- parU (stronglyConnComp verts) toModule
    let (graph, fromVertex, toVertex) = graphFromEdges verts
        moduleGraph = do (_, mn, _) <- verts
                         let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                             deps    = reachable graph v
                             toKey i = case fromVertex i of (_, key, _) -> key
                         return (mn, filter (/= mn) (map toKey deps))
    return (ms', moduleGraph)
  where
    toGraphNode :: S.Set ModuleName -> a -> m (a, ModuleName, [ModuleName])
    toGraphNode mns mod = do
      let deps = ordNub (getImports mod)
      void . parU deps $ \(dep, pos) ->
        when (dep `notElem` C.primModules && S.notMember dep mns) .
          throwError
            . addHint (ErrorInModule $ getModuleName mod)
            . errorMessage' pos
            $ ModuleNotFound dep
      pure (mod, getModuleName mod, map fst deps)

-- | Calculate a list of used modules based on explicit imports and qualified names.
usedModules :: Declaration -> Maybe (ModuleName, SourceSpan)
-- Regardless of whether an imported module is qualified we still need to
-- take into account its import to build an accurate list of dependencies.
usedModules (ImportDeclaration (ss, _) mn _ _) = pure (mn, ss)
usedModules _ = Nothing
