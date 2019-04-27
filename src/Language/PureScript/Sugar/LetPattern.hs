-- |
-- This module implements the desugaring pass which replaces patterns in let-in
-- expressions with appropriate case expressions.
--
module Language.PureScript.Sugar.LetPattern (desugarLetPatternModule) where

import Prelude.Compat

import Data.List (groupBy, concatMap)
import Data.Function (on)

import Language.PureScript.AST
import Language.PureScript.Crash

-- | Replace every @BoundValueDeclaration@ in @Let@ expressions with @Case@
-- expressions.
desugarLetPatternModule :: Module -> Module
desugarLetPatternModule (Module ss coms mn ds exts) = Module ss coms mn (map desugarLetPattern ds) exts

-- | Desugar a single let expression
desugarLetPattern :: Declaration -> Declaration
desugarLetPattern decl =
  let (f, _, _) = everywhereOnValues id replace id
  in f decl
  where
  replace :: Expr' -> Expr'
  replace (Let w ds e) = go w (partitionDecls ds) e
  replace other = other

  go :: WhereProvenance
     -- ^ Metadata about whether the let-in was a where clause
     -> [Either [Declaration] (SourceAnn, Binder, Expr)]
     -- ^ Declarations to desugar
     -> Expr
     -- ^ The original let-in result expression
     -> Expr'
  go _ [] e = eExpr e
  go w (Right ((pos, com), binder, boundE) : ds) e =
    PositionedValue com
      $ AnnExpr pos
      $ Case [boundE] [CaseAlternative [binder] [MkUnguarded $ AnnExpr pos $ go w ds e]]
  go w (Left ds:dss) e@(AnnExpr a _) = Let w ds (AnnExpr a $ go w dss e)

partitionDecls :: [Declaration] -> [Either [Declaration] (SourceAnn, Binder, Expr)]
partitionDecls = concatMap f . groupBy ((==) `on` isBoundValueDeclaration)
  where
    f ds@(d:_)
      | isBoundValueDeclaration d = map (Right . g) ds
    f ds = [Left ds]

    g (BoundValueDeclaration sa binder expr) = (sa, binder, expr)
    g _ = internalError "partitionDecls: the impossible happened."

isBoundValueDeclaration :: Declaration -> Bool
isBoundValueDeclaration BoundValueDeclaration{} = True
isBoundValueDeclaration _ = False
