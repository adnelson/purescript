-- |
-- Case binders
--
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.PureScript.AST.Binders where

import Prelude.Compat
import Control.Monad.Identity (runIdentity)
import Lens.Micro.Platform

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.Comments
import Language.PureScript.Types

-- |
-- Data type for binders
--
data Binder' a
  -- |
  -- Wildcard binder
  --
  = NullBinder
  -- |
  -- A binder which matches a literal
  --
  | LiteralBinder a (Literal (Binder' a))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder a Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder a (Qualified (ProperName 'ConstructorName)) [Binder' a]
  -- |
  -- A operator alias binder. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | OpBinder a (Qualified (OpName 'ValueOpName))
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | BinaryNoParensBinder (Binder' a) (Binder' a) (Binder' a)
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder (Binder' a)
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder a Ident (Binder' a)
  -- |
  -- A binder with source position information
  --
  | PositionedBinder a [Comment] (Binder' a)
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder SourceType (Binder' a)
  deriving (Show, Functor, Foldable, Traversable)

-- Manual Eq and Ord instances for `Binder` were added on 2018-03-05. Comparing
-- the `SourceSpan` values embedded in some of the data constructors of `Binder`
-- was expensive. This made exhaustiveness checking observably slow for code
-- such as the `explode` function in `test/purs/passing/LargeSumTypes.purs`.
-- Custom instances were written to skip comparing the `SourceSpan` values. Only
-- the `Ord` instance was needed for the speed-up, but I did not want the `Eq`
-- to have mismatched behavior.
instance Eq (Binder' a) where
  (==) NullBinder NullBinder = True
  (==) NullBinder _ = False

  (==) (LiteralBinder _ lb) (LiteralBinder _ lb') = (==) lb lb'
  (==) LiteralBinder{} _ = False

  (==) (VarBinder _ ident) (VarBinder _ ident') = (==) ident ident'
  (==) VarBinder{} _ = False

  (==) (ConstructorBinder _ qpc bs) (ConstructorBinder _ qpc' bs') =
    (==) qpc qpc' && (==) bs bs'
  (==) ConstructorBinder{} _ = False

  (==) (OpBinder _ qov) (OpBinder _ qov') =
    (==) qov qov'
  (==) OpBinder{} _ = False

  (==) (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    (==) b1 b1' && (==) b2 b2' && (==) b3 b3'
  (==) BinaryNoParensBinder{} _ = False

  (==) (ParensInBinder b) (ParensInBinder b') =
    (==) b b'
  (==) ParensInBinder{} _ = False

  (==) (NamedBinder _ ident b) (NamedBinder _ ident' b') =
    (==) ident ident' && (==) b b'
  (==) NamedBinder{} _ = False

  (==) (PositionedBinder _ comments b) (PositionedBinder _ comments' b') =
    (==) comments comments' && (==) b b'
  (==) PositionedBinder{} _ = False

  (==) (TypedBinder ty b) (TypedBinder ty' b') =
    (==) ty ty' && (==) b b'
  (==) TypedBinder{} _ = False

instance Ord (Binder' a) where
  compare NullBinder NullBinder = EQ
  compare NullBinder _ = LT

  compare (LiteralBinder _ lb) (LiteralBinder _ lb') = compare lb lb'
  compare LiteralBinder{} NullBinder = GT
  compare LiteralBinder{} _ = LT

  compare (VarBinder _ ident) (VarBinder _ ident') = compare ident ident'
  compare VarBinder{} NullBinder = GT
  compare VarBinder{} LiteralBinder{} = GT
  compare VarBinder{} _ = LT

  compare (ConstructorBinder _ qpc bs) (ConstructorBinder _ qpc' bs') =
    compare qpc qpc' <> compare bs bs'
  compare ConstructorBinder{} NullBinder = GT
  compare ConstructorBinder{} LiteralBinder{} = GT
  compare ConstructorBinder{} VarBinder{} = GT
  compare ConstructorBinder{} _ = LT

  compare (OpBinder _ qov) (OpBinder _ qov') =
    compare qov qov'
  compare OpBinder{} NullBinder = GT
  compare OpBinder{} LiteralBinder{} = GT
  compare OpBinder{} VarBinder{} = GT
  compare OpBinder{} ConstructorBinder{} = GT
  compare OpBinder{} _ = LT

  compare (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    compare b1 b1' <> compare b2 b2' <> compare b3 b3'
  compare BinaryNoParensBinder{} ParensInBinder{} = LT
  compare BinaryNoParensBinder{} NamedBinder{} = LT
  compare BinaryNoParensBinder{} PositionedBinder{} = LT
  compare BinaryNoParensBinder{} TypedBinder{} = LT
  compare BinaryNoParensBinder{} _ = GT

  compare (ParensInBinder b) (ParensInBinder b') =
    compare b b'
  compare ParensInBinder{} NamedBinder{} = LT
  compare ParensInBinder{} PositionedBinder{} = LT
  compare ParensInBinder{} TypedBinder{} = LT
  compare ParensInBinder{} _ = GT

  compare (NamedBinder _ ident b) (NamedBinder _ ident' b') =
    compare ident ident' <> compare b b'
  compare NamedBinder{} PositionedBinder{} = LT
  compare NamedBinder{} TypedBinder{} = LT
  compare NamedBinder{} _ = GT

  compare (PositionedBinder _ comments b) (PositionedBinder _ comments' b') =
    compare comments comments' <> compare b b'
  compare PositionedBinder{} TypedBinder{} = LT
  compare PositionedBinder{} _ = GT

  compare (TypedBinder ty b) (TypedBinder ty' b') =
    compare ty ty' <> compare b b'
  compare TypedBinder{} _ = GT

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder' a -> [Ident]
binderNames = go []
  where
  go ns (LiteralBinder _ b) = lit ns b
  go ns (VarBinder _ name) = name : ns
  go ns (ConstructorBinder _ _ bs) = foldl go ns bs
  go ns (BinaryNoParensBinder b1 b2 b3) = foldl go ns [b1, b2, b3]
  go ns (ParensInBinder b) = go ns b
  go ns (NamedBinder _ name b) = go (name : ns) b
  go ns (PositionedBinder _ _ b) = go ns b
  go ns (TypedBinder _ b) = go ns b
  go ns _ = ns
  lit ns (ObjectLiteral bs) = foldl go ns (map snd bs)
  lit ns (ArrayLiteral bs) = foldl go ns bs
  lit ns _ = ns

isIrrefutable :: Binder' a -> Bool
isIrrefutable NullBinder = True
isIrrefutable (VarBinder _ _) = True
isIrrefutable (PositionedBinder _ _ b) = isIrrefutable b
isIrrefutable (TypedBinder _ b) = isIrrefutable b
isIrrefutable _ = False
