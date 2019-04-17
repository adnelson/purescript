-- | Common code generation utility functions
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PureScript.CodeGen.JS.Common (
  JsIdent,
  jsIdentToText,
  jsIdentToPSString,
  freshJsIdent,
  moduleNameToJs,
  properToJs,
  identToJs,
  runIdentToJs,
  isValidJsIdentifier,
  unsafeMap,
  jsIdentFromText,
  rawIdentToJsIdent,
  ) where

import Prelude.Compat

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString)

import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.ReservedWords
import Control.Monad.Supply.Class (MonadSupply, freshName)
import Language.PureScript.PSString as PSString

-- | Abstract, representing a valid javascript identifier.

-- Can be constructed via 'properToJs' or 'identToJs'.
newtype JsIdent = JsIdent Text deriving (Show, Eq, Ord, IsString)

-- | Get the text out of the identifier.
jsIdentToText :: JsIdent -> Text
jsIdentToText (JsIdent txt) = txt

-- | Safely construct a JS identifier.
jsIdentFromText :: Text -> Maybe JsIdent
jsIdentFromText name
  | nameIsJsReserved name = Just $ JsIdent $ "$$" <> name
  | not (isValidJsIdentifier name) = Nothing
  | otherwise = Just $ JsIdent name

-- | Create a new unique identifier in the Supply monad.
freshJsIdent :: MonadSupply m => m JsIdent
freshJsIdent = rawIdentToJsIdent <$> freshName

-- | Modify the text content. Obviously this should be used with caution.
unsafeMap :: (Text -> Text) -> JsIdent -> JsIdent
unsafeMap f (JsIdent txt) = JsIdent (f txt)

-- | Convert an identifier to a PureScript string.
jsIdentToPSString :: JsIdent -> PSString
jsIdentToPSString = PSString.mkString . jsIdentToText

-- | Construct a JavaScript identifier from characters to be found in
-- | a PureScript identifier.
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
-- Note that this function assumes that the argument is a valid PureScript
-- identifier (either an 'Ident' or a 'ProperName') to begin with; as such it
-- will not produce valid JavaScript identifiers if the argument e.g. begins
-- with a digit. For this reason it's not exported in favor of
-- 'identToJs' or 'properToJs' below.
rawIdentToJsIdent :: Text -> JsIdent
rawIdentToJsIdent name = do
  let converted = T.concatMap identCharToText name
  case jsIdentFromText converted of
    Just ident -> ident
    _ -> internalError $
      "Converted purescript identifier " ++ show name ++
      " to " ++ show converted ++ ", which is not a valid JS identifier"

-- | Convert a purescript module name to an identifier.
moduleNameToJs :: ModuleName -> JsIdent
moduleNameToJs (ModuleName pns) = rawIdentToJsIdent name
  where name = T.intercalate "_" (runProperName `map` pns)


-- | Convert an 'Ident' into a valid JavaScript identifier. This will
-- fail if it does not have the 'Ident' constructor, so use with caution.
identToJs :: Ident -> JsIdent
identToJs (Ident i) = rawIdentToJsIdent i
identToJs ident = internalError ("Expected an Ident, but got " ++ show ident)


-- | Convert an ident to JS after "running" it -- this should not fail
runIdentToJs :: Ident -> JsIdent
runIdentToJs = rawIdentToJsIdent . runIdent


-- | Convert a 'ProperName' into a valid JavaScript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
properToJs :: ProperName a -> JsIdent
properToJs = rawIdentToJsIdent . runProperName

-- | Test if a string is a valid JavaScript identifier as-is. Note that, while
-- a return value of 'True' guarantees that the string is a valid JS
-- identifier, a return value of 'False' does not guarantee that the string is
-- not a valid JS identifier. That is, this check is more conservative than
-- absolutely necessary.
isValidJsIdentifier :: Text -> Bool
isValidJsIdentifier "" = False
isValidJsIdentifier ident | nameIsJsReserved ident = False
isValidJsIdentifier ident = validStart && allValidChars where
  allValidChars = T.all (\c -> c == '_' || c == '$' || isAlphaNum c) ident
  validStart = let hd = T.head ident in hd == '_' || hd == '$' || isAlpha hd

-- | Attempts to find a javascript-compatible replacement for
-- characters found in symbols. If none has been specified returns the
-- ordinal value.
identCharToText :: Char -> Text
identCharToText c | isAlphaNum c = T.singleton c
identCharToText '_' = "_"
identCharToText '.' = "$dot"
identCharToText '$' = "$dollar"
identCharToText '~' = "$tilde"
identCharToText '=' = "$eq"
identCharToText '<' = "$less"
identCharToText '>' = "$greater"
identCharToText '!' = "$bang"
identCharToText '#' = "$hash"
identCharToText '%' = "$percent"
identCharToText '^' = "$up"
identCharToText '&' = "$amp"
identCharToText '|' = "$bar"
identCharToText '*' = "$times"
identCharToText '/' = "$div"
identCharToText '+' = "$plus"
identCharToText '-' = "$minus"
identCharToText ':' = "$colon"
identCharToText '\\' = "$bslash"
identCharToText '?' = "$qmark"
identCharToText '@' = "$at"
identCharToText '\'' = "$prime"
identCharToText c = '$' `T.cons` T.pack (show (ord c))
