module Main where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson
import Data.DeepSeq

import qualified Language.PureScript.CoreFn as CF

main :: IO ()
main = do
  Just (modl :: CF.Module CF.Ann) <- decode <$> BL8.readFile "good.json"
  deepseq (CF.optimizeCoreFn modl) (pure ())
