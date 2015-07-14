{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Helpers.Annotations where

import Data.Data
import Language.Haskell.TH

data Where = CPU | GPU | Embedded
  deriving (Show, Eq, Read, Ord, Enum, Bounded, Typeable, Data)


mkGPUFun :: Name -> ExpQ
mkGPUFun _ =
  [| error "This stub is not implemented for the demo." |]

myLangDefs :: Q [Dec] -> Q [Dec]
myLangDefs x = do x' <- x
                  runIO $ do putStrLn "\nmyLangDefs (stub): would make definitions out of these:"
                             putStrLn $ pprint x' ++ "\n"
                  return x'
