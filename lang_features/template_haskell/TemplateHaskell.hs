{-# LANGUAGE TemplateHaskell #-}

{-| See wiki entry here:

  https://wiki.haskell.org/Template_Haskell#What_is_Template_Haskell.3F

 -}

module Main where

import Language.Haskell.TH
import Quotes


bar :: Int -> String
bar = $(foo)

quux :: Int
quux = $$baz

main :: IO ()
main = print "hi"
