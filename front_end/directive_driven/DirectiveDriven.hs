{-# LANGUAGE TemplateHaskell #-}

{-|

One popular approach to parallelizing programs and even programming
Accelerators is the "directive driven" approach embodied by OpenMP and
OpenAcc.  See this workshop, for example:

  http://www.openacc.org/WACCPD14

This is a competing approach to DSLs, which instead use customized
languages, rather than directive-annoted versions of general purpose
languages.  Or is it?  Typically, only a subset of the general purpose
language will run on, e.g., the GPU.  Thus the same language
subsetting problem arrises.

In this sketch we have a peek at what the directive-driven approach
may look like in Haskell.

-}

module Main where

import Helpers.Annotations

-- First we want to define a function that is usable in the permitted
-- subset.  Let's say, usable on GPU or on embedded platforms that
-- don't allow allocation.
{-# ANN foo [GPU,Embedded] #-}
foo :: Double -> Double
foo x = cos x + 3 * x

-- Next, ideally we would like to use Template Haskell's `reify`
-- feature to grab an AST representing the definition of `foo`.
-- Unfortunately, this functionality is currently incomplete and only
-- good for type-level introspection, not term-level.

-- $(mkGPUFun 'foo)

-- We do have recourse.  We can instead use one of the options:

-- OPTION (1): Use haskell-src-exts, which can parse whole modules, to
-- read in the abstract syntax, including the definition of `foo`.

-- OPTION (2): Use a GHC plugin compiler pass.  It will receive the
-- program's intermediate representation, which includes the source
-- annotation, "ANN", above.

-- OPTION (3): We can still use Template Haskell, if we put the
-- definitions in question directly within a quotation.  Note here
-- that quotations can include entire declarations as well as
-- exprressions:

$(myLangDefs
  [d|

    data MyDat = MyDat Int

    {-# ANN z [GPU,Embedded] #-}
    z :: Integer
    z = 34

    |])

-- In this case the package "th-desugar" (used by singletons) may come
-- in handy to reduce the number of constructs we need to deal with.

main = return ()
