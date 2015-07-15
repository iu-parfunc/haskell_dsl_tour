#!/bin/bash

set -xe

PKGS=" ./front_end/overloading/ \
       ./front_end/directive_driven/ \
       ./middle_end/nanopass/course_example/nanopass-sample.cabal \
       ./middle_end/nanopass/exercise/nanopass-exercise.cabal \
       ./middle_end/syntactic/nanofeldspar.cabal \
       ./middle_end/GADT_transforms/Mini-Accelerate.cabal \
       ./middle_end/multi-level_AST \

     "

cabal install -j $PKGS $@
