

PKGS = ./front_end/overloading/ \
       ./middle_end/GADT_transforms/Mini-Accelerate.cabal \
       ./middle_end/nanopass/course_example/nanopass-sample.cabal \
       ./middle_end/nanopass/exercise/nanopass-exercise.cabal \
       ./middle_end/syntactic/nanofeldspar.cabal

all:
	cabal install -j $(PKGS)
