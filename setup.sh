#! /usr/bin/env sh

# possibly also:

# nix-env -i ghcjs
# nix-env -f "<nixpkgs>" -iA haskell.packages.ghcjs.cabal-install
# wait wait, why would cabal-install compiled for ghcjs be important? that makes no sense
# should it be regular cabal?
# nix-env -f "<nixpkgs>" -A haskell.packages.ghc7103.cabal-install
# ???

# possibly...

# gotta `stack install happy` probably

stack setup --stack-yaml=server/server-stack.yaml --no-system-ghc
stack setup --stack-yaml=client/client-stack.yaml --no-system-ghc
