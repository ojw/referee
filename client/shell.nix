with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "big-template-client";
  ghc = haskell.packages.ghcjs.ghc;
  buildInputs = [ ncurses # For intero
                  git # To enable git packages in stack.yaml
                  cabal-install # For stack solver
                  # the below seem to be required to build ghcjs, maybe?
                  zlib
                  cairo
                  pango
                  gmp
                  haskellPackages.happy
                  # cacert? didn't fix my git problems :(
                ];
  GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt; # otherwise git clone fails :/

}
