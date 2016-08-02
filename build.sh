#! /usr/bin/env sh

stack build --stack-yaml=server/server-stack.yaml
stack build --stack-yaml=client/client-stack.yaml

cp -r client/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/referee-client-exe/referee-client-exe.jsexe/* static/
