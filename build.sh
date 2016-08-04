#! /usr/bin/env sh

stack build --stack-yaml=server/server-stack.yaml

rm -f static/all.js
cp -r $(stack path --stack-yaml=client/client-stack.yaml --local-install-root)/bin/referee-client-exe.jsexe/all.js static/all.js

stack build --stack-yaml=client/client-stack.yaml
