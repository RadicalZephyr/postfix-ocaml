#!/usr/bin/env bash

set -e

add-apt-repository ppa:avsm/ppa

apt-get update

# Install opam and ocaml
apt-get install -y ocaml ocaml-native-compilers camlp4-extra opam

# Install llvm 3.5
apt-get install -y llvm-3.5 llvm-3.5-dev llvm-3.5-runtime llvm-3.5-doc llvm-3.5-tools llvm-3.5-examples

opam init --no-setup --compiler 4.02.1

eval `opam config env`

opam update

opam install -y core core_extended utop llvm
