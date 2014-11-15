#!/usr/bin/env bash

set -e

apt-get update

# Install opam and llvm 3.5
apt-get install -y opam llvm-3.5 llvm-3.5-dev llvm-3.5-runtime llvm-3.5-doc llvm-3.5-tools llvm-3.5-examples

opam update

opam install core core_extended utop llvm
