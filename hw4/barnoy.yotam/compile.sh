#!/bin/bash

if [ ! -d "./bin" ]; then mkdir bin; fi
cd src; ocamlbuild MainGenetic.native -build-dir ../bin $@; cd ..
cd src; ocamlbuild MainEnt.native -build-dir ../bin $@; cd ..

