#!/bin/bash

if [ ! -d "./bin" ]; then mkdir bin; fi
cd src; ocamlbuild MainGenetic.p.native -build-dir ../bin $@; cd ..
cd src; ocamlbuild MainEnt.p.native -build-dir ../bin $@; cd ..

