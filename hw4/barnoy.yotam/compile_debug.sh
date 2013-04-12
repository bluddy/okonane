#!/bin/bash

if [ ! -d "./bin" ]; then mkdir bin; fi;
cd src; ocamlbuild MainGenetic.byte -build-dir ../bin $@; cd ..
cd src; ocamlbuild MainEnt.byte -build-dir ../bin $@; cd ..

