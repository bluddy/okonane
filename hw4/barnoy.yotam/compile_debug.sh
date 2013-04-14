#!/bin/bash

if [ ! -d "./bin" ]; then mkdir bin; fi;
cd src; ocamlbuild MainGenetic.byte -tag debug -build-dir ../bin $@; cd ..
cd src; ocamlbuild MainEnt.byte -tag debug -build-dir ../bin $@; cd ..

