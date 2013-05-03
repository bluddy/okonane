#!/bin/bash

if [ ! -d "./bin" ]; then mkdir bin; fi
cd src; ocamlbuild Main.native -build-dir ../bin $@; cd ..

