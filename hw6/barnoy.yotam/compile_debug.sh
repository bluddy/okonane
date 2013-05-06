#!/bin/bash

if [ ! -d "./bin" ]; then mkdir bin; fi
cd src; ocamlbuild Main.byte -tag debug -build-dir '../bin' $@; cd ..

