#!/bin/bash

if [ ! -d "./bin" ]; then mkdir bin; fi
cd src
ocamlbuild Test.byte -tag debug -build-dir '../bin' $@
ocamlbuild Print.byte -tag debug -build-dir '../bin' $@
cd ..

