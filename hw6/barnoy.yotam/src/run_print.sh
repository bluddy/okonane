#!/bin/bash
rm test_data.txt
ocamlrun -b bin/Print.byte $@
