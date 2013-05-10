#!/usr/bin/ruby

exec ("./run.sh 'map ./data/L-track.txt set verbose_sim=true set verbose_sim_delay=10 set use_basis=true set basis_width=5 set basis_max_dist=5 set alpha=0.01 set annealing=60 make q i 1 save test.agent q'")

