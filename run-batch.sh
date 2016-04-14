#!/bin/bash

# fuundoc1
prefix=$(ruby tools/create-call.rb init fuundoc1 terminate)
bin/dna_to_rna ${prefix} < endo.dna | bin/rna_to_image -o images/fuundoc1.png

# fuundoc2
prefix=$(ruby tools/create-call.rb init fuundoc2 terminate)
bin/dna_to_rna ${prefix} < endo.dna | bin/rna_to_image -o images/fuundoc2.png

# fuundoc3
prefix=$(ruby tools/create-call.rb init fuundoc3 terminate)
bin/dna_to_rna ${prefix} < endo.dna | bin/rna_to_image -o images/fuundoc3.png
