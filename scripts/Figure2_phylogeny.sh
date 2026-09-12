#!/bin/bash

# Example workflow for phylogenetic analysis using IQ-TREE v2

# Multiple sequence alignment
mafft --auto sequences.fasta > alignment.fasta

# Maximum-likelihood phylogenetic inference
iqtree2 \
    -s alignment.fasta \
    -m MFP \
    -bb 1000 \
    -nt AUTO