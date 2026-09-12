# IMO-26-Qin
Underrecognized Q Fever and Rickettsial Diseases Across China
This repository contains the source data and analysis scripts used to generate the main figures in the manuscript:

Underrecognized Q Fever and Rickettsial Diseases Across China

Repository Structure
QFever-Rickettsial-China/
├── README.md
├── scripts/
│   ├── Figure1A.R
│   ├── Figure1B-F.R
│   └── Figure2_phylogeny.sh
└── source_data/
    ├── Figure1/
    │   ├── provincereshape2.xlsx
    │   ├── provincecases.xlsx
    │   └── provinceinfectionlevel.xlsx
    └── Figure2/
        ├── case1.fasta
        ├── case2.fasta
        ├── case3.fasta
        ├── case4.fasta
        ├── case1.tree
        ├── case2.tree
        ├── case3.tree
        ├── case4.tree
        └── reference_genomes.tsv
Description
Figure 1
Figure1A.R generates Figure 1A using provincereshape2.xlsx.
Figure1B-F.R generates Figure 1B–F using provincecases.xlsx and provinceinfectionlevel.xlsx.
Figure 2
The Figure2 folder contains:

FASTA sequence files used for phylogenetic analysis.
Phylogenetic tree files.
Reference genome accession list (reference_genomes.tsv).
The analysis workflow is provided in Figure2_phylogeny.sh.

Software
R
IQ-TREE
MAFFT
Citation
If you use the data or scripts from this repository, please cite the associated publication.

Contact
Please contact the corresponding authors of the manuscript for questions regarding the data or analysis.
