1. download .fasta & patient info meta data from GISAID.
2. run .fasta data under Nextclade through docker platform. thet you can get the Nextclade is an open-source project for viral genome alignment, mutation calling, clade assignment, quality checks and phylogenetic placement
3. merge both data through ready_to_mut_mod.ipynb
   -> this could make the integrated data file
4. run vcf_transfer.R to get a single rows with mutation position and integrated domain, and so on.
   -> apply dN/dS , source:https://github.com/im3sanger/dndscv
