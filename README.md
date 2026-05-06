# Fold First, Ask Later: structure-informed function annotation of phage proteins

Fold First, Ask Later is a structure-informed phage protein annotation pipeline designed to integrate analyses across multiple [Foldseek](https://github.com/steineggerlab/foldseek)-compatible databases, extending and enhancing the structural annotation capabilities of [Phold](https://github.com/gbouras13/phold).

Fold First, Ask Later is currently under development, and should be used with caution. 

This branch currently expands Phold by:
* code support for FoldSeek database installs, and default installation of PDB and AlphaFold database (UniProt50, minimal version)
* code support for FoldSeek searches against the PDB and AlphaFold database by default (no processing of hits yet)
* code support for PDB (default enabled, can be disabled with --offline) and UniProt API calls (with tag --uniprot) to add additional protein information to search results from searches against the PDB and AlphaFold database 

## Installation

The best way to install `foldfirst` for now is by first installing [Phold](https://github.com/gbouras13/phold) as described by the authors and then installing the code in this repository in the same environment. The current `foldfirst` codebase is up-to-date with Phold v1.2.2, so the install instructions specifically install that version. 

### Step 1 - Phold install

You can install [Phold](https://github.com/gbouras13/phold) using conda via [miniforge](https://github.com/conda-forge/miniforge), as this will install [Foldseek](https://github.com/steineggerlab/foldseek) (the only non-Python dependency) along with the Python dependencies.

To install `phold` using [conda](https://github.com/conda-forge/miniforge):

```bash
conda create -n foldfirst_env -c conda-forge -c bioconda phold=1.2.2
```

To utilise `foldfirst` with GPU, a GPU compatible version of `pytorch` must be installed. By default conda will install a CPU-only version. 

If you have an NVIDIA GPU, please try:

```bash
conda create -n foldfirst_env -c conda-forge -c bioconda phold=1.2.2 pytorch=*=cuda*
```

If you have a Mac running an Apple Silicon chip (M1/M2/M3/M4), `foldfirst` should be able to use the GPU. Please try:

```bash
conda create -n foldfirst_env python==3.13  
conda activate foldfirst_env
conda install pytorch::pytorch torchvision torchaudio -c pytorch 
conda install -c conda-forge -c bioconda phold=1.2.2
```

If you are have a different non-NVIDIA GPU, or have trouble with `pytorch`, see [this link](https://pytorch.org) for more instructions. If you have an older version of CUDA installed, then you might find [this link useful](https://pytorch.org/get-started/previous-versions/).

### Step 2 - Fold First Ask Later install

After installing `phold`, you can now install the code stored in this repository in that environment.

```bash
git clone https://github.com/hannelorelongin/FoldFirstAskLater.git
cd FoldFirstAskLater
conda activate foldfirst_env
python -m pip install --upgrade pip
python -m pip install -e .
```

Once `foldfirst` is installed, to download and install the databases run:

```bash
foldfirst install -t 8
```

If you have an NVIDIA GPU and can take advantage of Foldseek's GPU acceleration, instead run

```bash
foldfirst install -t 8 --foldseek_gpu
```

* Note: You will need at least 170GB of free space (the `foldfirst` databases including ProstT5 are just over 166GB uncompressed).

## Quick start

The default way to run `foldfirst` is by using the `foldfirst run` command, which combines the `foldfirst predict` and `foldfirst compare` commands. By default, a GenBank format file is expected as input. We recommend adding `--foldseek_gpu` if you have a NVIDIA GPU available, if not, add `--cpu`. A GPU will significanlty speed up the `foldfirst predict` stage, and is required for large analyses.  

You can see more options to costumize and optimize your `foldfirst run` command by running `foldfirst run -h`.

## Usage

```bash
Usage: foldfirst [OPTIONS] COMMAND [ARGS]...

Options:
  -h, --help     Show this message and exit.
  -V, --version  Show the version and exit.

Commands:
  autotune          Determines optimal batch size for 3Di prediction with...
  citation          Print the citation(s) for this tool
  compare           Runs Foldseek vs Fold First Ask Later databases
  createdb          Creates foldseek DB from AA FASTA and 3Di FASTA input...
  install           Installs ProstT5 model and foldfirst databases
  plot              Creates Fold First Ask Later Circular Genome Plots
  predict           Uses ProstT5 to predict 3Di tokens - GPU recommended
  proteins-compare  Runs Foldseek vs Fold First Ask Later databases on...
  proteins-predict  Runs ProstT5 on a multiFASTA input - GPU recommended
  remote            Uses Foldseek API to run ProstT5 then Foldseek locally
  run               foldfirst predict then compare all in one - GPU...
```

```bash
Usage: foldfirst run [OPTIONS]

  foldfirst predict then compare all in one - GPU recommended

Options:
  -h, --help                     Show this message and exit.
  -V, --version                  Show the version and exit.
  -i, --input PATH               Path to input file in Genbank format or
                                 nucleotide FASTA format  [required]
  -o, --output PATH              Output directory   [default:
                                 output_foldfirst]
  -t, --threads INTEGER          Number of threads  [default: 1]
  -p, --prefix TEXT              Prefix for output files  [default: foldfirst]
  -d, --database TEXT            Specific path to installed foldfirst
                                 databases (Phold + PDB +
                                 AlphaFold/UniProt50-minimal)
  -f, --force                    Force overwrites the output directory
  --autotune                     Run autotuning to detect and automatically
                                 use best batch size for your hardware.
                                 Recommended only if you have a large dataset
                                 (e.g. thousands of proteins), or else
                                 autotuning will add rather than save runtime.
  --batch_size INTEGER           batch size for ProstT5.  [default: 1]
  --cpu                          Use cpus only.
  --omit_probs                   Do not output per residue 3Di probabilities
                                 from ProstT5. Mean per protein 3Di
                                 probabilities will always be output.
  --save_per_residue_embeddings  Save the ProstT5 embeddings per resuide in a
                                 h5 file
  --save_per_protein_embeddings  Save the ProstT5 embeddings as means per
                                 protein in a h5 file
  --mask_threshold FLOAT         Masks 3Di residues below this value of
                                 ProstT5 confidence for Foldseek searches
                                 [default: 25]
  --finetune                     Use gbouras13/ProstT5Phold encoder + CNN
                                 model both finetuned on phage proteins
  --vanilla                      Use vanilla CNN model (trained on CASP14)
                                 with ProstT5Phold encoder instead of the one
                                 trained on phage proteins
  --hyps                         Use this to only annotate hypothetical
                                 proteins from a Pharokka GenBank input
  -e, --evalue FLOAT             Evalue threshold for Foldseek  [default:
                                 1e-3]
  -s, --sensitivity FLOAT        Sensitivity parameter for FoldSeek  [default:
                                 9.5]
  --keep_tmp_files               Keep temporary intermediate files,
                                 particularly the large tsv files of all
                                 FoldSeek hits
  --card_vfdb_evalue FLOAT       Stricter E-value threshold for FoldSeek CARD
                                 and VFDB hits  [default: 1e-10]
  --separate                     Output separate GenBank files for each contig
  --max_seqs INTEGER             Maximum results per query sequence allowed to
                                 pass the prefilter. You may want to reduce
                                 this to save disk space for enormous datasets
                                 [default: 1000]
  --ultra_sensitive              Runs foldfirst with maximum sensitivity by
                                 skipping FoldSeek prefilter. Not recommended
                                 for large datasets.
  --extra_foldseek_params TEXT   Extra FoldSeek search params
  --custom_db TEXT               Path to custom database
  --foldseek_gpu                 Use this to enable compatibility with
                                 FoldSeek-GPU search acceleration
  --uniprot                      Use this to fetch up-to-date protein
                                 information from UniProt
  --offline                      Use this to run foldfirst in offline mode,
                                 not fetching any information through APIs.
  --restart                      Use this to restart foldfirst from
                                 'Processing FoldSeek output' after FoldSeek
                                 results tsv file is generated
  ```

## Output

* The primary outputs are:
  * `foldfirst_3di.fasta` containing the 3Di sequences for each CDS
  * `foldfirst_phold_per_cds_predictions.tsv` containing detailed annotation information on every CDS as obtained by Phold
  * `foldfirst_phold_all_cds_functions.tsv` containing counts per contig of CDS in each PHROGs category, VFDB, CARD, ACRDB and Defensefinder databases as obtained by Phold (similar to the `pharokka_cds_functions.tsv` from Pharokka)
  * `foldfirst_phold.gbk`, which contains a GenBank format file including the annotations obtained by Phold, and keeps any other genomic features (tRNA, CRISPR repeats, tmRNAs) included from the `pharokka` Genbank input file if provided
  * `pdb_database_hits.tsv`, which contains all FoldSeek hits against the PDB database for each CDS, supplemented by the corresponding protein annotations 
  * `af50m_database_hits.tsv`, which contains all FoldSeek hits against the AlphaFold database for each CDS, supplemented by the corresponding protein annotations 

## Acknowledgements

In Fold First, Ask Later, we adapted and extended functionality and documentation from:
* [Phold](https://github.com/gbouras13/phold) by [George Bouras](https://github.com/georgebouras) and collaborators, available under an [MIT License](https://github.com/gbouras13/phold/blob/main/LICENSE): phold protein annotation and structural analysis pipeline.

## References

While tool development is still ongoing, Fold First, Ask Later users can cite the original preprint, as well as Phold:
*  Longin H, Bouras G, Grigson SR, Edwards RA, Hendrix H, Lavigne R, van Noort V. "Fold first, ask later: structure-informed function prediction in Pseudomonas phages." Preprint at bioRxiv [https://doi.org/10.1101/2025.07.17.665397](https://doi.org/10.1101/2025.07.17.665397).
* Bouras G, Grigson SR, Mirdita M, Heinzinger M, Papudeshi B, Mallawaarachchi V, Green R, Kim SR, Mihalia V, Psaltis AJ, Wormald P-J, Vreugde S, Steinegger M, Edwards RA. "Protein Structure Informed Bacteriophage Genome Annotation with Phold." Nucleic Acids Research (2026) [https://doi.org/10.1093/nar/gkaf1448](https://doi.org/10.1093/nar/gkaf1448)

Please be sure to cite the following core dependencies - citing all bioinformatics tools that you use helps us, so helps you get better bioinformatics tools:

* Pharokka - (https://github.com/gbouras13/pharokka) Bouras G, Nepal R, Houtak G, Psaltis AJ, Wormald P-J, Vreugde S. "Pharokka: a fast scalable bacteriophage annotation tool." Bioinformatics (2023) [https://doi.org/10.1093/bioinformatics/btac776](https://doi.org/10.1093/bioinformatics/btac776)
* Foldseek - (https://github.com/steineggerlab/foldseek) [van Kempen M, Kim S, Tumescheit C, Mirdita M, Lee J, Gilchrist C, Söding J, and Steinegger M. "Fast and accurate protein structure search with Foldseek." Nature Biotechnology (2023) [doi:10.1038/s41587-023-01773-0](https://www.nature.com/articles/s41587-023-01773-0)
* ProstT5 - (https://github.com/mheinzinger/ProstT5) Heinzinger M,  Weissenow K, Gomez Sanchez J, Henkel A, Steinegger M, Rost B. "ProstT5: Bilingual language model for protein sequence and structure." NAR Genomics and Bioinformatics (2024) [doi:10.1101/2023.07.23.550085](https://doi.org/10.1093/nargab/lqae150) 
* Colabfold - (https://github.com/sokrypton/ColabFold) Mirdita M, Schütze K, Moriwaki Y, Heo L, Ovchinnikov S and Steinegger M. "ColabFold: Making protein folding accessible to all." Nature Methods (2022) [doi: 10.1038/s41592-022-01488-1 ](https://www.nature.com/articles/s41592-022-01488-1)
* PHROGs - (https://phrogs.lmge.uca.fr) Terzian P, Olo Ndela E, Galiez C, Lossouarn J, Pérez Bucio RE, Mom R, Toussaint A, Petit MA, Enault F. "PHROG : families of prokaryotic virus proteins clustered using remote homology." NAR Genomics and Bioinformatics (2021) [https://doi.org/10.1093/nargab/lqab067](https://doi.org/10.1093/nargab/lqab067)

Please also consider citing these supplementary databases where relevant:

* [CARD](https://card.mcmaster.ca) - Alcock BP, Huynh W, Chalil R, Smith KW, Raphenya AR, Wlodarski MA, Edalatmand A, Petkau A, Syed SA, Tsang KK, Baker SJC, Dave M, McCarthy MC, Mukiri KM, Nasir JA, Golbon B, Imtiaz H, Jiang X, Kaur K, Kwong M, Liang ZC, Niu KC, Shan P, Yang JYJ, Gray KL, Hoad GR, Jia B, Bhando T, Carfrae LA, Farha MA, French S, Gordzevich R, Rachwalski K, Tu MM, Bordeleau E, Dooley D, Griffiths E, Zubyk HL, Brown ED, Maguire F, Beiko RG, Hsiao WWL, Brinkman FSL, Van Domselaar G, McArthur AG. "CARD 2023: expanded curation, support for machine learning, and resistome prediction at the Comprehensive Antibiotic Resistance Database." Nucleic Acids Research (2022) [https://doi.org/10.1093/nar/gkac920](https://doi.org/10.1093/nar/gkac920)
* [VFDB](http://www.mgc.ac.cn/VFs/main.htm) - Chen L, Yang J, Yao Z, Sun L, Shen Y, Jin Q. "VFDB: a reference database for bacterial virulence factors." Nucleic Acids Research (2005) [https://doi.org/10.1093/nar/gki008](https://doi.org/10.1093/nar/gki008)
* [Defensefinder](https://defensefinder.mdmlab.fr) - Tesson F, Planel R,  Egorov A, Georjon H, Vaysset H, Brancotte B, Néron B, Mordret E, Bernheim A,  Atkinson G,  Cury J. "A Comprehensive Resource for Exploring Antiphage Defense: DefenseFinder Webservice, Wiki and Databases." Peer Community Journal (2024) [https://doi.org/10.24072/pcjournal.470](https://doi.org/10.24072/pcjournal.470)
* [acrDB](https://bcb.unl.edu/AcrDB/) - please cite the original acrDB database paper Huang L, Yang B, Yi H, Asif A, Wang J, Lithgow T, Zhang H, Minhas FuAA, Yin Y. "AcrDB: a database of anti-CRISPR operons in prokaryotes and viruses." Nucleic Acids Research (2021) [https://doi.org/10.1093/nar/gkaa857](https://doi.org/10.1093/nar/gkaa857) AND the paper that generated the structures for these protein used by `foldfirst` Sahakyan H, Makarova KS,  Koonin EV. "Search for Origins of Anti-CRISPR Proteins by Structure Comparison." The CRISPR Journal (2023) [https://doi.org/10.1089/crispr.2023.0011](https://doi.org/10.1089/crispr.2023.0011)
* [Netflax](http://netflax.webflags.se) - Ernits K, Saha CK, Brodiazhenko T, Chouhan B, Shenoy A, Buttress JA, Duque-Pedraza JJ, Bojar V, Nakamoto JA, Kurata T, Egorov AA, Shyrokova L, Johansson MJO, Mets T, Rustamova A, Džigurski J, Tenson T, Garcia-Pino A, Strahl H, Elofsson A, Hauryliuk V,  Atkinson GC. "The structural basis of hyperpromiscuity in a core combinatorial network of type II toxin–antitoxin and related phage defense systems." PNAS (2023) [https://doi.org/10.1073/pnas.2305393120](https://doi.org/10.1073/pnas.2305393120) 
* [PDB](https://www.rcsb.org/) - Berman HM, Westbrook J, Feng Z, Gilliland G, Bhat TN, Weissig H, Shindyalov IN, Bourne PE. "The Protein Data Bank" Nucleic Acids Research (2000) [https://doi.org/10.1093/nar/28.1.235](https://doi.org/10.1093/nar/28.1.235)
* [AlphaFold database](https://alphafold.com/) - Fleming J, Magana P, Nair S, Tsenkov M, Bertoni D, Pidruchna I, Afonso MQL, Midlik A, Paramval U, Žídek A, Laydon A, Kovalevskiy O, Pan J, Cheng J, Avsec Ž, Bycroft C, Wong LH, Last M, Mirdita M, Steinegger M, Kohli P, Váradi M, Velankar S. "AlphaFold Protein Structure Database and 3D-Beacons: New Data and Capabilities" Journal of Molecular Biology (2025) [https://doi.org/10.1016/j.jmb.2025.168967](https://doi.org/10.1016/j.jmb.2025.168967)
