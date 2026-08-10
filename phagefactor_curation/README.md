# phageFACTor curation layer for FoldFirstAskLater

Turns FFAL's structural hits into curated, named annotations: one reconciled
product name per CDS, an evidence category, and a short list of genes flagged for
manual review.

Origin and exact version: [`VERSION`](VERSION). Upstream:
<https://github.com/TMs-code/phagefactor>.

## Run it

```bash
bash phagefactor_curation/run_curation.sh <ffal_output_dir>
```

`<ffal_output_dir>` is the directory holding one subdirectory per phage, as
produced by FFAL. Each is expected to contain:

| file | used for |
|---|---|
| `<prefix>_phold_per_cds_predictions.tsv` | the per-CDS calls to reconcile against |
| `*_database_hits.tsv` (one per database) | the structural hits to score and rank |
| `<prefix>_phold.gbk` | gene order and strand, for the optional synteny hints |

Results are written back into each phage's own folder, as
`<phage>/phagefactor_curation/`:

| file | contents |
|---|---|
| `final_annotations_table.csv` / `.xlsx` | one row per CDS: `short_name`, `final_product`, `final_function`, evidence category, and the hit it came from |
| `curated_annotations.csv` | the curation decision per gene, with the rule that fired |
| `review_suggested.csv` | the genes the pipeline declined to call — read this one |
| `comparison_per_gene.csv` | PHold vs FoldSeek side by side, with the agreement label |
| `final_annotations_with_synteny.csv` | the above plus positional hints (C1/Cro, integration, lysis) |
| `*.log` | one log per step |

Useful options:

```bash
HOST_GENUS=Pseudomonas bash phagefactor_curation/run_curation.sh out/    # host boost
ONLY="PEV2 phiKZ"      bash phagefactor_curation/run_curation.sh out/    # subset
VERBOSE=1              bash phagefactor_curation/run_curation.sh out/
```

`HOST_MAP=hosts.tsv` gives a per-phage host genus (two columns: phage, genus) when
one dataset mixes hosts.

## Requirements

Python ≥ 3.9 with `pandas`, `biopython`, `pyyaml`, `openpyxl`. No FoldSeek, no
Pharokka, no GPU, no databases — the search already happened in FFAL. Runs in
seconds per phage on a laptop.

## What it does, in order

1. **Bridge** (`adapters/ffal_to_phagefactor.py`) — merges every
   `*_database_hits.tsv` for one phage into a single scored hit set and picks the
   best hit plus the top 3 per CDS.
2. **Compare** (`scripts/03`) — PHold call vs FoldSeek call per gene, producing an
   agreement label: `same_name`, `strong`, `complementary` (same biology,
   different vocabulary), `partial`, `different`, or one-sided.
3. **Curate** (`scripts/04`) — decides the final name through ordered gates, and
   flags a gene for review instead of guessing when the evidence conflicts.
4. **Build output** (`scripts/05`) — the tables above, plus `short_name`.
5. **Synteny** (`scripts/07`, optional) — positional hints from gene order and
   strand.

Two design points worth knowing:

- **Trusted calls are never re-annotated.** Only genes that came out of gene
  calling as hypothetical enter curation at all.
- **Ranking is not raw bitscore.** A hit's score is
  `bitscore × phage_boost × host_boost`, so a phage-specific name outranks a
  higher-scoring generic one. Uninformative descriptions ("hypothetical protein",
  "DUF1234 domain-containing protein", bare fold names) are dropped before
  ranking — that gate removes most hits.

Taxonomy in the hit table (`taxname`) is optional but enables two filters: the
eukaryote demote and the host boost. Without it both are silently inert, and
`run_curation.sh` says so.

## Where the decisions live

| | |
|---|---|
| `scripts/lib/lexicon.py` | every word-list and pattern — the single definition site |
| `scripts/lib/rules.py` | the ordered rules that consume it: name upgrades, category assignment, `short_name` |
| `config/config.yaml` | thresholds (e-value cut-offs, confidence tiers, fuzzy-match thresholds, the host boost factor) |

Adding a synonym or a new uninformative string is a one-line edit in
`lexicon.py`. Everything ships path-free: the database fields in `config.yaml` are
blank on purpose and are not needed here.
