#!/usr/bin/env python3
"""
lexicon.py -- SINGLE SOURCE OF TRUTH for every description word-list / pattern.
================================================================================
This file is the DEFINITION SITE. foldseek_scoring.py, config.py,
03_compare_annotations.py, 04_curate_annotations.py and utils.py import from
here; none of them define vocabulary any more.

    classify(desc) -> (tier, tags)
        tier : 0 uninformative | 1 low information | 2 proper
        tags : subset of TAGS, describing WHY

WHERE TIER IS USED
--------------------------------------------------------------------------------
Two places read `tier`: step 04 Case 4 (FoldSeek-only call) does not use a
tier-1 name with no inferable function as the final annotation (the hit stays
in the foldseek_* columns and curation_suggestion), and the top-3 majority
vote never lets a tier-1 name replace a named best hit. Every other predicate
reads TAGS, because tags map 1:1 onto the original functions while the tier
collapses distinctions the pipeline treats differently (a tier-1 description
is a ranking malus on one path and ignored on another). The 0/1/2 scheme exists so the
rule-based classifier is directly comparable with inPhormer
(github.com/Mila-MP/inPhormer), which uses the same three tiers.

WHY THE "GENERIC" LISTS ARE NOT COLLAPSED INTO ONE
--------------------------------------------------------------------------------
Merging every generic-sounding pattern into a single `is_generic()` is tempting
and wrong: the callers need DIFFERENT WIDTHS, and each width drives a different
consequence.

  * DROP     (uninformative / uncharacterized) - the hit is discarded.
  * MALUS    (domain/family suffix, gene labels) - the hit is KEPT, its ranking
             score multiplied by foldseek.low_info_malus (0.60). A dropped hit
             and a demoted hit are not the same.
  * UPGRADE  (gene-label / ORF-style only) - swaps in a better top-3 entry.
  * GATE     (curation, step 04) - some gates are intentionally NARROW (only a
             bare structural term counts), others intentionally BROAD.

Flattening them would fire narrow gates on broad terms, tripping curation rules
in exactly the cases they were written to avoid, and turn score maluses into
outright drops. The TAGS give one shared vocabulary while letting each caller
pick its own width. Nothing widens by accident.

PER-TAG PREPROCESSING
--------------------------------------------------------------------------------
Each tag is computed with the SAME normalisation as the predicate that consumes
it. This matters: _is_informative_fs unwraps a PDB title and re-tests the
EXTRACTED text, so running UNCHAR_PATTERN on the raw string gives a different
answer for e.g. 'Crystal structure of ORF041 from Bacteriophage 37'.
See classify() for the exact order.
"""
import re
from functools import lru_cache

# =============================================================================
# TAGS
# =============================================================================
TAG_UNCHARACTERIZED = "uncharacterized"   # hypothetical / uncharacterized / unknown
TAG_DUF             = "duf"               # DUF#### / domain of unknown function
TAG_DOMAIN_SUFFIX   = "domain_suffix"     # "X family protein", "domain-containing"
TAG_GENE_LABEL      = "gene_label"        # UPF1234 protein, YabC protein
TAG_ORF_STYLE       = "orf_style"         # orf42, "Prophage ..., Orf12"
TAG_STRUCTURAL_TERM = "structural_term"   # fold, not function: "four helix bundle"
TAG_GENERIC_STRUCTURAL = "generic_structural"  # 'endolysin' - REAL but unspecific
TAG_PDB_TITLE       = "pdb_title"         # "Crystal structure of ..."
TAG_PHAGE_SPECIFIC  = "phage_specific"    # named phage protein
TAG_PHAGE_CONTEXT   = "phage_context"     # merely viral/phage context
TAG_WRAPPER         = "wrapper"           # bare gpNN / "phage protein"
TAG_PROMISCUOUS     = "promiscuous"       # fold, not function
TAG_EUKARYOTIC      = "eukaryotic"        # eukaryote-specific by description
TAG_DEFENSE         = "defense"           # defense / toxin-antitoxin

TIER_UNINFORMATIVE, TIER_LOW, TIER_PROPER = 0, 1, 2

# =============================================================================
# A. INFORMATIVENESS
# =============================================================================
# A1 exact-match blocklist.
# Also consumed by utils.is_informative/clean_str on PHOLD + pharokka products,
# which is why PHold-side sentinels ('none', 'no phold match') live here even
# though a FoldSeek description will never contain them.
UNINFORMATIVE_STRINGS = frozenset({
    '', 'defensefinder protein', 'hypothetical protein',
    'n/a', 'na', 'nan',
    'no phold match', 'none', 'phage protein',
    'predicted protein', 'putative uncharacterized protein', 'uncharacterized protein',
    'uncharacterized protein (fragment)', 'unknown function',
})

# A2 uninformative-description test. ONE pattern on purpose: two overlapping
# informativeness patterns cannot be kept in step by hand.
UNCHAR_PATTERN = re.compile(
    r"""
      uncharacteri[sz]ed                       # both spellings
    | hypothetical
    | predicted\ protein
    | unknown\ function
    | \bDUF\d+\b
    | domain\ of\ unknown\ function
    | no\ annotation
    | putative\ uncharacterized
    | protein\ of\ unknown
    | ^orf\d+\s*(from\b.*)?$

    # Bare fold names: a shape, not a function.
    | ^(?:(?:two|three|four|five|six)[\s-]?)?helix[\s-]?bundle(?:\s+protein)?$
    | ^beta[\s-]?barrel(?:\s+protein)?$
    # 'phage protein' and its Bacteriophage-/Prophage- variants name nothing.
    | ^(?:putative\ |probable\ |conserved\ |predicted\ |hypothetical\ )*(?:bacterio)?(?:pro)?phage[\s-]?(?:associated|related|derived|encoded|specific)?[\s-]?protein(?:\s*\((?:fragment|unknown\ function)\))?$
    | ^Prophage\ \S+\ protein\ \d+$          # 'Prophage pi3 protein 52'
    | ^[A-Za-z]{2,4}\d{3,5}\ protein$        # locus tag + protein: 'Lin2420 protein'
    # Accession-shaped names. NARROW: 'SNF2-related', 'VgrG-related' and
    # 'tape measure-related' must survive; only a tag-shaped stem is rejected.
    | ^(?:[A-Za-z]{2,6}\d{2,5}|\d+)-related\s+protein$
    | ^[A-Za-z]{3,}[-_]?\d{3,5}$   # >=3 digits: 'Rad50'/'Mre11' are real genes
    # 'UPF0200 protein MM_1313' is a locus tag; 'UPF0102 protein YraN' is a gene
    # symbol. A DIGIT in the trailing token separates them.
    | ^UPF\d+\s+protein\s+\S*\d
    # A Pfam ACCESSION used as a name ('PF10979 family protein') is a DUF by
    # another spelling; 'Group-specific protein' is a placeholder that names
    # no function.
    | ^PF\d{5}\b(?:\s+family)?(?:\s+protein)?$
    | ^group[\s-]specific\s+protein$
    """,
    re.IGNORECASE | re.VERBOSE,
)

# Shared fragments for the wrapper / gene-label patterns below, so the patterns
# stay nested by construction (PHAGE_WRAPPER_ONLY is a subset of
# FS_GENERIC_BROAD; GENE_LABEL is a subset of GENERIC_NAMES).
_WRAP_PREFIX   = r"(?:putative\s+|conserved\s+)?"
_PHAGE_PROTEIN = r"(?:bacterio)?(?:pro)?phage[\s\-]*(?:associated|related|derived)?\s*protein\b"
_GP_NAME       = r"gp\d+\w*"
_GP_HOMOLOG    = r"homolog\s+to\s+.*\bgp\d+.*"
_Y_GENE        = r"^Y[a-z]{2}[A-Z]\s+protein\s*$|^protein\s+Y[a-z]{2}[A-Z]\s*$"   # YdaW protein / Protein YdaW

# A3 domain/family SUFFIX -> low-information malus (config foldseek.low_info_malus, 0.60).
GENERIC_DOMAIN = re.compile(
    r'''family protein$|domain.containing protein$|domain.containing$|homolog$|superfamily.*protein$|related protein$|like protein$'''
    # '-like' only as a bare one-word name ('YodL-like'): 'anti-termination
    # protein Q-like', 'Arc-like repressor' name a function.
    r'''|\brepeat(?:[\s-]containing)?(?:\s+family)?\s+protein$|\bdomain\s+protein$|^\S+-like$|\bmotif$''',
    re.IGNORECASE,
)

# A4 gene-label / ORF-style -> low-information malus (same).
GENERIC_NAMES = re.compile(
    r'''^UPF\d+\s+protein\b|^Prophage [^,]+,\s*Orf\d+\s*$|^orf\d+\s*$|^protein\d+\s*$'''
    + "|" + _Y_GENE +
    # Gene symbols of families with no assigned function outside the y-gene
    # convention. Explicit names only: 'symbol + protein' would catch RecA/DnaB.
    r'''|^TnpV\s+protein\s*$''',
    re.IGNORECASE,
)

# A5 GENERIC PHAGE STRUCTURAL PRODUCT NAMES.
# NOTE THE MEANING: these are real, informative annotations ('endolysin',
# 'major capsid protein') that a NARROW curation gate treats as "generic" only
# when deciding whether a MORE SPECIFIC call should win. They are NOT
# uninformative. Fold-not-function terms are handled in UNCHAR_PATTERN.
GENERIC_STRUCTURAL_PRODUCTS = re.compile(
    r'''^(tail protein|tail fiber protein|baseplate (wedge|spike|hub)?\s*(subunit\s*)?protein|endolysin|holin|spanin|portal protein|major capsid protein|minor capsid protein|head protein|tape measure protein|terminase(\s+(large|small))?\s*subunit|connector protein|neck protein)$''',
    re.IGNORECASE,
)

# A6 tokens removed before the Jaccard in utils.tokenize(). NOT an
# informativeness list: removing a token RAISES similarity, never drops a hit.
GENERIC_WORDS = frozenset({
    'from', 'with', 'uncharacterized',
    'a', 'an', 'and',
    'associated', 'binding', 'cds',
    'class', 'component', 'conserved',
    'containing', 'domain', 'factor',
    'family', 'function', 'gene',
    'homolog', 'hypothetical', 'in',
    'is', 'like', 'na',
    'of', 'or', 'phage',
    'phrog', 'phrogs', 'predicted',
    'probable', 'protein', 'putative',
    'related', 'subunit', 'superfamily',
    'the', 'to', 'type',
    'unknown',
})

# A9 EXACT-membership generic descriptors.
# Used by the generic-vs-specific gate. Membership is EXACT (never a substring),
# so a combined 'X / generic-Y' string cannot match and this cannot double-fire
# with _UPGRADE_RULES.
GENERIC_DESCRIPTORS = frozenset({
    "dna-binding protein", "dna binding protein",
    "membrane protein", "transmembrane protein", "integral membrane protein",
    "metal-binding protein", "nucleotide-binding protein",
    "atp-binding protein", "atpase", "binding protein",
    "hydrolase", "transferase", "lipoprotein",
    # Phold terms that are too generic to keep when FS names a specific
    # protein. The winner-must-be-specific guard in _generic_vs_specific stops
    # a generic FS wrapper from ever beating a specific phold call.
    "virion structural protein", "structural protein", "minor structural protein",
    "phage protein", "prophage protein", "putative phage protein",
    "phage-related protein", "phage derived protein", "conserved protein",
    "membrane-flanked domain", "transcriptional regulator",
    # bare "toxin"/"antitoxin": category-level placeholders. generic-vs-specific
    # yields to the other side ONLY when it is genuinely specific (its _specific()
    # guard), so a named FS TA family (HicA/PemK/RelE/...) wins while a bare
    # "toxin" is still kept when nothing more specific is offered.
    "toxin", "antitoxin", "toxin protein", "antitoxin protein",
    # Cellular LOCATION is not a function.
    "secreted protein", "exported protein", "cytoplasmic protein",
    "periplasmic protein", "extracellular protein", "cell surface protein",
    "membrane-associated protein",
})

# =============================================================================
# B. TAXONOMY
# =============================================================================
# B description-level eukaryote signal (no taxonomy needed).
EUKARYOTIC_DESC = re.compile(
    r'''\bHomo\s+sapiens\b|\bhuman\b.*\bprotein\b|\bMus\s+musculus\b|\bSaccharomyces\s+cerevisiae\b|\bCaenorhabditis\b|\bDrosophila\b|\bkinesin\b|\bmyosin\b|\bdynein\b|\bnephrocystin\b|\bBCL.6\b|\bcorepressor\b|\bubiquitin.protein\s+ligase\b|\bERAD.associated\b|\bSEL1.*UBX\b|\bretinoblastoma\b|\bubiquitin\s+carboxyl.terminal\s+hydrolase\b|\bactin\s+filament\b|\bkelch\b|\bisoform\s+X\d+\b''',
    re.IGNORECASE,
)

VIRUS_KEYWORDS = frozenset({
    'bacteriophage', 'phage', 'prophage',
    'virales', 'viridae', 'virinae',
    'virus',
})

ARCHAEA_KEYWORDS = frozenset({
    'acidianus', 'archaea', 'archaeon',
    'ferroplasma', 'haloarcula', 'halobacter',
    'halobacterium', 'haloferax', 'halorubrum',
    'ignicoccus', 'metallosphaera', 'methanobacterium',
    'methanocaldococcus', 'methanococcus', 'methanosarcina',
    'natronomonas', 'nitrososphaera', 'pyrobaculum',
    'pyrococcus', 'sulfolobus', 'thermococcus',
    'thermofilum', 'thermoplasma',
})

# B keyword-based kingdom assignment (no NCBI
# taxonomy dependency): inclusive but not exhaustive, and the lookup returns
# 'Unknown' rather than guessing. Extend when a common genus is being missed.
BACTERIA_KEYWORDS = frozenset({
    # Common bacterial genera (phage hosts and well-represented in AFDB)
    "campylobacter", "helicobacter", "escherichia", "pseudomonas", "salmonella",
    "staphylococcus", "streptococcus", "lactobacillus", "bacillus", "clostridium",
    "vibrio", "yersinia", "neisseria", "mycobacterium", "listeria", "borrelia",
    "treponema", "chlamydia", "rickettsia", "acinetobacter", "klebsiella",
    "enterobacter", "serratia", "citrobacter", "shigella", "haemophilus",
    "legionella", "francisella", "burkholderia", "bordetella", "brucella",
    "campylobacterales", "enterococcus", "bacteroides", "prevotella",
    "fusobacterium", "porphyromonas", "ruminococcus", "eubacterium", "blautia",
    "faecalibacterium", "akkermansia", "bifidobacterium", "propionibacterium",
    "cutibacterium", "corynebacterium", "actinomyces", "nocardia", "rhodococcus",
    "streptomyces", "mycoplasma", "ureaplasma", "spirochaeta", "leptospira",
    "deinococcus", "thermus", "synechocystis", "synechococcus", "nostoc",
    "anabaena", "cyanobacter", "geobacter", "shewanella", "moraxella",
    "stenotrophomonas", "ralstonia", "xanthomonas", "azotobacter", "rhizobium",
    "agrobacterium", "sinorhizobium", "mesorhizobium", "bradyrhizobium",
    "caulobacter", "rhodobacter", "paracoccus", "sphingomonas", "novosphingobium",
    "thiobacillus", "nitrosomonas", "nitrobacter", "desulfovibrio", "thermotoga",
    "aquifex", "deinococcales", "alistipes", "parabacteroides", "morganella",
    "proteus", "providencia", "edwardsiella", "pectobacterium", "erwinia",
    "pantoea", "cronobacter", "rahnella", "hafnia", "obesumbacterium",
    # Generic words
    "bacterium", "bacteria",
})

# B ONE eukaryote list, used by BOTH call sites (03's kingdom assignment and
# 02d's parse-time taxon filter). AFDB is dominated by human/mouse/yeast
# structures, so any helix bundle finds some eukaryotic match.
#
# MATCHED ON WORD BOUNDARIES, via is_eukaryotic_taxname() -- not as substrings.
# This is what makes one list safe for both callers: substring matching deleted
# 526 BACTERIAL taxnames outright ('Campylobacter conciSUS', 'AnoxybacillUS
# flavitherMUS', 'AchromobacterS PANius', 'Algoriphagus machiPONGOnensis').
# 03 never noticed because it tests Bacteria first; 02d filters at parse time,
# where there is no such rescue and the hit is simply gone.
#
# Ambiguous single-word epithets are therefore listed as BINOMIALS only:
# canis / felis / ovis / sus / mus / bos / pan / pongo / equus are all valid
# bacterial species epithets (Helicobacter canis, Moraxella ovis, Brucella ovis).
EUKARYOTE_KEYWORDS = frozenset({
    # plants, fungi, protists, animals seen among FoldSeek hits
    "gossypium", "medicago", "salix", "crocus", "camptotheca", "micractinium",
    "symbiodinium", "effrenium", "kipferlia", "neospora", "aphanomyces",
    "parastagonospora", "trichophyton", "monilinia", "mycosarcoma", "ogataea",
    "lachancea", "hanseniaspora", "mytilus", "strongylocentrotus", "ornithodoros",
    "nothobranchius", "branchiostoma", "brugia", "cylicostephanus", "stylophora",
    "hydra", "bos",
    # Mammals and other animals. Genus alone where unambiguous; binomial where
    # the genus name is also a bacterial species epithet.
    "homo", "homo sapiens", "mus musculus", "rattus", "bos taurus",
    "sus scrofa", "canis lupus", "canis familiaris", "felis catus",
    "ovis aries", "equus caballus", "macaca", "pan troglodytes",
    "pan paniscus", "pongo abelii", "pongo pygmaeus",
    "monodelphis", "ornithorhynchus", "phascolarctos",
    # Common eukaryotic model organisms
    "saccharomyces", "schizosaccharomyces", "candida", "aspergillus", "neurospora",
    "drosophila", "caenorhabditis", "anopheles", "apis mellifera", "bombyx",
    "tribolium",
    "arabidopsis", "oryza", "zea", "glycine max", "solanum", "nicotiana", "populus",
    "physcomitrella", "chlamydomonas", "selaginella",
    "danio", "xenopus", "gallus", "anolis", "alligator", "takifugu",
    # Protists / parasites
    "plasmodium", "trypanosoma", "leishmania", "giardia", "entamoeba",
    "trichomonas", "tetrahymena", "paramecium", "dictyostelium",
    "phytophthora", "pythium", "thalassiosira", "phaeodactylum",
    # Fungi (other than yeasts above)
    "ustilago", "magnaporthe", "fusarium", "trichoderma", "cryptococcus",
    "puccinia", "coprinopsis", "agaricus",
    # Taxonomic ranks / generic terms
    "metazoa", "viridiplantae", "fungi", "eukaryota", "eukaryote", "eukaryotic",
    "mammalia", "chordata",
    # Not a eukaryote, but the same decision: a designed sequence is not a
    # biological homolog. Kept here because 02d's filter is the only consumer
    # and a second list for one entry is not worth keeping in step.
    "synthetic construct",
})



_EUKA_SINGLE = None
_EUKA_MULTI = None


def is_eukaryotic_taxname(taxname):
    """True if a FoldSeek taxname is eukaryotic (or a synthetic construct).

    Word-boundary matching, NOT substring: see the note on EUKARYOTE_KEYWORDS.
    Single-word keys must match a whole token; multi-word keys must appear as a
    phrase. Conservative by design -- when in doubt it returns False, because
    02d uses this to DELETE hits before scoring.
    """
    global _EUKA_SINGLE, _EUKA_MULTI
    if _EUKA_SINGLE is None:
        _EUKA_SINGLE = frozenset(k for k in EUKARYOTE_KEYWORDS if " " not in k)
        _EUKA_MULTI = tuple(k for k in EUKARYOTE_KEYWORDS if " " in k)
    if not taxname or not isinstance(taxname, str):
        return False
    t = taxname.lower()
    if any(k in t for k in _EUKA_MULTI):
        return True
    return bool(_EUKA_SINGLE & set(re.findall(r"[a-z]+", t)))

# =============================================================================
# C. PHAGE TERMS
# =============================================================================
# C x2.00 boost: a NAMED phage protein. gpNN is phage gene nomenclature; orfNN is
# not (any genome numbers its ORFs: 'IS66 family element, orf1') and is handled as a
# gene label (ORF_STYLE / GENERIC_NAMES), never boosted.
# Toxin-antitoxin families: a top-3 hit naming one is preferred when phold gives
# only the TA role ([merge: ta-functionalize]; 'YgiT family' over 'Cro/CI').
TA_FAMILY = re.compile(
    r'''\b(?:anti)?toxin\b|\b(?:ygiT|mqsA|mqsR|hicA|hicB|relB|relE|parD|parE|higA|higB|mazE|mazF|vapB|vapC|yefM|yoeB|ccdA|ccdB|hipA|hipB)\b|death.on.curing|addiction.module''',
    re.I)

# Domains that are part of the protein phold names, not another function:
# (phold regex, FoldSeek regex, why). Keep phold, no review ([keep-phold: domain-partner]).
DOMAIN_PARTNERS = [
    (re.compile(r'\bHNH\b|homing.endonuclease', re.I), re.compile(r'\bAP2\b', re.I),
     "AP2 is the DNA-binding domain of AP2-HNH homing endonucleases"),
]

PHAGE_SPECIFIC = re.compile(
    r'''\bgp\d+\b|terminase|capsid|portal|baseplate|tail.fiber|\b(?:anti)?holin|endolysin|spanin|excisionase|integrase|recombinase|resolvase|invertase|\bbet\b|\bexo\b|anti.?repressor|(?:\bphage|\bprophage|\blambda|\b434|\bp22|\bc[i12]\b|\bcro\b)[^/,;]{0,40}?\brepressor|\bcro\b|\bdica\b|n-protein|o-protein|head.*protein|structural.*protein.*phage|major capsid|minor capsid|tape.measure|tail.(?:sheath|tube|terminator)|late.control|anti.crispr|abortive''',
    re.IGNORECASE,
)

# C x1.50 boost: viral/phage context only.
PHAGE_CONTEXT = re.compile(
    r'''\bphage\b|\bprophage\b|\bbacteriophage\b|\bviral\b''',
    re.IGNORECASE,
)

# C x0.40 malus: a wrapper or placeholder at the start of the name.
GP_WRAPPER = re.compile(
    r"^" + _WRAP_PREFIX + r"(?:" + _PHAGE_PROTEIN + r"|" + _GP_NAME
    + r"|\S*\s*gp\d+\b|mu-?like\s+prophage|hypothetical\s+protein|uncharacteri[sz]ed)",
    re.IGNORECASE,
)

# =============================================================================
# D. DEFENSE
# =============================================================================
# One vocabulary, two groups:
#
#   DEFENSE_SYSTEM_TERMS  defense in the BROAD sense: anti-phage / immune
#                         systems AND the arms race around them (restriction
#                         alleviation Lar, anti-restriction ArdC, anti-CRISPR,
#                         immunity proteins) -> category "defense" (rules.py)
#   DEFENSE_TA_TERMS      toxin-antitoxin families
#                         -> category rules.TA_CATEGORY (PHROG convention: moron)
#   DEFENSE_ANY = both    -> the FoldSeek defense FLAG (step 02,
#                            foldseek_scoring / 02d) and the curation-side "is
#                            this a defense/TA call" check (step 04)
# Add a term ONCE, here; every consumer picks it up.
# 'toxin' followed by secretion/translocation/transport/export is a secretion
# system or holin ('Toxin secretion/phage lysis holin'), not a TA toxin.
DEFENSE_SYSTEM_TERMS = (
    r"defen[cs]e|anti.?phage|anti.?viral|abortive.infection|\babi[A-Z]\b"
    r"|restriction|\bRM\b\s*system|\bimmunity\b"
    r"|\bCBASS\b|\bSMODS\b|\bSLATT\b|\bPARIS\b|\brlo[GCH]\b|\bsir\s*2\b|\bsirtuin\b"
    r"|\bCami1\b|\bCARF\b|type.III.CRISPR|anti.?crispr"
)
DEFENSE_TA_TERMS = (
    r"toxin.antitoxin|\bantitoxin\b|\btoxin\b(?!\s+(?:\S+\s+)?(?:secretion|translocation|transport|export))|\bmaz[EF]{1,2}\b|\bpem[IK]\b|\brel[BE]\b|\bhic[AB]\b"
    r"|\bvap[BC]\b|\bhig[AB]\b|\byoeB\b|\bpar[DE]\b|\bphd\b|death.on.curing|\bdoc\b"
    r"|\bsym[ER]\b|netflax"
)
DEFENSE_SYSTEM = re.compile(DEFENSE_SYSTEM_TERMS, re.IGNORECASE)
DEFENSE_TA     = re.compile(DEFENSE_TA_TERMS, re.IGNORECASE)
DEFENSE_ANY    = re.compile(DEFENSE_SYSTEM_TERMS + "|" + DEFENSE_TA_TERMS, re.IGNORECASE)


# =============================================================================
# E. PROMISCUOUS FOLDS (fold != function)
# =============================================================================
PROMISCUOUS_FOLDS = re.compile(
    r'''\bbeta.lactamase\b|\bmetallo.beta.lactamase\b|\bglyoxalase\s+II\b|\btRNase\s*Z\b|\bCPSF.7[03]\b|\barc\s+family\b|\bribbon[\s-]?helix[\s-]?helix\b|\bRHH\b|\bhicB\b\s+(family\s+)?antitoxin|\bkinesin\b|\bmyosin\b|\bdynein\b|\bubiquitin.protein\s+ligase\b|\bSEL1.*UBX\b|\bERAD.associated\b|\bnephrocystin\b''',
    re.IGNORECASE,
)

# =============================================================================
# F. PDB TITLE CLEANUP
# =============================================================================
PDB_CRYSTAL_PREFIX = re.compile(
    r'''^(?:crystal\s+structure\s+of\s+(?:a\s+|an\s+|the\s+)?|structure\s+of\s+(?:a\s+|an\s+|the\s+)?)''',
    re.IGNORECASE,
)

PDB_TRAILING_QUAL = re.compile(
    r'''\s+(?:from|in\s+complex\s+with|in\s+the\s+|bound\s+to|at\s+\d|\bwith\b|\busing\b|\bby\b).*$''',
    re.IGNORECASE,
)

PDB_UNINFORMATIVE = re.compile(
    r'''^orf\d+\s*$|^orf\d+\s+from\b|^engineered protein|^designed protein|^northeast structural genomics|^semet\s+apo\s+|\bdarpin\b|^four.helix bundle|^three.helix bundle|^helix bundle''',
    re.IGNORECASE,
)

PDB_TITLE = re.compile(
    r'''\b(structure|characteri[sz]ation|biophysical|cryo-?em|in complex with|bound to|implications for|crystal structure of)\b''',
    re.IGNORECASE,
)

# =============================================================================
# G. CHARACTERIZED DOMAINS (positive counterpart to E)
# =============================================================================
RELEVANT_DOMAINS = [
    ('SPOR', 'SPOR',
     'peptidoglycan-binding protein',
     'Yahashiri 2017 PMID 28396350'),
    ('PIN', 'PIN',
     'ribonuclease',
     'Matelska 2017 PMID 28575517'),
    ('LysM', 'LysM',
     'peptidoglycan-binding protein',
     'Buist 2008 Mol Microbiol PMID 18430080'),
    ('HNH', 'HNH',
     'endonuclease',
     'Mehta 2004 PMID 14691243'),
    ('GIY[- ]?YIG', 'GIY-YIG',
     'endonuclease',
     'Dunin-Horkawicz 2006 PMID PMC1564403'),
    ('HEPN', 'HEPN',
     'ribonuclease',
     'Anantharaman 2013 Biol Direct PMID 23768067'),
    ('CBS', 'CBS',
     'ligand-binding regulatory protein (adenosine/energy sensor)',
     'Ereno-Orbea 2013 PNAS PMID 24344311'),
]


# =============================================================================
# H. CURATION-GATE VOCABULARY (step 04)
# =============================================================================
# A7 NARROW generic-FoldSeek gate.
FS_GENERIC = re.compile(
    r'''\b(putative|hypothetical|uncharacteri[sz]ed|unknown)\b.*\bprotein\b|exported protein|lipoprotein|^duf\d+|^upf\d+|^(putative |conserved )?(outer |inner )?membrane protein$''',
    re.IGNORECASE,
)

# A8 BROAD generic-FoldSeek gate (superset of PHAGE_WRAPPER_ONLY).
# Deliberately WIDER than FS_GENERIC: see the module docstring on widths.
FS_GENERIC_BROAD = re.compile(
    r"^" + _WRAP_PREFIX + r"(?:" + _PHAGE_PROTEIN + r".*|" + _GP_NAME + r"|" + _GP_HOMOLOG
    + r'''|\S*\s*gp\d+\b.*|mu-?like\s+prophage.*|.*\bhk97\b.*|phage\s+(nucleotide-?binding|minor\s+structural|structural|virion\s+morphogenesis|derived|associated)\s+protein.*|atp-?binding protein|atpase|dna-?damage-?inducible protein.*|(?:pro)?phage\s+\w+\s+protein\s+\d+|.*\b(ig-?like|asch|atp-grasp|duf\d+|zinc[\s\-]?(ribbon|finger))\s+domain.?containing\s+protein.*|pentapeptide\s+repeat.*|tigr\d+\s+family\s+protein.*|chp\d+.*|.*\b[nc]-?terminal\s+domain.?containing\s+protein|.*\b(macro|jmjc|toprim)\s+domain.?containing\s+protein|(uncharacteri[sz]ed|hypothetical)\s+protein.*)$''',
    re.IGNORECASE,
)

# C ANCHORED wrapper: the whole name is a wrapper, so 'baseplate phage
# protein' is NOT one (unlike GP_WRAPPER, which only reads the start).
PHAGE_WRAPPER_ONLY = re.compile(
    r"^" + _WRAP_PREFIX + r"(?:" + _PHAGE_PROTEIN + r".*|" + _GP_NAME
    + r"|phage\s+protein\s+(?:gp)?\d+.*|" + _GP_HOMOLOG + r")$",
    re.IGNORECASE,
)

# B eukaryote-homolog RESCUE list. These are
# eukaryotic-looking names that ARE meaningful in phage defence.
EUKA_RESCUE = re.compile(
    r'''\bsir\s*2\b|\bsirtuin\b|\bTIR\b|\bSTING\b|\bcGAS\b|\bcd-?ntase\b|\bviperin\b|\bargonaute\b|\bp?Ago\b|\bgasdermin\b|\bSamHD1\b''',
    re.IGNORECASE,
)

# A4b single Y-gene label (subset of GENERIC_NAMES): drives an UPGRADE, not a malus.
GENE_LABEL = re.compile(_Y_GENE, re.IGNORECASE)

# =============================================================================
# I. FUNCTIONAL EQUIVALENCE GROUPS
# =============================================================================
# Consumed two ways. (1) 03_compare's is_complementary() keys on the PHold
# *category* (left) and looks for any keyword (right) in the FoldSeek
# description. (2) 04_curate's _shares_function treats each value-list as a
# SYNONYM GROUP (symmetric, category-agnostic): two descriptions that each
# contain a keyword from the SAME list are "related" (no review flag).
# Extend known biological equivalences here rather than adding a parallel list.
COMPLEMENTARY_CATEGORY_MAP = {
    # cell-wall hydrolases (lysis): muramidase/amidase/glycosidase are all peptidoglycan
    # hydrolases -> phosphodiester glycosidase ~ amidase. A spanin IS a lysis protein,
    # so "spanin"/"lysis" belong in one group rather than reported as a divergence.
    "lysis":                   ["lysin", "endolysin", "holin", "spanin", "amidase",
                                "muramidase", "lysozyme", "glycosidase", "glucosaminidase",
                                "phosphodiester", "lysis"],
    "tail":                    ["tail", "baseplate", "fiber", "spike", "needle", "tape measure"],
    # prohead/maturation proteases are head&packaging; the protease/peptidase terms
    # make metallo-protease ~ metallopeptidase pairs related for _shares_function.
    "head and packaging":      ["portal", "capsid", "terminase", "head", "major capsid",
                                "scaffold", "prohead", "morphogenesis",
                                "protease", "peptidase", "metallopeptidase", "metalloprotease"],
    "integration and excision":["integrase", "excisionase", "recombinase", "transposase",
                                "resolvase", "dna repair", "site-specific"],
    # a transcriptional regulator is a DNA-binding/HTH protein. Fur/ferric-uptake and
    # LexA are CAP-like HTH metal/SOS regulators (Holm 1994 PMID 7708205).
    "transcription regulation":["repressor", "activator", "anti-repressor", "cro",
                                "dna binding", "dna-binding", "helix-turn-helix", "hth",
                                "winged helix", "arc", "regulator", "sigma", "abrb", "lexa",
                                "fur", "ferric", "regulation"],
    # replisome~replication, parA/parB/Soj~chromosome partitioning~ribonuclease, RecT/RecA.
    # Soj is a ParA homolog. RusA: "Endodeoxyribonuclease RusA" vs "RusA-like Holliday
    # junction resolvase" is the same enzyme, one names the activity.
    "DNA, RNA and nucleotide metabolism": ["replication", "polymerase", "ligase", "helicase",
                                "primase", "nuclease", "exonuclease", "endonuclease",
                                "recombinase", "recombination", "ribonuclease", "replisome",
                                "resolvase", "holliday", "rusa",
                                "reca", "rect", "sbc", "methyltransferase",
                                "deoxyribosyltransferase", "partition", "chromosome partitioning",
                                "parb", "para", "soj", "korb", "atpase", "aaa", "atp-binding",
                                "primosome", "terminase", "hth", "dna-binding"],
    # Fe-S redox enzymes carried as morons: Fe-S-cluster redox ~ Fe-S-oxidoreductase.
    "moron, auxiliary metabolic gene and host takeover": ["toxin", "antitoxin", "amg",
                                "secreted", "effector", "oxidoreductase", "redox",
                                "fe-s", "fes", "ferredoxin"],
    "connector":               ["connector", "adaptor", "portal", "neck", "head-tail"],
}

# Helper subsets used only by classify(); no caller reads them directly.
DUF_PATTERN = re.compile(r"\bDUF\d+\b|domain of unknown function", re.IGNORECASE)
ORF_STYLE = re.compile(r"^orf\d+\s*$|^Prophage [^,]+,\s*Orf\d+\s*$", re.IGNORECASE)

_RELEVANT_PATTERNS = [
    (re.compile(rf"\b(?:{tok})\b[\s-]*domain", re.IGNORECASE), label, activity)
    for (tok, label, activity, _ref) in RELEVANT_DOMAINS
]


def strip_fragment(desc):
    """Drop a trailing '(Fragment)'. Used before the generic/domain tests, to
    match _phage_boost_factor, which strips it before applying the malus."""
    return re.sub(r"\s*\(fragment\)\s*$", "", str(desc).strip(), flags=re.IGNORECASE)


def extract_pdb_description(desc):
    """Strip 'Crystal structure of ...' and trailing qualifiers, mirroring
    foldseek_scoring._extract_pdb_description. Returns '' when nothing
    informative survives."""
    d = PDB_CRYSTAL_PREFIX.sub("", str(desc).strip())
    d = PDB_TRAILING_QUAL.sub("", d).strip(" .,;")
    if not d or PDB_UNINFORMATIVE.search(d):
        return ""
    return d


def relevant_domain(description):
    """(DOMAIN_label, activity) for a characterized-domain hit, else None.
    Fires only on DOMAIN-LEVEL descriptions ('PIN domain-containing protein'),
    never on a specific protein name that merely contains the token."""
    if not description:
        return None
    for pat, label, activity in _RELEVANT_PATTERNS:
        if pat.search(str(description)):
            return (label, activity)
    return None


def classify(desc):
    """Return (tier, tags). ONE pass, no side effects; tags is a frozenset.

    Order matters and mirrors the original predicates exactly:
      1. exact blocklist on the lowercased, stripped string;
      2. PDB titles are UNWRAPPED and the extracted text is what gets tested
         for 'uncharacterized' -- this is what _is_informative_fs does;
      3. the generic/domain tests run on the (Fragment)-stripped string,
         matching _phage_boost_factor;
      4. phage-specific and phage-context are mutually exclusive (elif), as in
         _phage_boost_factor.
    """
    if not desc or not isinstance(desc, str):
        return TIER_UNINFORMATIVE, frozenset()
    return _classify_str(desc)


# Cached: a FoldSeek hit table repeats each description many times
# (~8 hits per unique description), and every predicate below calls classify().
@lru_cache(maxsize=None)
def _classify_str(desc):
    tags = set()
    d = desc.strip()
    if not d:
        return TIER_UNINFORMATIVE, frozenset({TAG_UNCHARACTERIZED})
    # A blocklisted string is uninformative but must STILL collect its other
    # tags: 'Hypothetical protein' also matches GP_WRAPPER, and
    # _phage_boost_factor applies the x0.40 wrapper malus regardless.
    if d.lower() in UNINFORMATIVE_STRINGS:
        tags.add(TAG_UNCHARACTERIZED)

    # (2) unwrap PDB titles before the uncharacterized test
    probe = d
    if PDB_CRYSTAL_PREFIX.match(d):
        tags.add(TAG_PDB_TITLE)
        probe = extract_pdb_description(d)
        if not probe:
            tags.add(TAG_UNCHARACTERIZED)
    if probe and UNCHAR_PATTERN.search(probe):
        tags.add(TAG_UNCHARACTERIZED)

    if DUF_PATTERN.search(d):
        tags.add(TAG_DUF)
    if GENERIC_STRUCTURAL_PRODUCTS.search(d):
        tags.add(TAG_GENERIC_STRUCTURAL)      # informative, just unspecific

    # (3) generic tests on the fragment-stripped string
    gen = strip_fragment(d)
    if GENERIC_DOMAIN.search(gen):
        tags.add(TAG_DOMAIN_SUFFIX)
    if GENERIC_NAMES.search(gen):
        tags.add(TAG_GENE_LABEL)
    if ORF_STYLE.search(gen):
        tags.add(TAG_ORF_STYLE)

    # (4) mutually exclusive, as in _phage_boost_factor
    if PHAGE_SPECIFIC.search(d):
        tags.add(TAG_PHAGE_SPECIFIC)
    elif PHAGE_CONTEXT.search(d):
        tags.add(TAG_PHAGE_CONTEXT)

    if GP_WRAPPER.search(d):
        tags.add(TAG_WRAPPER)
    if PROMISCUOUS_FOLDS.search(d):
        tags.add(TAG_PROMISCUOUS)
    if EUKARYOTIC_DESC.search(d):
        tags.add(TAG_EUKARYOTIC)
    if DEFENSE_ANY.search(d):
        tags.add(TAG_DEFENSE)

    tags = frozenset(tags)   # cached result: must not be mutable
    if tags & {TAG_UNCHARACTERIZED, TAG_DUF, TAG_STRUCTURAL_TERM}:
        return TIER_UNINFORMATIVE, tags
    if tags & {TAG_DOMAIN_SUFFIX, TAG_GENE_LABEL, TAG_ORF_STYLE, TAG_WRAPPER}:
        return TIER_LOW, tags
    return TIER_PROPER, tags


def is_informative_desc(desc):
    """Tag-derived equivalent of foldseek_scoring._is_informative_fs."""
    return TAG_UNCHARACTERIZED not in classify(desc)[1]


if __name__ == "__main__":   # smoke test: python scripts/lib/lexicon.py
    for d in ["hypothetical protein", "DUF1234 domain-containing protein",
              "tail fiber family protein", "UPF0210 protein", "orf42",
              "Phage portal protein", "Crystal structure of the Mu transpososome",
              "Crystal structure of ORF041 from Bacteriophage 37",
              "beta-lactamase", "PIN domain-containing protein",
              "CBS domain-containing protein",
              "Type II toxin-antitoxin system MazF"]:
        t, tags = classify(d)
        print(f"  tier={t}  {d[:46]:48} tags={sorted(tags)}")
