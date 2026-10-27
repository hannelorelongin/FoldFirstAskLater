#!/usr/bin/env python3
"""
rules.py -- ordered RULES, as opposed to vocabulary.

lexicon.py owns the word-lists: "which words mean X". This file owns the ordered
rules that consume them: "given this description, rewrite it / assign it a
category". The distinction is not cosmetic --

  * a lexicon entry is a set member; order is irrelevant, and adding one widens
    a test by exactly that one string.
  * a rule here is (pattern -> action) and FIRST MATCH WINS. Adding a rule at the
    top can shadow every rule below it.

What lives here:

  UPGRADE_RULES    (step 04) -- (desc_pattern, top3_pattern, replacement): swap a
                   generic winning name for a specific one in the FoldSeek top-3.
  CATEGORY_RULES   (steps 04, 05, 07) -- description -> PHROG category.
                   category_from_text() / phrog_category() apply it.
  make_short_name() (steps 05, 07) -- final_product -> a concise `short_name`.
"""
import re

from lexicon import DEFENSE_SYSTEM_TERMS, DEFENSE_TA_TERMS, PHAGE_SPECIFIC, PHAGE_CONTEXT

# Category of a toxin-antitoxin gene. PHROG / PHold convention: TA genes are
# morons. Set it to "defense" to count TA systems as defense everywhere.
TA_CATEGORY = "moron, auxiliary metabolic gene and host takeover"


# =============================================================================
# NAME UPGRADES (step 04)
# =============================================================================
UPGRADE_RULES = [
    # Prefer specific phage repressor over generic "Transcriptional regulator"
    (re.compile(r'^transcriptional regulator$', re.I),
     re.compile(r'bacteriophage\s+[Cc][Ii]\s+repressor|ci\s+repressor\s+protein', re.I),
     None),
    # Prefer GTPase Era (a bacterial ribosome assembly factor) over structural
    # "Four helix bundle protein" -- EST3 telomere replication protein is a
    # false structural homology from yeast; GTPase Era is the correct bacterial hit.
    (re.compile(r'^four helix bundle protein$', re.I),
     re.compile(r'\bGTPase Era\b', re.I),
     None),
    # Prefer Endonuclease I over the broader "Deoxyribonuclease"
    (re.compile(r'^deoxyribonuclease$', re.I),
     re.compile(r'\bendonuclease\s+I\b', re.I),
     None),
    # Prefer Phosphocholine transferase AnkX over generic ankyrin repeat
    (re.compile(r'^ankyrin repeat', re.I),
     re.compile(r'phosphocholine transferase|AnkX', re.I),
     None),
    # Biotin operon repressor + phage associated context -> compound name
    (re.compile(r'^winged helix.turn.helix domain', re.I),
     re.compile(r'biotin operon repressor', re.I),
     "{match} (phage associated)"),
    # Prefer antitoxin RelB over CopG when both are in top3;
    # otherwise expand CopG to full descriptive name
    (re.compile(r'^CopG family', re.I),
     re.compile(r'antitoxin RelB|RelB\b', re.I),
     "CopG family transcriptional regulator (putative antitoxin, RHH family)"),
    # Prefer HicB antitoxin qualifier for anti-repressor
    (re.compile(r'^anti.repressor$', re.I),
     re.compile(r'HicB family antitoxin|hicB', re.I),
     "anti-repressor (putative HicB antitoxin)"),
    # Resolvase beats generic DNA binding protein
    (re.compile(r'^DNA binding protein$', re.I),
     re.compile(r'resolvase', re.I),
     None),
    # Com family DNA-binding -> conservative generic name (top-3 don't agree on specifics)
    (re.compile(r'^Com family DNA-binding', re.I),
     None,   # unconditional
     "transcription regulator"),
    # Strip "domain-containing" from tail fiber descriptions
    (re.compile(r'^tail fiber domain.containing protein$', re.I),
     None,   # no top3 lookup needed -- unconditional rename
     "Tail fiber protein"),
    # YopX: strip verbose "domain-containing protein" suffix
    (re.compile(r'^YopX protein domain.containing protein', re.I),
     None,
     "YopX protein"),
    # "minor tail protein / TIGR04255" -> "minor tail protein"
    (re.compile(r'^minor tail protein\s*/\s*TIGR', re.I),
     None,
     "minor tail protein"),
    # "DNA helicase / AAA family ATPase" -> "DNA helicase" (helicases are ATPases)
    (re.compile(r'^DNA helicase\s*/\s*AAA family ATPase', re.I),
     None,
     "DNA helicase"),
    # gp16 (Mu-like) -> shorter display name with GemA note (user-curated form)
    (re.compile(r'Mu-like prophage protein gp16.*GemA DNA gyrase', re.I),
     None,
     "Mu-like gp16 (putative GemA DNA gyrase)"),
    # gp16 (Mu-like) without GemA yet -> add GemA note first, then shorten above
    (re.compile(r'^Mu-like prophage protein gp16$', re.I),
     None,
     "Mu-like gp16 (putative GemA DNA gyrase)"),
    # Bacteriophage CI repressor: fix lowercase "ci" -> "CI" (standard nomenclature)
    (re.compile(r'(?i)bacteriophage\s+ci\s+repressor', re.I),
     None,
     "Bacteriophage CI repressor protein"),
    # HTH cro/C1-type domain-containing protein -> concise form with domain in parens
    (re.compile(r'^HTH cro/C1-type domain.containing protein$', re.I),
     None,
     "Transcription regulator (HTH cro/C1-type domain)"),
    # Transcriptional repressor NrdR-like N-terminal domain-containing protein -> NrdR
    (re.compile(r'^Transcriptional repressor NrdR.*$', re.I),
     None,
     "Transcriptional repressor NrdR"),
    # Resolvase HTH domain-containing protein -> concise form
    (re.compile(r'^Resolvase HTH domain.containing protein$', re.I),
     None,
     "Resolvase (HTH domain)"),
    # 'UPF0102 protein YraN' -> 'YraN'. Letters-only trailing token = gene
    # symbol; a locus tag ('UPF0200 protein MM_1313') is rejected upstream.
    (re.compile(r'^UPF\d+\s+protein\s+([A-Za-z]+)$'),
     None,
     r"\1"),
    # "Bacteriophage T4, Gp53, baseplate wedge protein" -> "baseplate wedge
    # protein": the phage + gp number say where the reference structure came
    # from; the function is the part after them.
    (re.compile(r'^(?:bacterio)?phage\s+[^,]+,\s*gp\s?\d+\w*,\s*(.+)$', re.I),
     None,
     r"\1"),
    # DksA C4-type zinc-finger domain = the DksA/TraR family of small
    # RNA-polymerase-binding regulators (phage/plasmid TraR-like proteins).
    # Named, not just stripped: stripping "domain-containing protein" blindly
    # would turn "Zinc ribbon domain-containing protein" into "Zinc ribbon".
    (re.compile(r'^DksA C4-type domain.containing protein$', re.I),
     None,
     "DksA/TraR family C4-type zinc finger protein"),
    # SMODS-associating 2TM effector = SLATT domain: membrane effector of
    # CBASS-like (SMODS nucleotidyltransferase) conflict systems (Burroughs 2015).
    (re.compile(r'^SMODS.associating 2TM.*$', re.I),
     None,
     "SMODS-associated 2TM effector (SLATT domain)"),
    # Cami1: cA4-activated CARF-RelE ribonuclease of type III CRISPR-Cas,
    # captured by the ribosomal stalk (PDB 8PHB / 8PHJ).
    (re.compile(r'^Cami1$', re.I),
     None,
     "Cami1 (CARF-RelE ribonuclease, type III CRISPR ancillary)"),
    # "70S-PHIKZ014": phage protein solved bound to the 70S ribosome (8CD1).
    # Looks like a locus tag, but the complex IS the functional evidence.
    (re.compile(r'^(?:30S|50S|70S)[-_ ]([A-Za-z0-9]+)$'),
     None,
     r"ribosome-associated protein \1"),
]

# =============================================================================
# FUNCTION CATEGORY (steps 04, 05, 07)
# =============================================================================
# One ordered map, description -> PHROG category; first match wins. Specific
# phrases come before the broad words they contain ("terminase endonuclease"
# is head and packaging, "tail lysozyme" is tail, "restriction endonuclease"
# is defense), and generic enzyme classes come last.
PHROG_CANON = {"head and packaging", "connector", "tail",
               "DNA, RNA and nucleotide metabolism", "integration and excision",
               "transcription regulation", "lysis",
               "moron, auxiliary metabolic gene and host takeover",
               "other", "unknown function"}

_DNA = "DNA, RNA and nucleotide metabolism"
_MORON = "moron, auxiliary metabolic gene and host takeover"

CATEGORY_RULES = [
    # structural phrases that contain a broader enzyme / regulator word
    (re.compile(r"terminase|portal|\bcapsid\b|prohead|procapsid|scaffold|head.morphogenesis|"
                r"head (protein|maturation|completion|closure)|major head|packaging", re.I),
     "head and packaging"),
    (re.compile(r"head.?(to.?)?tail|connector|\bneck\b|adaptor|adapter|stopper", re.I),
     "connector"),
    (re.compile(r"\btail\b|baseplate|tape.measure|\bfib(er|re)|\bspike\b|sheath|"
                r"\btube\b|whisker|virion structural|\bDit\b|head.decoration", re.I),
     "tail"),
    (re.compile(r"integrase|recombinase|excisionase|transposase|resolvase|invertase|"
                r"att(achment)? site", re.I),
     "integration and excision"),
    (re.compile(r"\b(?:anti)?holin|endolysin|\blysin\b|lysozyme|spanin|\blysis\b|"
                r"cell.wall (hydrolase|amidase)|muramidase|amidase", re.I),
     "lysis"),
    # defense before DNA metabolism, so 'Restriction endonuclease' is defense
    (re.compile(DEFENSE_SYSTEM_TERMS, re.I), "defense"),
    (re.compile(DEFENSE_TA_TERMS, re.I), TA_CATEGORY),
    # regulators before DNA metabolism: an HTH / sigma factor binds DNA to regulate
    (re.compile(r"transcription|repressor|\bregulator\b|regulatory protein|\bcro\b|"
                r"\bci\b|anti.?terminat|sigma.factor|\bHTH\b|helix.turn.helix|winged.helix|copG|NrdR|\bCox\b|"
                r"DksA|\bTraR\b|RNAP.interacting", re.I),
     "transcription regulation"),
    (re.compile(r"beta.lactamase|antibiotic.resistance", re.I), _MORON),
    (re.compile(r"helicase|polymerase|primase|nuclease|\bRNase\b|DNA.?methyl|"
                r"cytosine.?methyl|adenine.?methyl|\bmethylase|\bligase\b|replicati|"
                r"topoisomerase|gyrase|single.strand|ssDNA|\bssb\b|nucleotid|DNA.binding|DNA.repair|recombination|"
                r"annealing|Holliday|transposition", re.I),
     _DNA),
    # host-derived metabolic enzymes, ribosomal proteins, transporters
    (re.compile(r"ribosom|zinc.uptake|\bzur\b|fur.family|\bkinase\b|oxidoreductase|"
                r"dehydrogenase|reductase|aldolase|racemase|permease|dehydratase|"
                r"\bsynthase\b|synthetase|isomerase|epimerase|\bmutase\b|phosphatase|"
                r"transferase|metaboli|tRNA|amino.acid|sugar|transport|moron", re.I),
     _MORON),
    # an enzyme class is a function even without a PHROG category of its own
    (re.compile(r"peptidase|protease|glycosidase|hydrolase|esterase|\blyase\b", re.I),
     "other"),
]


def category_from_text(text: str) -> str:
    """PHROG category inferred from a description, or 'unknown function'."""
    if not text or not isinstance(text, str):
        return "unknown function"
    for pat, cat in CATEGORY_RULES:
        if pat.search(text):
            return cat
    return "unknown function"


def phrog_category(function, product):
    """Canonical PHROG category: keep `function` if already canonical, else infer
    it from product + function text; else 'other' (non-empty) / 'unknown function'."""
    f = str(function or "").strip()
    if f in PHROG_CANON:
        return f
    cat = category_from_text(f"{product or ''} {f}")
    if cat != "unknown function":
        return cat
    return "other" if f and f.lower() not in ("nan", "na") else "unknown function"


# =============================================================================
# SHORT NAME (steps 05 / 07)
# =============================================================================
# Derive a concise `short_name` from `final_product`
# ====================================================================
# Used by 05_build_output.py to add a `short_name` column placed BEFORE
# `final_product`. It does NOT touch the curation decision tree (03/04) — it is a
# pure, deterministic post-pass on the already-chosen final_product string, so it
# is easy to audit and tweak and cannot change which annotation was selected.
# 
# Rule order (first match wins), distilled from per-gene review:
#   1. Sub-DB "Name (Category): description"  -> Name        (RecN, Cya, Ibes,
#        "Hemolysin HlyA", LapB)   [VFDB/CARD/DefenseFinder formatting]
#   2. Slash alternatives "A / B / C"         -> clean(A)     (primary call;
#        "transaldolase / Dihydrodipicolinate..." -> transaldolase;
#        "baseplate wedge subunit / Phage protein GP46" -> baseplate wedge subunit)
#   3. Decoration stripping (always): drop (Fragment), (Modular protein), (EC ...),
#        ", Tyr-sensitive", "from bacteriophage X", ", catalytic domain",
#        "Lambda family ", ", lambda family", "HK97 gpNN family ", "domain-
#        containing protein"->core, leading "Phage/Prophage/Putative".
#   4. Trailing gene symbol "<long descriptor> XxxN" -> symbol (DinI, TagO, BamE,
#        YfiB, DapA) when >=3 descriptor words precede it.
#   5. Head enzyme noun: collapse long enzyme names to their class word
#        (aldolase, dehydrogenase, oxidoreductase, hydrolase, ...).
#   6. Otherwise: the cleaned string, trimmed to <=5 words.
# 
# These are heuristics — a first pass. Edge cases that need a specific wording
# (e.g. "Adhesion protein LapB", "T4SSB protein IcmN") can be handled with a
# small manual override map (see `input/overrides.tsv`); rule 1/4 give the gene
# symbol which is a sensible default in the meantime.

# CamelCase-style gene symbols: DinI, TagO, BamE, YfiB, DapA, HlyA, RecN, Zur,
# IcmN. Require an initial capital, some lowercase, then a capital or digit, so
# plain English words (e.g. "Repressor") don't match.
_GENE_SYMBOL = re.compile(r'^[A-Z][a-z]{1,4}[A-Z0-9][A-Za-z0-9]*$')

# enzyme/function class "head nouns" — collapse long names to these.
_HEAD_NOUNS = [
    "transaldolase", "aldolase", "dehydrogenase", "oxidoreductase", "reductase",
    "dehydratase", "racemase", "epimerase", "isomerase", "mutase", "synthetase",
    "synthase", "transferase", "kinase", "phosphatase", "hydrolase", "nuclease",
    "ligase", "permease", "peptidase", "protease", "phosphodiesterase",
    "cyclase", "deaminase", "hydroxylase", "decarboxylase", "carboxylase",
]
_HEAD_NOUN_RE = re.compile(r'\b(' + "|".join(_HEAD_NOUNS) + r')\b', re.I)

# Canonical short symbols for defense / TA / restriction-modification families
# (first match wins). Keeps short_name a clean symbol instead of a truncated
# phrase (e.g. "Type I restriction modification DNA specificity..." -> "Type I RM").
_DEFENSE_SHORT = [
    (re.compile(r'type\s+I\b.*restriction', re.I),   "Type I RM"),
    (re.compile(r'type\s+II\b.*restriction', re.I),  "Type II RM"),
    (re.compile(r'type\s+III\b.*restriction', re.I), "Type III RM"),
    (re.compile(r'\bhicA\b', re.I),                  "HicA"),
    (re.compile(r'\bhicB\b', re.I),                  "HicB"),
    (re.compile(r'\bmazF\b|mazF-like', re.I),        "MazF"),
    (re.compile(r'\brelE\b|relE-like', re.I),        "RelE"),
    (re.compile(r'death.on.curing|\bdoc-?like', re.I), "Doc"),
    (re.compile(r'\babiH\b', re.I),                  "AbiH"),
    (re.compile(r'\bsir\s*2\b|\bsirtuin\b', re.I),   "Sir2"),
    (re.compile(r'\bvacA\b|vacuolating cytotoxin', re.I), "VacA"),
]

# trailing conjunction/preposition/article left by the <=5-word trim
_DANGLING_TAIL = re.compile(r'\s+(and|or|of|the|a|an|with|to|in|for|by|from)$', re.I)

_DECORATION = [
    re.compile(r'\s*\((fragment|modular protein|partial)\)', re.I),
    re.compile(r'\s*\(EC[ :][\d.\-]+\)', re.I),
    re.compile(r'\s*,\s*(tyr|phe|trp|his)-?sensitive', re.I),
    re.compile(r'\s+from bacteriophage\s+\w+', re.I),
    re.compile(r'\s*,?\s*catalytic domain', re.I),
    re.compile(r'\s*,\s*contains\b.*$', re.I),
    re.compile(r'\s*,?\s*lambda family', re.I),
    re.compile(r'\bHK97\s+gp\d+\s+family\s+', re.I),
    re.compile(r'\s*\(ACLAME[^)]*\)', re.I),
    re.compile(r'\s*\(Modular protein\)', re.I),
    re.compile(r'\s+\(IPT/TIG domain\)', re.I),
]
_LEADING = re.compile(r'^(putative|prophage|phage|bacteriophage|probable)\s+', re.I)
_TRAIL_NUM = re.compile(r'\s+\d+$')


def _strip_decoration(s: str) -> str:
    for pat in _DECORATION:
        s = pat.sub("", s)
    s = s.replace("domain-containing protein", "protein")
    s = _TRAIL_NUM.sub("", s).strip(" /,;")
    # collapse repeated leading Phage/Putative (e.g. "Phage holin" -> "holin")
    prev = None
    while prev != s:
        prev = s
        s = _LEADING.sub("", s).strip()
    return s.strip() or prev


# -----------------------------------------------------------------------------
# PRODUCT-NAME CLEAN-UP (used by the FoldSeek top-3 vote, foldseek_scoring)
# -----------------------------------------------------------------------------
# Database titles often decorate a correct name: a hedge ('Putative ...'), a
# phage-lineage family suffix (', HK97 family', ', TP901 family, core region'),
# a fragment tag, or an organism prefix ('Cyanophage A-1(L) tail fiber',
# 'Bacteriophage lambda head decoration protein D'). Unlike _strip_decoration
# (short_name), this keeps a full name: 'Phage portal protein' stays as is.
_HEDGE_PREFIX = re.compile(r"^(?:putative|probable|possible|predicted)\s+", re.I)
_HEDGE_INNER  = re.compile(r",\s*(?:putative|probable|possible|predicted)\b", re.I)
_FAMILY_TAIL  = re.compile(r"(?:,\s*[^,]*\b(?:family|subfamily|superfamily|region)\b[^,]*)+$", re.I)
_ORGANISM_PFX = re.compile(r"^(?:bacteriophage|cyanophage|enterobacteria\s+phage|phage)\s+"
                           r"(?!gp\d)(?:lambda|mu|[a-z0-9()\-]*\d[a-z0-9()\-]*)\s+", re.I)
# fragment/partial, EC number, 'from bacteriophage X', 'lambda family',
# 'HK97 gpNN family', ACLAME tag, '(Modular protein)'
_NAME_DECORATION = tuple(_DECORATION[i] for i in (0, 1, 3, 6, 7, 8, 9))
# phage STRUCTURAL proteins: there a ', X family' suffix is a lineage label
# ('Phage portal protein, HK97 family'); on regulators/toxins it is the function
# ('Phage transcriptional regulator, RinA family', 'Addiction module toxin, HicA family')
_STRUCTURAL = re.compile(r"portal|capsid|\btail|tape.measure|baseplate|terminase|\bhead\b|\bneck\b|"
                         r"connector|sheath|\btube\b|scaffold|decoration|prohead|collar|fib(?:er|re)", re.I)
# a named phage function, gpNN labels excluded ('GpU' alone is no phage context)
_PHAGE_FUNCTION = re.compile(rf"(?!gp\d)(?:{PHAGE_SPECIFIC.pattern})|{_STRUCTURAL.pattern}", re.I)
_NAME_STOP = {"protein", "proteins", "phage", "prophage", "bacteriophage", "putative",
              "probable", "possible", "predicted", "family", "the", "of", "and", "a",
              "fragment", "like"}


def _core_words(name: str) -> set:
    return {t for t in re.findall(r"[a-z0-9]+", name.lower()) if t not in _NAME_STOP}


def clean_name(desc: str) -> str:
    """'Putative phage tail tube protein'            -> 'Phage tail tube protein'
       'Phage portal protein, putative, A118 family' -> 'Phage portal protein'
       'Cyanophage A-1(L) tail fiber'                -> 'Tail fiber'
       'Addiction module toxin, HicA family'         -> unchanged (family IS the information)
    A name is never cleaned into one without content words; then it is returned as is."""
    orig = str(desc or "").strip()
    d = orig
    for pat in _NAME_DECORATION:
        d = pat.sub("", d)
    d = _HEDGE_INNER.sub("", d)
    d = _HEDGE_PREFIX.sub("", d).strip()
    # a lineage/family suffix on a phage STRUCTURAL protein adds little; elsewhere keep it
    stripped = _FAMILY_TAIL.sub("", d).strip(" ,")
    if stripped != d and len(stripped.split()) >= 2 and _STRUCTURAL.search(stripped):
        d = stripped
    # drop an organism prefix only when the rest still names a phage function
    # ('Phage P1 portal protein' -> 'Portal protein'); 'Phage P2 GpU' and
    # 'Bacteriophage Mu P protein' keep it, as it is their only phage context
    rest = _ORGANISM_PFX.sub("", d).strip(" ,")
    if rest != d and _PHAGE_FUNCTION.search(rest):
        d = rest
    if d == orig or not _core_words(d):
        return orig                       # untouched (case included), or nothing left
    # capitalise only a plain word exposed by removing a prefix ('putative tail
    # tube' -> 'Tail tube'); keep 'rRNA', 'dnaB', 'IS110' as they are
    if d[:1].islower() and d[1:2].islower() and orig[:1].isupper():
        d = d[:1].upper() + d[1:]
    return d


def name_alternatives(desc: str) -> list:
    """Core-word sets of a cleaned name; 'A/B' names give one set per alternative."""
    return [w for w in (_core_words(a) for a in re.split(r"\s*/\s*", clean_name(desc))) if w]


# Words that make a different protein of the one they qualify: a helper
# ('tail fiber ASSEMBLY protein'), an opponent ('ANTI-repressor', 'integrase
# INHIBITOR') or a partner ('integrase-BINDING protein').
_OTHER_PROTEIN = re.compile(
    r"^(?:anti\w*|assembly|chaperone|chaperonin|inhibitor|binding|maturation|"
    r"activator|modulator|regulator|regulatory|adaptor|adapter|scaffold\w*)$", re.I)


def _function_words(name: str) -> list:
    """Core-word sets per alternative, with 'anti-X'/'X-binding' split so the
    qualifier is its own word."""
    return [_core_words(re.sub(r"[-/]", " ", a))
            for a in re.split(r"\s*/\s*", clean_name(name)) if _core_words(a)]


def same_function(a: str, b: str) -> bool:
    """One name's core words are contained in the other's (for any alternative),
    and the extra words do not make it another protein:
    'Portal protein' ~ 'Phage portal protein, HK97 family';
    'Putative transposase/integrase' ~ 'Site-specific integrase';
    NOT 'Phage tail sheath' ~ 'Phage tail tape measure protein';
    NOT 'Tail fiber protein' ~ 'Tail fiber assembly protein'; NOT 'Repressor' ~ 'Anti-repressor'."""
    for x in _function_words(a):
        for y in _function_words(b):
            if x <= y or y <= x:
                if not any(_OTHER_PROTEIN.match(w) for w in x ^ y):
                    return True
    return False


def make_short_name(final_product, phold_phrog=None) -> str:
    """Return a concise short_name for a final_product string."""
    if final_product is None:
        return ""
    desc = str(final_product).strip()
    if not desc or desc.lower() in ("hypothetical protein", "nan", "na", ""):
        return "hypothetical protein" if "hypoth" in desc.lower() else ""

    # Rule 1: sub-DB "Name (Category): description". Use the Name as short_name
    # ONLY when it is a real gene symbol (RecN, Cya, HlyA, LapB, Ibes) — not a
    # phenotype/system label like "Lateral flagella" or "Biofilm". Otherwise fall
    # through and shorten the functional description instead.
    m = re.match(r'^(.{2,40}?)\s*\([^)]+\):\s*(.+)$', desc)
    if m:
        name, descpart = m.group(1).strip(), m.group(2).strip()
        name_toks = name.split()
        looks_like_symbol = (
            any(_GENE_SYMBOL.match(t) or t.isupper() for t in name_toks)
            or (len(name_toks) == 1 and len(name) <= 5)   # Cya, Ibes, Lap
        )
        if looks_like_symbol:
            return name
        desc = descpart   # phenotype label -> shorten the description instead

    # Rule 2: slash alternatives -> take the first (the primary/chosen call)
    if " / " in desc:
        desc = desc.split(" / ")[0].strip()

    # Rule 3: strip decoration
    desc = _strip_decoration(desc)

    # Rule 3.5: canonical short symbol for a defense/TA/RM family
    for pat, sym in _DEFENSE_SHORT:
        if pat.search(desc):
            return sym

    # Rule 4: trailing gene symbol after a long descriptor -> symbol
    toks = desc.split()
    if len(toks) >= 4 and _GENE_SYMBOL.match(toks[-1]):
        return toks[-1]

    # Rule 5: collapse long enzyme names to their head noun. Triggers when the
    # name is wordy (>3 tokens) OR carries a long chemical-substrate prefix
    # (any token >12 chars, e.g. "Phospho-2-dehydro-3-deoxyheptonate aldolase").
    if len(toks) > 3 or any(len(t) > 12 for t in toks):
        hits = _HEAD_NOUN_RE.findall(desc)
        if hits:
            return hits[-1].lower()

    # Rule 6: trim to <=5 words
    out = " ".join(toks[:5]) if len(toks) > 5 else desc
    # the word-trim can cut INSIDE a parenthetical ("tail fiber protein (Large
    # polyvalent") -> drop the dangling "(...": never leave an unbalanced bracket.
    if out.count("(") > out.count(")"):
        out = out[:out.rfind("(")].strip()
    out = out.strip(" -/,;")
    # drop a dangling trailing conjunction/preposition left by the word-trim
    # ("membrane-bound chemoreceptor sensing arginine and" -> "... arginine")
    out = _DANGLING_TAIL.sub("", out).strip(" -/,;")
    # never return a useless bare "protein" (e.g. from stripping "Prophage protein")
    if out.strip().lower() in ("", "protein"):
        return "phage protein"
    return out
