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

Two things live here:

  UPGRADE_RULES   (used by 04_curate) -- (desc_pattern, top3_pattern, replacement)
                  Swap a generic winning name for a specific one already present
                  in the top-3 FoldSeek hits.
  FUNC_CAT_RULES  (used by 04_curate) -- desc -> PHROG category, for genes with
                  no PHold call at all.
  CAT_KEYWORDS + PHROG_CANON + phrog_category()  (used by 05_build_output) --
                  desc -> PHROG category at the OUTPUT layer, for genome-mode
                  pharokka passthrough rows.
  make_short_name()  (used by 05_build_output and 07_integrate) -- final_product
                  -> a concise `short_name`. Same shape as UPGRADE_RULES (ordered
                  rewrite rules on a product string), so it lives here rather
                  than in a module of its own; it writes a DIFFERENT column,
                  which is the only reason it is a separate function.

KNOWN OVERLAP, deliberately not yet merged
------------------------------------------
FUNC_CAT_RULES and CAT_KEYWORDS both map a description onto a PHROG category and
their patterns overlap, but they are NOT interchangeable: FUNC_CAT_RULES runs
during curation on hypothetical genes with no PHold category, CAT_KEYWORDS runs
at output on pharokka products that already have a real product name, and it
also has the `defense`/`other`/`unknown function` fallbacks that curation does
not use. Merging them would change output categories, so they are kept apart --
side by side here, where the difference is visible.
"""
import re


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
    # "70S-PHIKZ014": phage protein solved bound to the 70S ribosome (8CD1).
    # Looks like a locus tag, but the complex IS the functional evidence.
    (re.compile(r'^(?:30S|50S|70S)[-_ ]([A-Za-z0-9]+)$'),
     None,
     r"ribosome-associated protein \1"),
]

# =============================================================================
# FUNCTION CATEGORY, curation side (step 04)
# =============================================================================
FUNC_CAT_RULES = [
    (re.compile(r'transcription.{0,4}regulat|ci.repressor|sigma.factor|copG|NrdR|'
                r'anti.repressor|HTH.cro|bacteriophage.ci|Com.family.DNA.binding|'
                r'repressor.protein|winged.helix', re.I),
     "transcription regulation"),
    (re.compile(r'DNA.helicase|RNA.polymerase|DNA.gyrase|topoisomerase|'
                r'GemA.DNA.gyrase|gp16.*gyrase|replication.initiation|'
                r'DNA.transposition|primase|DNA.repair', re.I),
     "DNA, RNA and nucleotide metabolism"),
    (re.compile(r'tail.fiber|tail.protein|tail.assembly|baseplate|'
                r'head.decoration|major.tail|minor.tail|tape.measure|'
                r'Phage.tail', re.I),
     "tail"),
    (re.compile(r'portal.protein|head.morphogenesis|capsid|head.and.packaging', re.I),
     "head and packaging"),
    (re.compile(r'head.tail.connector|head.to.tail.connector|connector.protein|'
                r'gp6.like.head.tail|neck.protein', re.I),
     "connector"),
    (re.compile(r'RloG|defense.system|abortive.infection|anti.crispr|anti.phage', re.I),
     "defense"),
    (re.compile(r'integrase|excisionase|transposase|resolvase|invertase', re.I),
     "integration and excision"),
    (re.compile(r'beta.lactamase|antibiotic.resistance', re.I),
     "moron, auxiliary metabolic gene and host takeover"),
    (re.compile(r'deoxyribonuclease|endonuclease|restriction.endonuclease|'
                r'HNH.endonuclease', re.I),
     "DNA, RNA and nucleotide metabolism"),
    # Host-derived metabolic enzymes, ribosomal proteins and metal-uptake
    # regulators carried by the prophage are auxiliary metabolic / host-takeover
    # morons. (Only applied when phold left the category 'unknown' -- see
    # _infer_function_cat -- so phold's own category calls are never overwritten.)
    (re.compile(r'ribosom|zinc.uptake|\bzur\b|fur.family|\bkinase\b|'
                r'oxidoreductase|dehydrogenase|transaldolase|aldolase|racemase|'
                r'reductase|permease|dehydratase|\bsynthase\b|isomerase|epimerase|'
                r'\bmutase\b|\bphosphatase\b|sialyltransferase', re.I),
     "moron, auxiliary metabolic gene and host takeover"),
    (re.compile(r'antitoxin|RelB|HicB|MazE', re.I),
     "other"),   # toxin-antitoxin systems
]

# =============================================================================
# FUNCTION CATEGORY, output side (step 05)
# =============================================================================
PHROG_CANON = {"head and packaging", "connector", "tail",
                "DNA, RNA and nucleotide metabolism", "integration and excision",
                "transcription regulation", "lysis",
                "moron, auxiliary metabolic gene and host takeover",
                "other", "unknown function"}

CAT_KEYWORDS = [
    (re.compile(r"integrase|recombinase|excisionase|transposase|resolvase|invertase|att(achment)? site", re.I), "integration and excision"),
    (re.compile(r"holin|endolysin|\blysin\b|lysozyme|spanin|\blysis\b|cell.wall (hydrolase|amidase)", re.I), "lysis"),
    (re.compile(r"terminase|portal|\bcapsid\b|prohead|procapsid|scaffold|head (protein|maturation|completion|decoration|closure)|major head|packaging", re.I), "head and packaging"),
    (re.compile(r"head.?tail|connector|\bneck\b|adaptor|adapter|stopper", re.I), "connector"),
    (re.compile(r"\btail\b|baseplate|tape.measure|fib(er|re)|spike|sheath|\btube\b|whisker|virion structural|distal tail|Dit\b|tail terminator", re.I), "tail"),
    (re.compile(r"helicase|polymerase|primase|exonuclease|endonuclease|nuclease|\bkinase\b|methyltransferase|methylase|\bligase\b|replicati|topoisomerase|single.strand|ssDNA|ssb\b|nucleotide|ribonucleotide|DNA.binding|recombination|annealing|Holliday", re.I), "DNA, RNA and nucleotide metabolism"),
    (re.compile(r"repressor|regulator|\bcro\b|\bci\b|antiterminat|anti.terminat|transcription|sigma.factor|\bHTH\b|anti.repressor|Cox\b", re.I), "transcription regulation"),
    (re.compile(r"defen[cs]e|restriction|toxin|antitoxin|abortive infection|anti.crispr|\bCBASS\b|\bPARIS\b|\bRM\b system|immunity", re.I), "defense"),
    (re.compile(r"ribosom|\bzur\b|fur.family|oxidoreductase|dehydrogenase|reductase|transferase|permease|metaboli|tRNA|amino.acid|sugar|transport|hydrolase|phosphatase|synthase|isomerase|racemase|epimerase|moron", re.I), "moron, auxiliary metabolic gene and host takeover"),
]

def phrog_category(function, product):
    """Return a canonical PHROG category. Keep `function` if already canonical,
    else infer from the product via the keyword map; else other/unknown."""
    f = str(function or "").strip()
    if f in PHROG_CANON:
        return f
    text = f"{product or ''} {f}"
    for pat, cat in CAT_KEYWORDS:
        if pat.search(text):
            return cat
    return "other" if f and f.lower() not in ("", "nan", "na") else "unknown function"


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
