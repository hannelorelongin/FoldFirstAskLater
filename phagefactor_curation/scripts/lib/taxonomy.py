"""
taxonomy.py -- domain (kingdom) of a FoldSeek hit, from the best source available.

Each hit gets one of Bacteria / Archaea / Eukaryote / Virus / Unknown, and the
source it was read from (`kingdom_source`):

  1. lineage : FoldSeek's own `taxlineage` column (local search, DB with a
               `_taxonomy` file). Exact, needs no extra download.
  2. taxid   : the hit's taxid walked up an NCBI taxdump (nodes.dmp +
               merged.dmp in databases.taxdump). Covers web-API mode, FFAL
               tables and runs searched before taxlineage was requested.
  3. name    : keyword match on the organism name (lexicon lists). Last
               resort: misses many eukaryotic genera.

Hits whose taxid has no domain (metagenome, "uncultured organism", plasmid,
synthetic construct) fall through to 3, which usually returns Unknown.

Unknown is never treated as eukaryotic: the eukaryote filters act on positive
evidence only.
"""
from __future__ import annotations

import os
from pathlib import Path

from lexicon import (
    VIRUS_KEYWORDS as _VIRUS_KEYWORDS,
    ARCHAEA_KEYWORDS as _ARCHAEA_KEYWORDS,
    BACTERIA_KEYWORDS as _BACTERIA_KEYWORDS,
    is_eukaryotic_taxname as _is_eukaryotic_taxname,
)

# NCBI top-level taxa. The lineage names match whatever rank prefix FoldSeek
# writes: older DB taxonomies give "d_Eukaryota", current NCBI dumps
# (superkingdom renamed to domain in 2025) give "-_Eukaryota".
DOMAIN_TAXIDS = {"2": "Bacteria", "2157": "Archaea", "2759": "Eukaryote", "10239": "Virus"}
_DOMAIN_NAMES = {"Bacteria": "Bacteria", "Archaea": "Archaea",
                 "Eukaryota": "Eukaryote", "Viruses": "Virus"}
KINGDOMS = ("Bacteria", "Archaea", "Eukaryote", "Virus")


def kingdom_from_lineage(lineage) -> str:
    """Domain named in a FoldSeek taxlineage string, or "" when there is none."""
    if not lineage or not isinstance(lineage, str):
        return ""
    for tok in lineage.split(";"):
        name = tok.split("_", 1)[1] if tok[1:2] == "_" else tok
        k = _DOMAIN_NAMES.get(name.strip())
        if k:
            return k
    return ""


def kingdom_from_taxname(taxname) -> str:
    """Keyword-based fallback: Bacteria / Archaea / Virus / Eukaryote / Unknown.

    Conservative: unknown taxa return "Unknown" rather than a guess.
    """
    if not taxname or not isinstance(taxname, str):
        return "Unknown"
    t = taxname.strip().lower()
    if not t or t in ("na", "nan", "unknown", "none"):
        return "Unknown"
    # Virus first: some virus names contain a host genus ("Campylobacter virus CP21")
    if any(kw in t for kw in _VIRUS_KEYWORDS):
        return "Virus"
    first = t.split()[0]
    if first in _BACTERIA_KEYWORDS or any(kw in t for kw in ("bacteri", "proteobacter")):
        return "Bacteria"
    if first in _ARCHAEA_KEYWORDS or "archae" in t:
        return "Archaea"
    if _is_eukaryotic_taxname(t):
        return "Eukaryote"
    return "Unknown"


class TaxdumpDomains:
    """taxid -> domain from an NCBI taxdump directory (nodes.dmp, merged.dmp).

    Loaded on first use; a missing directory just makes every lookup return "".
    """

    def __init__(self, taxdump_dir):
        self.dir = Path(taxdump_dir) if taxdump_dir else None
        self._parent = None
        self._merged = {}
        self._memo = {}

    @property
    def available(self) -> bool:
        return bool(self.dir) and (self.dir / "nodes.dmp").is_file()

    def _load(self):
        import numpy as np
        import pandas as pd
        nodes = pd.read_csv(self.dir / "nodes.dmp", sep="\t", header=None,
                            usecols=[0, 2], dtype="int64")
        parent = np.zeros(int(nodes[0].max()) + 1, dtype=np.int64)
        parent[nodes[0].to_numpy()] = nodes[2].to_numpy()
        self._parent = parent
        merged = self.dir / "merged.dmp"
        if merged.is_file():
            m = pd.read_csv(merged, sep="\t", header=None, usecols=[0, 2], dtype="int64")
            self._merged = dict(zip(m[0].tolist(), m[2].tolist()))

    def kingdom(self, taxid) -> str:
        """Domain of taxid, or "" when unknown (no taxdump, bad id, no domain)."""
        try:
            t = int(float(taxid))
        except (TypeError, ValueError):
            return ""
        if t <= 1 or not self.available:
            return ""
        if t in self._memo:
            return self._memo[t]
        if self._parent is None:
            self._load()
        x = self._merged.get(t, t)
        path = []
        k = ""
        for _ in range(200):
            if x >= len(self._parent) or self._parent[x] == 0:
                break          # deleted / unknown id
            if x in self._memo:
                k = self._memo[x]
                break
            path.append(x)
            dom = DOMAIN_TAXIDS.get(str(x))
            if dom:
                k = dom
                break
            nxt = int(self._parent[x])
            if nxt == x:
                break          # root
            x = nxt
        for p in path:
            self._memo[p] = k
        self._memo[t] = k
        return k


_RESOLVER = None


def default_resolver() -> TaxdumpDomains:
    """Resolver on config's TAXDUMP_DIR (env PHAGEFACTOR_TAXDUMP wins)."""
    global _RESOLVER
    if _RESOLVER is None:
        d = os.environ.get("PHAGEFACTOR_TAXDUMP")
        if not d:
            try:
                from config import TAXDUMP_DIR
                d = TAXDUMP_DIR
            except Exception:
                d = None
        _RESOLVER = TaxdumpDomains(d)
    return _RESOLVER


def hit_kingdom(taxid=None, taxname=None, lineage=None, resolver=None) -> tuple:
    """(kingdom, source) for one hit: lineage, then taxid, then name."""
    k = kingdom_from_lineage(lineage)
    if k:
        return k, "lineage"
    k = (resolver or default_resolver()).kingdom(taxid)
    if k:
        return k, "taxid"
    k = kingdom_from_taxname(taxname)
    return k, ("name" if k != "Unknown" else "none")


def add_kingdom_columns(df, resolver=None):
    """Add `kingdom` and `kingdom_source` to a hits DataFrame (in place, returned).

    Uses the taxid / taxname / taxlineage columns that are present; each unique
    combination is resolved once.
    """
    if df is None or len(df) == 0:
        if df is not None:
            df["kingdom"] = []
            df["kingdom_source"] = []
        return df
    def _col(c):
        return df[c].astype(str).where(df[c].notna(), "") if c in df.columns else [""] * len(df)
    keys = list(zip(_col("taxid"), _col("taxname"), _col("taxlineage")))
    res = {k: hit_kingdom(k[0], k[1], k[2], resolver) for k in set(keys)}
    df["kingdom"] = [res[k][0] for k in keys]
    df["kingdom_source"] = [res[k][1] for k in keys]
    return df


def kingdom_counts_log(df) -> str:
    """One-line summary of kingdom_source counts, for step logs."""
    if df is None or "kingdom_source" not in df.columns or len(df) == 0:
        return "no hits"
    c = df["kingdom_source"].value_counts()
    return ", ".join(f"{k}={int(c.get(k, 0))}" for k in ("lineage", "taxid", "name", "none"))
