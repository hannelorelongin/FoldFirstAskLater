#!/usr/bin/env python3

import re

# REGEX for uninformative annotations
uninformative_search = ["whole genome shotgun sequence"] 
uninformative_match = ["unnamed product", 
                       "unknown function",
                       "protein of unknwon function", "protein of unknown function",
                       "conserved domain protein, histidine-rich", 
                       "phage related-protein", 
                       "phage d3 protein", 
                       r"genome assembly, chromosome: [\w+]+", 
                       r"dna, contig(: [\w+]+)?", 
                       r"str. [\w+]+[\d+]+", 
                       r"orf[\w+]+( domain-containing)?( protein)?", 
                       r"gene [\d+]+ protein",
                       r"lin[\d+]+ protein",
                       r"(phage( |-))?(protein )?(putative )?[g]?p[\d+]+\b(-like)?( domain(-containing)?)?(( |-)family)?( protein)?",
                       r"(putative )?uncharacterized protein(?! duf[\d+]+)( \w+)?", 
                       r"((hypothetical|putative|probable|conserved|uncharacterized) )?((hypothetical|putative|probable|conserved|uncharacterized) )?(bacterio|pro)?(phage)((-| )(like|related|associated))?( (hypothetical|putative|probable|conserved|uncharacterized))?( protein)?(,? [g]?p[\d+]+\b)?(, family)?(, putative)?",
                       r"putative similar to (bacterio)?phage protein",
                       r"gifsy-[\d+] prophage protein", 
                       r"hk97( family)?( phage)? protein", 
                       r"(putative )?(conserved )?(\([a-z ]+\) )?((uncharacterized)|(hypothet(h)?ical)|(unannotated))( conserved)?( protein)?(, (pro)?phage-related)?(, isoform [AB])?",
                       r"(predicted|unannotated|uncharacterized|hypothetical|putative( |_)?)?(expressed)?(conserved)?((mobile element)|(integron protein cassette))?(( )?domain)?(constituent)?([\d+]+)?( |_)protein(_-_conserved)?", 
                       r"uncharacterized protein conserved in bacteria(, prophage-related)?", 
                       r"(hypothetical )?genomic island protein",
                       r"warning uniprot entry is obsolete, no information fetched\.",
                       r"warning pdb entry is obsolete, no information fetched\.",
                       r"warning no uniprot entry associated with this pdb entry, no information fetched\."
                       ]

# REGEX for low informative annotations
low_informative_search = ["br0599",
                          r"duf[0-9]+"
                          ] 
low_informative_match = ["putative phage-related exported protein",
                         r"uncharacterized (mitochondrial )?protein ((xf_1581/xf_1686)|(atmg00810-like))", 
                         r"uncharacterized( membrane)? protein ([\w+]+ )?\((upf)[\d+]+( family\))", 
                         r"cson[\d+]+ protein", 
                         r"((hypothetical|predicted|putative) )?(conserved )?((pro)?phage )?(cytosolic|membrane|secreted|exported|cytoplasmic)( associated)?( phage)? protein(, putative)?"
                         ]

# classify into three tiers: low/uninformative/informative annotations
def is_informative_tt(descr):
    low_informative_label = False
    for filter_search in low_informative_search:
        if re.search(filter_search, descr.strip().lower()) != None:
            low_informative_label = True
    for filter_match in low_informative_match:
        if re.fullmatch(filter_match, descr.strip().lower()) != None:
            low_informative_label = True

    if low_informative_label:
        return "low informative"
    else:
        return is_uninformative(descr)

# classify uninformative annotations
def is_uninformative(descr):
    uninformative_label = False
    for filter_search in uninformative_search:
        if re.search(filter_search, descr.strip().lower()) != None:
            uninformative_label = True
    for filter_match in uninformative_match:
        if re.fullmatch(filter_match, descr.strip().lower()) != None:
            uninformative_label = True

    if uninformative_label:
        return "uninformative"
    else:
        return "informative"


