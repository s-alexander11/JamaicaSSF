library(taxize)

spc <- "Lutjanus analis"

# Search in databases (checks for typos)
res <- gnr_resolve(spc)
res <- gnr_resolve("Lutjanus analas")

# Get higher taxonomic ranks (from ITIS)
classification(spc, db = "itis")

# Find synonyms (e.g accepted name from old name) from ITIS
synonyms(spc, db = "itis")
synonyms("Mesoprion analis", db = "itis")

library(rfishbase)