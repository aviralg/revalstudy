DATA_DIR  <- path(params$base_dir, "data")
PAPER_DIR <- path(params$base_dir, "paper")
TAGS_DIR  <- path(PAPER_DIR, "tags")

## INPUTs

PACKAGE_METADATA     <- path(DATA_DIR, "metadata.csv")
PACKAGE_SLOC         <- path(DATA_DIR, "sloc.csv")

## OUTPUTs

CORPUS_FILE                  <- path(DATA_DIR, "corpus.txt")
CORPUS_REVDEPS               <- path(DATA_DIR, "corpus-revdeps.csv")
CORPUS_REVDEPS_FILE          <- path(DATA_DIR, "corpus-revdeps.txt")
CORPUS_ALL_PACKAGES_FILE     <- path(DATA_DIR, "corpus-all.txt")

