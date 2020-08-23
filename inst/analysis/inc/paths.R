DATA_DIR  <- path(params$base_dir, "data")
PAPER_DIR <- path(params$base_dir, "paper")
TAGS_DIR  <- path(PAPER_DIR, "tags")


R_DIR <- "/R/R-4.0.2"

## INPUTs

PACKAGE_METADATA_FILE     <- path(DATA_DIR, "metadata.csv")
PACKAGE_SLOC_FILE         <- path(DATA_DIR, "sloc.csv")
PACKAGE_COVERAGE_FILE     <- path(DATA_DIR, "coverage.csv")
PACKAGE_COVERAGE_LOG      <- path(DATA_DIR, "coverage.log")

## OUTPUTs

CORPUS_FILE                  <- path(DATA_DIR, "corpus.txt")
CORPUS_REVDEPS               <- path(DATA_DIR, "corpus-revdeps.csv")
CORPUS_REVDEPS_FILE          <- path(DATA_DIR, "corpus-revdeps.txt")
CORPUS_ALL_PACKAGES_FILE     <- path(DATA_DIR, "corpus-all.txt")

