## INPUTs

PACKAGE_METADATA     <- path(params$data_dir, "metadata.csv")
PACKAGE_SLOC         <- path(params$data_dir, "sloc.csv")

## OUTPUTs

## corpus only
CORPUS_FILE                  <- path(params$output_dir, "corpus.txt")
CORPUS_REVDEPS               <- path(params$output_dir, "corpus-revdeps.csv")
CORPUS_REVDEPS_FILE          <- path(params$output_dir, "corpus-revdeps.txt")
CORPUS_ALL_PACKAGES_FILE     <- path(params$output_dir, "corpus-all.txt")

TAGS_DIR <- params$output_dir
