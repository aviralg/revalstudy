################################################################################
# PATHS
################################################################################

DATA_DIR  <- path(params$base_dir, "revalstudy", "inst", "data")
PAPER_DIR <- path(params$base_dir, "revalstudy", "inst", "paper")
RUN_DIR   <- path(params$base_dir, "run")
TAGS_DIR  <- path(PAPER_DIR, "tags")

R_DIR      <- path(params$base_dir, "R-4.0.2")
R_LIBS_DIR <- path(params$base_dir, "library", "4.0")

## INPUTs

PACKAGE_EVALS_FILE         <- path(DATA_DIR, "evals.fst")
PACKAGE_COVERAGE_FILE      <- path(DATA_DIR, "coverage.fst")
PACKAGE_METADATA_FILE      <- path(DATA_DIR, "metadata.fst")
PACKAGE_SLOC_FILE          <- path(DATA_DIR, "sloc.fst")
PACKAGE_REVDEPS_FILE       <- path(DATA_DIR, "revdeps.fst")
PACKAGE_FUNCTIONS_FILE     <- path(DATA_DIR, "functions.fst")
PACKAGE_RUNNABLE_CODE_FILE <- path(DATA_DIR, "runnable-code-metadata.fst")
PACKAGE_RUNS_FILE          <- path(DATA_DIR, "run.fst")
PACKAGE_TRACE_RUNS_FILE    <- path(DATA_DIR, "run-trace.fst")
PACKAGE_TRACE_LOG_FILE     <- path(DATA_DIR, "parallel-trace.fst")

KAGGLE_KERNELS_FILE <- path(DATA_DIR, "kaggle-kernels.fst")
KAGGLE_LOG_FILE     <- path(DATA_DIR, "parallel-kaggle.fst")

## OUTPUTs

# TODO: rename to corpus-stage1.*
CORPUS_S1_FILE         <- path(DATA_DIR, "corpus-1000.txt")
CORPUS_S1_DETAILS_FILE <- path(DATA_DIR, "corpus-1000.fst")
CORPUS_FILE            <- path(DATA_DIR, "corpus.txt")

################################################################################
# GLOBALS
################################################################################

# number of packages to select for the stage 1 - for runs
CORPUS_S1_NUM_PKGS <- 1000
# final number of packages
CORPUS_NUM_PKGS <- 500
# which reverse dependencies shall be considered
WHICH_DEPENDENCIES <- c("Depends", "Imports")
# local cran mirror
CRAN_MIRROR_LOCAL_URL <- paste0("file://", params$base_dir, "/CRAN")
