################################################################################
# PATHS
################################################################################

DATA_DIR  <- path(params$base_dir, "revalstudy", "inst", "data")
PAPER_DIR <- path(params$base_dir, "revalstudy", "inst", "paper")
RUN_DIR   <- path(params$base_dir, "run")
PLOT_DIR  <- path(PAPER_DIR, "img")
TAGS_DIR  <- path(PAPER_DIR, "tag")

if (!dir_exists(PLOT_DIR)) dir_create(PLOT_DIR)
if (!dir_exists(TAGS_DIR)) dir_create(TAGS_DIR)

R_DIR      <- path(params$base_dir, "R-4.0.2")
R_LIBS_DIR <- path(params$base_dir, "library", "4.0")

## INPUTs

PACKAGE_EVALS_DYNAMIC_FILE <- path(DATA_DIR, "evals-dynamic.fst")
PACKAGE_EVALS_UNDEFINED_FILE <- path(DATA_DIR, "evals-undefined.fst")
PACKAGE_EVALS_FILE         <- path(DATA_DIR, "evals.fst")
PACKAGE_COVERAGE_FILE      <- path(DATA_DIR, "coverage.fst")
PACKAGE_METADATA_FILE      <- path(DATA_DIR, "metadata.fst")
PACKAGE_SLOC_FILE          <- path(DATA_DIR, "sloc.fst")
PACKAGE_REVDEPS_FILE       <- path(DATA_DIR, "revdeps.fst")
PACKAGE_FUNCTIONS_FILE     <- path(DATA_DIR, "functions.fst")
PACKAGE_RUNNABLE_CODE_FILE <- path(DATA_DIR, "runnable-code-metadata.fst")
PACKAGE_TRACE_RUNS_FILE    <- path(DATA_DIR, "run-trace.fst")
PACKAGE_TRACE_LOG_FILE     <- path(DATA_DIR, "parallel-trace.fst")
CRAN_PROGRAMS_FILE         <- path(DATA_DIR, "cran-programs.fst")

RUN_TRACE_DIR  <- path(RUN_DIR, "package-evals-traced.3")
RUN_KAGGLE_DIR <- path(RUN_DIR, "kaggle-run")

KAGGLE_DEPENDENCIES_FILE <- path(DATA_DIR, "kaggle-dependencies.txt")
KAGGLE_KERNELS_FILE      <- path(DATA_DIR, "kaggle-kernels.fst")
KAGGLE_LOG_FILE          <- path(DATA_DIR, "parallel-kaggle.fst")
KAGGLE_PROGRAMS_FILE     <- path(DATA_DIR, "kaggle-programs.fst")
KAGGLE_CALLS_FILE        <- path(RUN_KAGGLE_DIR, "calls.fst")

## OUTPUTs

# TODO: rename to corpus-stage1.*
CORPUS_S1_FILE         <- path(DATA_DIR, "corpus-1000.txt")
CORPUS_S1_DETAILS_FILE <- path(DATA_DIR, "corpus-1000.fst")
CORPUS_FILE            <- path(DATA_DIR, "corpus.txt")
CORPUS_DETAILS_FILE    <- path(DATA_DIR, "corpus.fst")
CORPUS_PLOT            <- path(PLOT_DIR, "corpus.pdf")

CALLS_FILE               <- path(RUN_TRACE_DIR, "calls.fst")
PROGRAM_FILE             <- path(RUN_TRACE_DIR, "program.fst")
# TODO: rename to SUMMARIZED_...
EVALS_RAW_FILE           <- path(RUN_TRACE_DIR, "raws.fst")
EVALS_SUM_CORE_FILE      <- path(RUN_TRACE_DIR, "summarized-core.fst")
EVALS_SUM_PKGS_FILE      <- path(RUN_TRACE_DIR, "summarized-packages.fst")
EVALS_SUM_KAGGLE_FILE    <- path(RUN_TRACE_DIR, "summarized-kaggle.fst")
EVALS_SUM_EXTERNALS_FILE <- path(RUN_TRACE_DIR, "summarized-externals.fst")

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
# size of a package to be considered as outlier
PACKAGE_SIZE_OUTLIER <- 50000
PACKAGE_EVALS_OUTLIER <- 50
PACKAGE_EVALS_DYN_OUTLIER <- 50
MOST_DOWNLOADED <- 25
# R packages distributed with vanilla R
CORE_PACKAGES <- c(
  "base",
  "compiler",
  "graphics",
  "grDevices",
  "grid",
  "methods",
  "parallel",
  "profile",
  "splines",
  "stats",
  "stats4",
  "tcltk",
  "tools",
  "utils"
)
