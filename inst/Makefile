MAKEFLAGS += --no-builtin-rules

.SUFFIXES:
.PHONY: data

ANALYSIS_DIR := analysis
DATA_DIR     := data
RUN_DIR      := ../../run

RUN_PACKAGE_METADATA_DIR      := $(RUN_DIR)/package-metadata
RUN_PACKAGE_COVERAGE_DIR      := $(RUN_DIR)/package-coverage
RUN_KAGGLE_DIR                := $(RUN_DIR)/kaggle-run
RUN_PACKAGE_EVALS_DIR         := $(RUN_DIR)/package-evals-static
RUN_PACKAGE_TRACE_DIR         := $(RUN_DIR)/package-evals-traced
RUN_PACKAGE_RUNNABLE_CODE_DIR := $(RUN_DIR)/package-runnable-code
RUN_PACKAGE_RUNS_DIR          := $(RUN_DIR)/run-extracted-code

PACKAGE_EVALS_FILE         := $(DATA_DIR)/evals.fst
PACKAGE_COVERAGE_FILE      := $(DATA_DIR)/coverage.fst
PACKAGE_METADATA_FILE      := $(DATA_DIR)/metadata.fst
PACKAGE_SLOC_FILE          := $(DATA_DIR)/sloc.fst
PACKAGE_REVDEPS_FILE       := $(DATA_DIR)/revdeps.fst
PACKAGE_FUNCTIONS_FILE     := $(DATA_DIR)/functions.fst
PACKAGE_RUNNABLE_CODE_FILE := $(DATA_DIR)/runnable-code-metadata.fst
PACKAGE_RUNS_FILE          := $(DATA_DIR)/run.fst
PACKAGE_TRACE_RUNS_FILE    := $(DATA_DIR)/run-trace.fst
PACKAGE_TRACE_LOG_FILE     := $(DATA_DIR)/parallel-trace.fst

KAGGLE_LOG_FILE     := $(DATA_DIR)/parallel-kaggle.fst
KAGGLE_KERNELS_FILE := $(DATA_DIR)/kaggle-kernels.fst

R := R --silent --no-save -e

all: data

$(PACKAGE_EVALS_FILE): $(RUN_PACKAGE_EVALS_DIR)/package-evals.csv

# TODO: why $(RUN_PACKAGE_COVERAGE_DIR)/$(@F) does not work?
$(PACKAGE_COVERAGE_FILE): $(RUN_PACKAGE_COVERAGE_DIR)/coverage.csv

$(PACKAGE_METADATA_FILE): $(RUN_PACKAGE_METADATA_DIR)/metadata.csv

$(PACKAGE_SLOC_FILE): $(RUN_PACKAGE_METADATA_DIR)/sloc.csv

$(PACKAGE_REVDEPS_FILE): $(RUN_PACKAGE_METADATA_DIR)/revdeps.csv

$(PACKAGE_FUNCTIONS_FILE): $(RUN_PACKAGE_METADATA_DIR)/functions.csv

$(PACKAGE_RUNNABLE_CODE_FILE): $(RUN_PACKAGE_RUNNABLE_CODE_DIR)/runnable-code-metadata.csv

$(PACKAGE_RUNS_FILE): $(RUN_PACKAGE_RUNS_DIR)/run.csv

$(PACKAGE_TRACE_RUNS_FILE): $(RUN_PACKAGE_TRACE_DIR)/run.csv

$(PACKAGE_TRACE_LOG_FILE): $(RUN_PACKAGE_TRACE_DIR)/parallel.log
	$(R) 'fst::write_fst(readr::read_tsv("$<"), "$@", compress=100)'

$(KAGGLE_LOG_FILE): $(RUN_KAGGLE_DIR)/parallel.csv

$(KAGGLE_KERNELS_FILE): $(RUN_KAGGLE_DIR)/kernels.csv

data/%.fst: 
	$(R) 'fst::write_fst(readr::read_csv("$<"), "$@", compress=100)'


data: \
  $(PACKAGE_EVALS_FILE) \
  $(PACKAGE_COVERAGE_FILE) \
  $(PACKAGE_METADATA_FILE) \
  $(PACKAGE_SLOC_FILE) \
  $(PACKAGE_REVDEPS_FILE) \
  $(PACKAGE_FUNCTIONS_FILE) \
  $(PACKAGE_RUNNABLE_CODE_FILE) \
  $(PACKAGE_RUNS_FILE) \
  $(PACKAGE_TRACE_RUNS_FILE) \
  $(PACKAGE_TRACE_LOG_FILE) \
  $(KAGGLE_LOG_FILE) \
  $(KAGGLE_KERNELS_FILE)