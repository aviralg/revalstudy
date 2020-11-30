MAKEFLAGS += --no-builtin-rules

.SUFFIXES:
.PHONY: calls corpus kaggle preprocess analysis

ANALYSIS_DIR := analysis
HTML_DIR     := $(ANALYSIS_DIR)/html
DATA_DIR     ?= data
BASE_DIR     := ../..
RUN_DIR      := $(BASE_DIR)/run

RUN_PACKAGE_METADATA_DIR      := $(RUN_DIR)/package-metadata
RUN_PACKAGE_COVERAGE_DIR      := $(RUN_DIR)/package-coverage
RUN_KAGGLE_DIR                := $(RUN_DIR)/kaggle-run
RUN_PACKAGE_EVALS_DIR         := $(RUN_DIR)/package-evals-static
RUN_PACKAGE_TRACE_DIR         ?= $(RUN_DIR)/package-evals-traced.5
RUN_PACKAGE_RUNNABLE_CODE_DIR := $(RUN_DIR)/package-runnable-code-eval

# raw files
RUN_PACKAGE_COVERAGE_FILE      := $(RUN_PACKAGE_COVERAGE_DIR)/coverage.csv
RUN_PACKAGE_RUNNABLE_CODE_FILE := $(RUN_PACKAGE_RUNNABLE_CODE_DIR)/runnable-code-metadata.csv
RUN_PACKAGE_TRACE_RUNS_FILE    := $(RUN_PACKAGE_TRACE_DIR)/run.csv
RUN_PACKAGE_TRACE_LOG_FILE     := $(RUN_PACKAGE_TRACE_DIR)/parallel.log
RUN_CALLS_FILE                 := $(RUN_PACKAGE_TRACE_DIR)/calls.fst
RUN_PROGRAM_FILE               := $(RUN_PACKAGE_TRACE_DIR)/program.fst

RUN_SUM_CORE_FILE   	:= $(RUN_PACKAGE_TRACE_DIR)/summarized-core.fst
RUN_SUM_KAGGLE_FILE 	:= $(RUN_PACKAGE_TRACE_DIR)/summarized-kaggle.fst
RUN_SUM_PKGS_FILE   	:= $(RUN_PACKAGE_TRACE_DIR)/summarized-packages.fst
RUN_RAW_FILE        	:= $(RUN_PACKAGE_TRACE_DIR)/raw.fst
RUN_SUM_EXTERNALS_FILE	:= $(RUN_PACKAGE_TRACE_DIR)/summarized-externals.fst
RUN_SUM_UNDEFINED_FILE  := $(RUN_PACKAGE_TRACE_DIR)/summarized-evals-undefined.fst

PACKAGE_EVALS_FILE         := $(DATA_DIR)/evals.fst
PACKAGE_COVERAGE_FILE      := $(DATA_DIR)/coverage.fst
PACKAGE_METADATA_FILE      := $(DATA_DIR)/metadata.fst
PACKAGE_SLOC_FILE          := $(DATA_DIR)/sloc.fst
PACKAGE_FUNCTIONS_FILE     := $(DATA_DIR)/functions.fst
PACKAGE_RUNNABLE_CODE_FILE := $(DATA_DIR)/runnable-code-metadata.fst
PACKAGE_TRACE_RUNS_FILE    := $(DATA_DIR)/run-trace.fst
PACKAGE_TRACE_LOG_FILE     := $(DATA_DIR)/parallel-trace.fst
CORPUS_FILE                := $(DATA_DIR)/corpus.fst

# kaggle corpus
KAGGLE_KERNELS_FILE := $(DATA_DIR)/kaggle-kernels.fst
KAGGLE_LOG_FILE     := $(DATA_DIR)/parallel-kaggle.fst

# kaggle input - manually - cannot redistribute kaggle stuff
# this is here just to raise error when missing
KAGGLE_KERNELS_DIR  := $(RUN_DIR)/kaggle-kernels
KAGGLE_DATASET_DIR  := $(RUN_DIR)/kaggle-datasets

# kaggle runtime artifacts
RUN_KAGGLE_LOG_FILE         := $(RUN_KAGGLE_DIR)/parallel.log
RUN_KAGGLE_CALLS_FILE       := $(RUN_KAGGLE_DIR)/calls.fst
RUN_KAGGLE_SCRIPTS_FILE     := $(RUN_KAGGLE_DIR)/scripts.txt
RUN_KAGGLE_KERNEL_FILE      := $(RUN_KAGGLE_DIR)/kernels.csv
RUN_KAGGLE_PROGRAM_FILE     := $(RUN_KAGGLE_DIR)/program.fst

# preprocess runtime artifacts
PACKAGE_EVALS_DYNAMIC_FILE	:= $(DATA_DIR)/evals-dynamic.fst

R_DIR        := $(realpath $(BASE_DIR))/R-dyntrace
R_BIN        := $(R_DIR)/bin/R
R_SCRIPT_BIN := $(R_DIR)/bin/Rscript
R            := $(R_BIN) --silent --no-save -e

MERGE_FST  := $(R_SCRIPT_BIN) $(BASE_DIR)/runr/inst/merge-fst.R
MERGE_CSV  := $(R_SCRIPT_BIN) $(BASE_DIR)/runr/inst/merge-csv.R
TSV_TO_FST  = $(R) 'fst::write_fst(readr::read_tsv("$<"), "$@", compress=100)'
RENDER_RMD  = $(R) 'rmarkdown::render("$(1)", output_dir="$(HTML_DIR)")'

all: kaggle preprocess

################################################################################
## CORPUS
################################################################################

# TODO: add all steps for package-evals
# TODO: add all steps for metadata
# TODO: add all steps for runnable code
# TODO: add all steps for runs

$(PACKAGE_EVALS_FILE): $(RUN_PACKAGE_EVALS_DIR)/package-evals.csv

# TODO: why $(RUN_PACKAGE_COVERAGE_DIR)/$(@F) does not work?
$(PACKAGE_COVERAGE_FILE): $(RUN_PACKAGE_COVERAGE_FILE)

$(PACKAGE_METADATA_FILE): $(RUN_PACKAGE_METADATA_DIR)/metadata.csv

$(PACKAGE_SLOC_FILE): $(RUN_PACKAGE_METADATA_DIR)/sloc.csv

$(PACKAGE_FUNCTIONS_FILE): $(RUN_PACKAGE_METADATA_DIR)/functions.csv

$(PACKAGE_RUNNABLE_CODE_FILE): $(RUN_PACKAGE_RUNNABLE_CODE_FILE)

$(PACKAGE_TRACE_RUNS_FILE): $(RUN_PACKAGE_TRACE_RUNS_FILE)

$(PACKAGE_TRACE_LOG_FILE): $(RUN_PACKAGE_TRACE_LOG_FILE)
	$(TSV_TO_FST)

# TODO can there be a rule from csv to fst and from tsv to fst?
data/%.fst:
	$(R) 'fst::write_fst(readr::read_csv("$<"), "$@", compress=100)'

$(CORPUS_FILE): \
  $(PACKAGE_EVALS_FILE) \
  $(PACKAGE_COVERAGE_FILE) \
  $(PACKAGE_METADATA_FILE) \
  $(PACKAGE_SLOC_FILE) \
  $(PACKAGE_FUNCTIONS_FILE) \
  $(PACKAGE_RUNNABLE_CODE_FILE) \
  $(PACKAGE_RUNS_FILE) \
  $(PACKAGE_TRACE_RUNS_FILE) \
  $(PACKAGE_TRACE_LOG_FILE) \
  $(KAGGLE_LOG_FILE) \
  $(KAGGLE_KERNELS_FILE)

	$(call RENDER_RMD,$(ANALYSIS_DIR)/corpus-stage2.Rmd)

corpus: $(CORPUS_FILE)

################################################################################
## CALLS
################################################################################

$(RUN_PROGRAM_FILE): $(CORPUS_FILE)
	$(MERGE_FST) $(RUN_PACKAGE_TRACE_DIR) $(@F)
	$(R) 'fst::write_fst(dplyr::semi_join(dplyr::mutate(fst::read_fst("$(RUN_PROGRAM_FILE)"), package=basename(dirname(dirname(file)))), fst::read_fst("$(CORPUS_FILE)"), by="package"), "$(RUN_PROGRAM_FILE)");'

$(RUN_PACKAGE_COVERAGE_FILE):
	$(MERGE_CSV) $(@D) $(@F)

$(RUN_PACKAGE_RUNNABLE_CODE_FILE):
	$(MERGE_CSV) $(@D) $(@F)

$(RUN_PACKAGE_TRACE_RUNS_FILE):
	$(MERGE_CSV) $(RUN_PACKAGE_TRACE_DIR) $(@F)

$(RUN_CALLS_FILE):
	$(MERGE_FST) $(RUN_PACKAGE_TRACE_DIR) $(@F) calls-ERROR

#$(RUN_SUM_CORE_FILE) $(RUN_SUM_KAGGLE_FILE) $(RUN_SUM_PKGS_FILE) $(RUN_RAW_FILE): $(RUN_CALLS_FILE) $(RUN_KAGGLE_CALLS_FILE) $(RUN_PROGRAM_FILE)
#	$(call RENDER_RMD,$(ANALYSIS_DIR)/enrich_dataset.Rmd)

calls: $(RUN_CALLS_FILE) $(RUN_PROGRAM_FILE)

# FIXME: why does it run twice
#enrich: $(RUN_SUM_CORE_FILE) $(RUN_SUM_KAGGLE_FILE) $(RUN_SUM_PKGS_FILE) $(RUN_RAW_FILE)
preprocess:  $(RUN_CALLS_FILE) $(RUN_KAGGLE_CALLS_FILE)
	$(R_SCRIPT_BIN) $(ANALYSIS_DIR)/preprocess.R $(CORPUS_FILE) $(RUN_CALLS_FILE) $(RUN_KAGGLE_CALLS_FILE) $(PACKAGE_EVALS_DYNAMIC_FILE) $(RUN_SUM_UNDEFINED_FILE) $(RUN_RAW_FILE) $(RUN_SUM_CORE_FILE) $(RUN_SUM_PKGS_FILE) $(RUN_SUM_KAGGLE_FILE) $(RUN_SUM_EXTERNALS_FILE)

analysis: $(RUN_SUM_CORE_FILE) $(RUN_SUM_KAGGLE_FILE) $(RUN_SUM_PKGS_FILE)
	$(call RENDER_RMD,$(ANALYSIS_DIR)/analysis_stable.Rmd)

################################################################################
## KAGGLE
################################################################################

$(KAGGLE_LOG_FILE): $(RUN_KAGGLE_LOG_FILE)
	$(TSV_TO_FST)

$(KAGGLE_KERNELS_FILE): $(RUN_KAGGLE_KERNEL_FILE)

# FIXME: this was supposed to be the kaggle.Rmd, but I cannot call rmarkdown
#        from rmarkdown
$(RUN_KAGGLE_SCRIPTS_FILE) (RUN_KAGGLE_KERNEL_FILE): $(KAGGLE_KERNELS_DIR) $(KAGGLE_DATASET_DIR)
	cd $(ANALYSIS_DIR) && $(R_SCRIPT_BIN) kaggle.R $(realpath $(BASE_DIR))

$(RUN_KAGGLE_LOG_FILE): $(RUN_KAGGLE_SCRIPTS_FILE)
	-parallel \
    -a $(RUN_KAGGLE_SCRIPTS_FILE) \
    --bar \
    --joblog $(RUN_KAGGLE_LOG_FILE) \
    --jobs 50 \
    --results '{1}/' \
    --tagstring '{/}' \
    --timeout 1h \
    --workdir '{1}/' \
    $(realpath $(R_BIN)) CMD BATCH '{1}/run.R' '{1}/run.R.out'

$(RUN_KAGGLE_PROGRAM_FILE): $(RUN_KAGGLE_LOG_FILE)
		$(MERGE_FST) $(@D) $(@F)

$(RUN_KAGGLE_CALLS_FILE): $(RUN_KAGGLE_LOG_FILE)
	$(MERGE_FST) $(RUN_KAGGLE_DIR) calls.fst calls-ERROR

kaggle: $(RUN_KAGGLE_CALLS_FILE) $(RUN_KAGGLE_PROGRAM_FILE)
	$(call RENDER_RMD,$(ANALYSIS_DIR)/kaggle-stage2.Rmd)

download:
	scp prl3:/var/lib/R/project-evalR/run/package-evals-traced.2/sum*.fst $(RUN_PACKAGE_TRACE_DIR)
