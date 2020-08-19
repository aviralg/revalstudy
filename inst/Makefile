include Makevars

JOBS          ?= 70
PACKAGES_FILE ?= data/corpus.txt
TIMEOUT       ?= 30m

RUNR_DIR           := $(R_PROJECT_BASE_DIR)/runr
RUNR_TASKS_DIR     := $(RUNR_DIR)/inst/tasks

ON_EACH_PACKAGE := $(MAKE) on-each-package
MERGE_CSV := $(RUNR_DIR)/inst/merge-csv.R
MERGE_FST := $(RUNR_DIR)/inst/merge-fst.R

RUN_DIR := $(CURDIR)/run
TRACE_EVAL_DIR := $(RUN_DIR)/trace-eval

# runs
CALLS_FST := $(TRACE_EVAL_DIR)/calls.fst

.PHONY: trace-eval

EVAL_CODE_CSV    := $(RUN_DIR)/eval-code/runnable-code.csv
EVAL_RUN_CSV     := $(RUN_DIR)/eval-run/run.csv

$(EVAL_CODE_CSV): export OUTPUT_DIR=$(@D)
$(EVAL_CODE_CSV):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/package-runnable-code-eval.R
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F) runnable-code-metadata.csv

$(CALLS_FST): $(EVAL_CODE_CSV)
$(CALLS_FST): export OUTPUT_DIR=$(@D)
$(CALLS_FST): export START_XVFB=1
$(CALLS_FST):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/run-extracted-code.R ARGS="$(dir $(EVAL_CODE_CSV))/{1/}"
	$(MERGE_FST) "$(OUTPUT_DIR)" $(@F)

eval-code: $(EVAL_CODE_CSV)
trace-eval: $(CALLS_FST)

on-each-package:
	@[ "$(TASK)" ] || ( echo "*** Undefined TASK"; exit 1 )
	@[ -x "$(TASK)" ] || ( echo "*** $(TASK): no such file"; exit 1 )
	@[ "$(OUTPUT_DIR)" ] || ( echo "*** Undefined OUTPUT_DIR"; exit 1 )

	-mkdir -p "$(OUTPUT_DIR)"
	-if [ -z "$(START_XVFB)" ]; then  \
     nohup Xvfb :6 -screen 0 1280x1024x24 >/dev/null 2>&1 & \
     export DISPLAY=:6; \
  fi; \
  parallel \
    -a $(PACKAGES_FILE) \
    --bar \
    --env PATH \
    --jobs $(JOBS) \
    --results "$(OUTPUT_DIR)/parallel.csv" \
    --tagstring "$(notdir $(TASK)) - {/}" \
    --timeout $(TIMEOUT) \
    --workdir "$(OUTPUT_DIR)/{/}/" \
    $(RUNR_DIR)/inst/run-task.sh \
      $(TASK) "$(PACKAGES_SRC_DIR)/{1/}" $(ARGS)
