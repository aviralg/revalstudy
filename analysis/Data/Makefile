HOST := prl2
BASE_URL := https://$(HOST).prl.fit.cvut.cz/mnt/ocfs_vol_00/project-evalr/evalr-experiment
define CURL
  [ -d $(dir $(2)) ] || mkdir -p $(dir $(2)) && \
  if [ -f $(2) ]; then \
    curl --progress-bar -z $(2) -o $(2) $(BASE_URL)/$(1); \
  else \
    curl --progress-bar -o $(2) $(BASE_URL)/$(1); \
  fi
endef

FILES := \
  package/corpus.fst \
  package/code.fst \
  package/corpus.txt \
  package/evals-static.csv \
  package/normalized-expressions.csv \
  package/summarized.fst \
  package/summarized-externals.fst \
  package/undefined.fst \
  package/trace-log.csv \
  package/run-log.csv \
  package/runnable-code.csv \
  package/side-effects.fst \
  base/corpus.txt \
  base/evals-static.csv \
  base/normalized-expressions.csv \
  base/summarized.fst \
  base/summarized-externals.fst \
  base/undefined.fst \
  kaggle/corpus.txt \
  kaggle/evals-static.csv \
  kaggle/kernel.csv \
  kaggle/normalized-expressions.csv \
  kaggle/summarized.fst \
  kaggle/summarized-externals.fst \
  kaggle/undefined.fst

.PHONY: all
all: $(FILES)

.PHONY: clean
clean:
	-rm -f $(FILES)

.PHONY: FORCE

package/%: FORCE
	$(call CURL,run/preprocess/$@,$@)
base/%: FORCE
	$(call CURL,run/preprocess/$@,$@)
kaggle/%: FORCE
	$(call CURL,run/preprocess/$@,$@)
