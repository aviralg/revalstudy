HOST := prl2
BASE_URL := https://$(HOST).prl.fit.cvut.cz/mnt/ocfs_vol_00/project-evalr/evalr-experiment
define CURL
  [ -d $(dir $(2)) ] || mkdir -p $(dir $(2)) && \
  curl --progress-bar --retry 99 --retry-max-time 0 -C - -z $(2) -o $(2).part $(BASE_URL)/$(1) && \
  [ -f $(2) ] || mv $(2).part $(2)
endef

FILES := \
  corpus.fst \
  corpus-all.fst \
  package/code.fst \
  package/corpus.txt \
  package/evals-static.csv \
  package/normalized-expressions.csv \
  package/summarized.fst \
  package/summarized-externals.fst \
  package/undefined.fst \
  package-snapshot/evals-static.csv \
  package-snapshot/summarized.fst \
  package-snapshot/summarized-externals.fst \
  package-snapshot/undefined.fst \
  package-snapshot/code.fst \
  package-snapshot/normalized-expressions.csv \
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

corpus.fst: FORCE
	$(call CURL,$@,$@)
corpus-all.fst: FORCE
	$(call CURL,$@,$@)
kaggle/kernel.csv: FORCE
	$(call CURL,run/kaggle-kernels/kernel.csv,$@)

package-snapshot/%: FORCE
	$(call CURL,run/preprocess/$@,$@)
package/%: FORCE
	$(call CURL,run/preprocess/$@,$@)
base/%: FORCE
	$(call CURL,run/preprocess/$@,$@)
kaggle/%: FORCE
	$(call CURL,run/preprocess/$@,$@)
