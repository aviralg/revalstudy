BASE_URL := https://prl3.prl.fit.cvut.cz/mnt/ocfs_vol_00/project-evalr/evalr-experiment
define CURL
  [ -d $(dir $(2)) ] || mkdir -p $(dir $(2)) && \
  curl --progress-bar --retry 99 --retry-max-time 0 -C - -o $(2).part $(BASE_URL)/$(1) && \
  mv $(2).part $(2)
endef

FILES := \
  corpus.fst \
  package-evals-static.csv \
  package/summarized.fst \
  package/summarized-externals.fst \
  package/undefined.fst \
  base/summarized.fst \
  base/summarized-externals.fst \
  base/undefined.fst \
  kaggle/summarized.fst \
  kaggle/summarized-externals.fst \
  kaggle/undefined.fst

.PHONY: all
all: $(FILES)

.PHONY: clean
clean:
	-rm -f $(FILES)

corpus.fst:
	$(call CURL,$@,$@)

package-evals-static.csv:
	$(call CURL,run/package-evals-static/$@,$@)

package/%:
	$(call CURL,run/preprocess/$@,$@)
base/%:
	$(call CURL,run/preprocess/$@,$@)
kaggle/%:
	$(call CURL,run/preprocess/$@,$@)
