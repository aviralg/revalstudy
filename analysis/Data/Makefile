BASE_URL := https://prl3.prl.fit.cvut.cz/mnt/ocfs_vol_00/project-evalr/evalr-experiment
CURL = curl --progress-bar --retry 99 --retry-max-time 0 -C - -o $(notdir $(1)).part $(BASE_URL)/$(1) && \
  mv $(notdir $(1)).part $(notdir $(1))

FILES := \
  corpus.fst \
  package-evals-static.csv \
  summarized-core.fst \
  summarized-evals-undefined.fst \
  summarized-externals.fst \
  summarized-kaggle.fst \
  summarized-packages.fst

.PHONY: all
all: $(FILES)

.PHONY: clean
clean:
	-rm -f $(FILES)

corpus.fst:
	$(call CURL,$@)

package-evals-static.csv:
	$(call CURL,run/package-evals-static/$@)

summarized-%.fst:
	$(call CURL,run/preprocess/$@)
