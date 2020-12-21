BASE_URL := https://prl3.prl.fit.cvut.cz/mnt/ocfs_vol_00/project-evalr/evalr-experiment
CURL := curl --retry 10 --progress-bar -O $(BASE_URL)
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
	$(CURL)/$@

package-evals-static.csv:
	$(CURL)/run/package-evals-static/$@

summarized-%.fst:
	$(CURL)/run/preprocess/$@
