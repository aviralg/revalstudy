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
  package/evals-static.csv \
  package/summarized.fst \
  package/summarized-externals.fst \
  package/undefined.fst \
  package-snapshot/evals-static.csv \
  package-snapshot/summarized.fst \
  package-snapshot/summarized-externals.fst \
  package-snapshot/undefined.fst \
  base/summarized.fst \
  base/summarized-externals.fst \
  base/undefined.fst \
  base/evals-static.csv \
  kaggle/summarized.fst \
  kaggle/summarized-externals.fst \
  kaggle/undefined.fst \
  kaggle/evals-static.csv \
  kaggle/kernel.csv

.PHONY: all
all: $(FILES)

.PHONY: clean
clean:
	-rm -f $(FILES)

corpus.fst:
	$(call CURL,$@,$@)
corpus-all.fst:
	$(call CURL,$@,$@)

package/evals-static.csv:
	$(call CURL,run/package-evals-static/package-evals-static.csv,$@)
package-snapshot/evals-static.csv:
	$(call CURL,run/package-evals-static/package-evals-static.csv,$@)
base/evals-static.csv:
	$(call CURL,run/base-evals-static/base-evals-static.csv,$@)
kaggle/evals-static.csv:
	$(call CURL,run/kaggle-kernels/kaggle-evals-static.csv,$@)
kaggle/kernel.csv:
	$(call CURL,run/kaggle-kernels/kernel.csv,$@)

package-snapshot/%:
	$(call CURL,run/preprocess/$@,$@)
package/%:
	$(call CURL,run/preprocess/$@,$@)
base/%:
	$(call CURL,run/preprocess/$@,$@)
kaggle/%:
	$(call CURL,run/preprocess/$@,$@)
