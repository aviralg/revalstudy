include ../Makevars

.PHONY: base-image install-cran-packages image image-upload

# # TODO make a macro
# RUNR_VERSION := $(shell 2>/dev/null curl -s https://api.github.com/repos/PRL-PRG/runr/commits?sha=typer-oopsla20 | 2>/dev/null jq -r '.[0].sha')
# ifndef RUNR_VERSION
# RUNR_VERSION := $(shell date +%s)
# endif

# CONTRACTR_VERSION := $(shell 2>/dev/null curl -s https://api.github.com/repos/PRL-PRG/contractr/commits?sha=typer-oopsla20 | 2>/dev/null jq -r '.[0].sha')
# ifndef CONTRACTR_VERSION
# CONTRACTR_VERSION := $(shell date +%s)
# endif

CRAN_INSTALL_LOG := $(CRAN_MIRROR_LOCAL)/install.log
LIBRARY_STAT := $(R_LIBS)/stat

DOCKER_LIBRARY_STAT := $(DOCKER_PREFIX)/$(LIBRARY_STAT)

base-image:
	docker build \
    --rm \
    -t $(BASE_IMAGE_NAME) \
    .

IN_DOCKER := docker run \
    --rm \
    -it \
    -e CRAN_MIRROR=$(CRAN_MIRROR) \
    -e CRAN_MIRROR_URL=$(CRAN_MIRROR_URL) \
    -e CRAN_MIRROR_LOCAL=$(DOCKER_CRAN_MIRROR_LOCAL) \
    -e CRAN_MIRROR_LOCAL_URL=$(DOCKER_CRAN_MIRROR_LOCAL_URL) \
    -e R_LIBS=$(DOCKER_R_LIBS) \
    $(BASE_IMAGE_NAME)

$(CRAN_MIRROR_LOCAL_PACKAGES):

$(LIBRARY_STAT): mirror-cran
	-mkdir -p $(R_LIBS)
	$(IN_DOCKER) \
    R -e 'source("$(RUNR_GITHUB_URL)/inst/install-cran-packages.R"); install_cran_packages("$(DOCKER_CRAN_MIRROR_LOCAL_URL)", "$(DOCKER_R_LIBS)")' \
    2>&1 | tee -a $(CRAN_INSTALL_LOG)
	$(IN_DOCKER) \
    R -e 'writeLines(dim(installed.packages("$(DOCKER_R_LIBS)"))[1], "$(DOCKER_LIBRARY_STAT)")'

run-bash:
	$(IN_DOCKER) bash

run-r:
	$(IN_DOCKER) R

mirror-cran:
	-mkdir -p "$(CRAN_MIRROR_LOCAL)/src"
	$(IN_DOCKER) rsync \
    -rtlzv \
    --delete \
    --include='*.tar.gz' \
    --include='PACKAGES*' \
    --exclude='*/*' \
    "$(CRAN_MIRROR)::CRAN/src/contrib" "$(DOCKER_CRAN_MIRROR_LOCAL)/src/"

install-cran-packages: $(LIBRARY_STAT)

image: base-image mirror-cran install-cran-packages

image-upload: image
	docker push $(BASE_IMAGE_NAME)
