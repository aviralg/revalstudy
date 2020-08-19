include ../Makevars

IN_DOCKER := docker run \
    --rm \
    -it \
    -e CRAN_MIRROR=$(CRAN_MIRROR) \
    -e CRAN_MIRROR_URL=$(CRAN_MIRROR_URL) \
    -e CRAN_MIRROR_LOCAL=$(DOCKER_CRAN_MIRROR_LOCAL) \
    -e CRAN_MIRROR_LOCAL_URL=$(DOCKER_CRAN_MIRROR_LOCAL_URL) \
    -e R_LIBS=$(DOCKER_R_LIBS) \
    -v "$$(pwd)/$(CRAN_MIRROR_LOCAL):$(DOCKER_CRAN_MIRROR_LOCAL)" \
    -v "$$(pwd)/$(R_LIBS):$(DOCKER_R_LIBS)" \
    $(IMAGE_NAME)

.PHONY: run-bash \
  run-r

run-bash:
	$(IN_DOCKER) R

run-r:
	$(IN_DOCKER) R
