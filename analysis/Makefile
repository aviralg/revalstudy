DATA_DIR   := Data
OUTPUT_DIR := html

## The following is used by Jan to run "usage.Rmd" in docker.
## http://localhost:8787    uid/pwd == rstudio

# Run docker
dock:
	docker run --rm --name rstudio -p 8787:8787 -v "$$HOME:$$HOME" -e ROOT=TRUE -e DISABLE_AUTH=true fikovnik/rstudio:4.0.2

# If already in use...
killrm:
	docker kill rstudio
	docker rm -f rstudio

# Clean up the generated files..
clean:
	rm -rf *.pdf *.tex *~ *.html *_cache *_files
	rm -rf $(OUTPUT_DIR)

.PHONY: data
data:
	$(MAKE) -C $(DATA_DIR)

.PHONY: clean-data
clean-data:
	$(MAKE) -C $(DATA_DIR) clean

$(OUTPUT_DIR)/base-%.html: %.Rmd
	-mkdir -p $(OUTPUT_DIR)
	R -e 'rmarkdown::render("$<", output_file = "$@", output_dir="$(OUTPUT_DIR)", params = list(base_dir = "Data/base/"))'

$(OUTPUT_DIR)/package-%.html: %.Rmd
	-mkdir -p $(OUTPUT_DIR)
	R -e 'rmarkdown::render("$<", output_file = "$@", output_dir="$(OUTPUT_DIR)", params = list(base_dir = "Data/package/"))'

$(OUTPUT_DIR)/kaggle-%.html: %.Rmd
	-mkdir -p $(OUTPUT_DIR)
	R -e 'rmarkdown::render("$<", output_file = "$@", output_dir="$(OUTPUT_DIR)", params = list(base_dir = "Data/kaggle/"))'

$(OUTPUT_DIR)/%.html: $(OUTPUT_DIR)/kaggle-%.html $(OUTPUT_DIR)/base-%.html $(OUTPUT_DIR)/package-%.html
	echo "Generating for all datasets"

.PHONY: usage
usage: $(OUTPUT_DIR)/usage.html

.PHONY: corpus
corpus:
	R -e 'rmarkdown::render("corpus.Rmd", output_file = "$(OUTPUT_DIR)/corpus.html", output_dir="$(OUTPUT_DIR)")'

.PHONY: normalized
normalized:
	R -e 'rmarkdown::render("normalized.Rmd", output_file = "$(OUTPUT_DIR)/normalized.html", output_dir="$(OUTPUT_DIR)", params = list(base_dir = "Data/package/"))'

.PHONY: all
all: usage corpus normalized
