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

$(OUTPUT_DIR)/%.html: %.Rmd
	-mkdir -p $(OUTPUT_DIR)
	R -e 'rmarkdown::render("$<", output_dir="$(OUTPUT_DIR)")'

.PHONY: usage
usage: $(OUTPUT_DIR)/usage.html
