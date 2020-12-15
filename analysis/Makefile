## The following is used by Jan to run "usage.Rmd" in docker.

# Run docker
dock:
	docker run --name rstudio -p 8787:8787 -v "$(pwd):/mnt" -e ROOT=TRUE -e DISABLE_AUTH=true fikovnik/rstudio:4.0.2

# If already in use...
killrm:
	docker kill rstudio
	docker rm -f rstudio

# Clean up the generated files..
clean:
	rm -rf *.pdf *.tex *~ *.html *_cache *_files

#  http://localhost:8787    uid/pwd == rstudio
