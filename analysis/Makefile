
## The following is used by Jan to run "usage_metrics" in docker.

# Run docker
dock:
	docker run --name rstudio -p 8787:8787 -v "$(pwd):/mnt" -e ROOT=TRUE -e DISABLE_AUTH=true fikovnik/rstudio:4.0.2

killrm:
	docker kill rstudio
	docker rm -f rstudio

clean:
	rm -rf *.pdf *.tex *~ *.html *_cache *_files

#  http://localhost:8787    uid/pwd == rstudio
