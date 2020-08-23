#!/bin/bash

DEF_PORT=8787
DEF_CMD=/init

DOCKER_IMAGE_NAME=prlprg/project-evalr
CONTAINER_NAME=project-eval-rstudio

function show_help() {
    echo "Usage: $(basename $0) [-p NUM ] [CMD]"
    echo
    echo "where:"
    echo
    echo "  -p NUM      (optional) port for RStudio (defaults to $DEF_PORT)"
    echo "  CMD         (optional) command to run (defaults to RStudio)"
    echo
}

port=$DEF_PORT
cmd=$DEF_CMD

while getopts "hp:" opt; do
    case "$opt" in
    h)
        show_help
        exit 0
        ;;
    p)  port=$OPTARG
        ;;
    esac
done

shift $((OPTIND -1))

[ $# -gt 0 ] && cmd="$@"

docker run \
  -ti \
  --rm \
  --name "$CONTAINER_NAME" \
  -e ROOT=TRUE \
  -e DISABLE_AUTH=true \
  -e USERID=$(id -u) \
  -e GROUPID=$(id -g) \
  -e USER=rstudio \
  -p "$port:8787" \
  -v "$(pwd):/home/rstudio/evalR" \
  "$DOCKER_IMAGE_NAME" \
  $cmd

  #bash -c "source /etc/cont-init.d/userconf && exec /usr/lib/rstudio-server/bin/rserver --auth-none 1 --auth-timeout-minutes 0 --www-port 8787 --server-daemonize 0"

  # -v "$(pwd)/README.Rmd:/home/rstudio/README.Rmd" \
  # -v "$(pwd)/README.html:/home/rstudio/README.html" \
  # -v "$(pwd)/README.md:/home/rstudio/README.md" \
  # -v "$(pwd)/.Rprofile:/home/rstudio/.Rprofile" \
