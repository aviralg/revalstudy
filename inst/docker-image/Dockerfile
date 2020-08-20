FROM rocker/rstudio:4.0.2

# common devel dependencies
RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update -yqq && \
    apt-get install -yqq \
      aptitude \
      build-essential \
      cloc \
      curl \
      default-jdk \
      libbz2-dev \
      libcairo2-dev \
      liblzma-dev \
      libpcre2-dev \
      libpcre3-dev \
      libreadline-dev \
      libxml2-dev \
      rsync \
      tree \
      vim \
      x11-utils \
      xorg-dev \
      xvfb \
      zlib1g-dev

ADD system-requirements.txt .
RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get -yqq remove libcurl4-openssl-dev && \
    cat system-requirements.txt | grep "^apt-get" | sed 's/^apt-get install -y \(.*\)/\1/' | sort | uniq | xargs aptitude install -y

WORKDIR /R

# bootstrap scripts
ARG RUNR_GIT_BRANCH="project-evalR"
ARG CRAN_MIRROR="cran.r-project.org"

env R_VERSION=4.0.2 \
    R_PROJECT_BASE_DIR=/R

env R_DIR="$R_PROJECT_BASE_DIR/R-$R_VERSION" \
    RDT_DIR="$R_PROJECT_BASE_DIR/R-dyntrace" \
    R_LIBS="$R_PROJECT_BASE_DIR/library/$R_VERSION" \
    PACKAGES_SRC_DIR="/$R_PROJECT_BASE_DIR/CRAN/extracted" \
    PACKAGES_ZIP_DIR="/$R_PROJECT_BASE_DIR/CRAN/src/contrib" \
    CRAN_MIRROR_LOCAL_DIR="$R_PROJECT_BASE_DIR/CRAN" \
    R_KEEP_PKG_SOURCE=1 \
    R_ENABLE_JIT=0 \
    R_COMPILE_PKGS=0 \
    R_DISABLE_BYTECODE=1 \
    OMP_NUM_THREADS=1 \
    RUNR_GITHUB_URL="https://raw.githubusercontent.com/PRL-PRG/runr/$RUNR_GIT_BRANCH"

env CRAN_MIRROR_LOCAL_URL="file://$CRAN_MIRROR_LOCAL_DIR"

# R
RUN curl "$RUNR_GITHUB_URL/inst/install-r.sh" | bash -s -- -d "$R_DIR" -v "$R_VERSION"

env PATH=$R_DIR/bin:$PATH

# CRAN packages
RUN mkdir -p "$PACKAGES_ZIP_DIR" && \
    rsync \
      -rtlzv \
      --delete \
      --include='*.tar.gz' \
      --include='PACKAGES*' \
      --exclude='*/*' \
      "${CRAN_MIRROR}::CRAN/src/contrib/" "$PACKAGES_ZIP_DIR"

RUN mkdir -p "$R_LIBS" && \
    mkdir -p "$PACKAGES_ZIP_DIR"

RUN nohup Xvfb :6 -screen 0 1280x1024x24 >/dev/null 2>&1 & \
    export DISPLAY=:6 && \
    R --slave -e \
      "source('$RUNR_GITHUB_URL/inst/install-cran-packages.R'); \
       install_cran_packages(mirror='$CRAN_MIRROR_LOCAL_URL', lib='$R_LIBS');" \
      2>&1 | tee "$R_LIBS/install.log"

RUN mkdir -p "$PACKAGES_SRC_DIR" && \
    ls -1 "$PACKAGES_ZIP_DIR"/*.tar.gz | xargs -L 1 tar -C "$PACKAGES_SRC_DIR" -xzf

# ## The following are dependencies that are too outdated in the package repository

# GNU parallel
RUN mkdir parallel && \
    cd parallel && \
    curl http://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2 | tar -xjf- --strip 1 && \
    ./configure && \
    make install && \
    mkdir /root/.parallel && \
    touch /root/.parallel/will-cite && \
    mkdir /home/rstudio/.parallel && \
    touch /home/rstudio/.parallel/will-cite

# tooling

ARG INJECTR_VERSION=HEAD
RUN git clone https://github.com/PRL-PRG/injectr && \
    cd injectr && \
    git checkout $INJECTR_VERSION && \
    R CMD INSTALL .

ARG INSTRUMENTR_VERSION=HEAD
RUN git clone https://github.com/PRL-PRG/instrumentr && \
    cd instrumentr && \
    git checkout $INSTRUMENTR_VERSION && \
    R CMD INSTALL .

ARG EVIL_VERSION=HEAD
RUN git clone https://github.com/PRL-PRG/evil && \
    cd evil && \
    git checkout $EVIL_VERSION && \
    R CMD INSTALL .

ARG RUNR_VERSION=HEAD
RUN git clone https://github.com/PRL-PRG/runr && \
    cd runr && \
    git checkout $RUNR_VERSION && \
    R CMD INSTALL .

WORKDIR /

ADD Rprofile.site $R_DIR/etc
# # configure entrypoint
# ADD entrypoint.sh /
# RUN chmod +x /entrypoint.sh
# ENTRYPOINT ["/entrypoint.sh"]
