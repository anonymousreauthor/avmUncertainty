
# Initial Base Image
FROM rocker/r-ver:3.6.0

# Maintainer Signature
LABEL maintainer="anonymousREauthor"

# Linux setup (non-R libraries)
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core \
	libcurl4-openssl-dev \
	libssh2-1-dev \
	libssl-dev \
	make \
	pandoc \
	pandoc-citeproc \
	unixodbc-dev \
	zlib1g-dev

# Install R dependencies (CRAN-available) found with containerit
RUN ["install2.r", "assertthat", "backports", "bit", "bit64", "blob", "boot", "callr", "class", "classInt", "cli", "coda", "codetools", "crayon", "curl", "DBI", "deldir" ,"DEoptimR", "desc", "devtools", "digest", "doSNOW", "dplyr"]

RUN ["install2.r", "e1071", "expm", "foreach", "formatR", "fs", "futile.logger", "futile.options", "gdata", "glue", "gmodels", "gtools", "git2r", "glue", "hms", "httr", "iterators", "KernSmooth", "knitr", "lambda.r", "lattice"]

RUN ["install2.r", "LearnBayes", "magrittr", "MASS", "Matrix", "memoise", "nlme", "odbc", "packrat", "pillar", "pkgbuild", "pkgconfig", "pkgload", "prettyunits", "processx", "ps", "purrr", "R6", "Rcpp", "randomForest", "ranger"]

RUN ["install2.r", "RANN", "rcpp", "remotes", "rjson", "rlang", "robustbase", "RPresto", "rprojroot", "rstudioapi", "semver", "sessioninfo", "sf", "snow", "sp", "spData", "spgwr", "stevedore", "stringi", "stringr"]

RUN ["install2.r", "testthat", "tibble", "tidyr", "tidyselect", "units", "usethis", "withr", "xfun", "yaml", "RODBC"]

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y \
	libcurl4-openssl-dev \
	qpdf \
	wget \
	git \
	libgdal-dev \
	libgeos-dev \
	libproj-dev \
	liblwgeom-dev \
	libudunits2-dev \
	postgis

RUN ["install2.r", "covr", "raster", "sf"]

RUN ["install2.r", "lwgeom"]

RUN ["install2.r", "spdep"]

RUN ["install2.r", "grf"]

RUN ["install2.r", "hpiR"]

RUN ["install2.r", "tidyverse"]

RUN ["install2.r", "tigris"]

# Create a directory structure
RUN mkdir /packages
RUN mkdir /code
RUN mkdir /data
RUN mkdir /output
RUN mkdir /cfgs

# Load and install custom, local packages
COPY docker/avmUncertainty_0.1.1.tar.gz /packages/avmUncertainty.tar.gz

RUN R -e "install.packages('/packages/avmUncertainty.tar.gz', repos = NULL, type = 'source')"

RUN R -e "remotes::install_github('anonymousreauthor/kingCoData')"

# Copy data, scripts and configs
COPY /scripts/sandbox.R /code/test.R
COPY /scripts/1_run_experiments.R /code/1_run_experiments.R

# Run test script
RUN Rscript /code/test.R
