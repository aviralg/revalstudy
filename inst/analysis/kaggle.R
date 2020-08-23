#!/usr/bin/env Rscript

for (x in c(
  "dplyr", 
  "DT", 
  "fs",
  "fst",
  "knitr",
  "purrr",
  "pbapply",
  "readr",
  "rjson",
  "rmarkdown",
  "runr",
  "stringr",
  "tibble"
)) {
  suppressPackageStartupMessages(library(x, character.only=TRUE))
}

pboptions(type="txt")

# simulate from Rmd
params <- list(
  base_dir=commandArgs(trailingOnly=TRUE)[1],
  competitions=c("titanic")
)

source("inc/paths.R")

KAGGLE_KERNELS_DIR  <- path(RUN_DIR, "kaggle-kernels", "notebooks", "r", "kernels")
KAGGLE_DATASETS_DIR <- path(RUN_DIR, "kaggle-datasets")
KAGGLE_RUN_DIR      <- path(RUN_DIR, "kaggle-run")

KERNELS_FILE      <- path(KAGGLE_RUN_DIR, "kernels.csv")
SCRIPTS_FILE      <- path(KAGGLE_RUN_DIR, "scripts.txt")
PARALLEL_LOG_FILE <- path(KAGGLE_RUN_DIR, "parallel.log")

cat("- kaggle kernels:", KAGGLE_KERNELS_DIR, "\n")
cat("- kaggle datasets:", KAGGLE_DATASETS_DIR, "\n")

stopifnot(dir_exists(KAGGLE_KERNELS_DIR))
stopifnot(dir_exists(path(KAGGLE_DATASETS_DIR, params$competitions)))

wrap <- function(file, body) {
  str_glue(
    "evil::write_eval_traces(",
    "  evil::trace_eval(",
    "    quote = TRUE,",
    "    code = {{",
    "      {gsub('\n', '\n      ', body)}",
    "    }}",
    "  ),",
    "  datadir=getwd()",
    ")",
    .sep = "\n"
  )
}

extract_kernel_code <- function(path, code_file, competition) {
  run_path <- path(KAGGLE_RUN_DIR, competition, basename(path))
  code_file <- path(path, "script", code_file)
  run_code_file <- path(run_path, "run.R")
  
  if (!dir_exists(run_path)) dir_create(run_path)
  
  if (str_ends(code_file, "\\.[rR]$")) {
    file_copy(code_file, run_code_file, overwrite=TRUE)
  } else if (str_ends(code_file, "\\.Rmd")) {
    knitr::purl(code_file, run_code_file, quiet=TRUE)
  } else if (str_ends(code_file, "\\.irnb") || str_ends(code_file, "\\.ipynb")) {
    tmp <- tempfile(fileext = "Rmd")
    rmarkdown:::convert_ipynb(code_file, tmp)
    knitr::purl(tmp, run_code_file, quiet=TRUE)
  }
  
  if (file_exists(run_code_file)) {
    tmp <- read_file(run_code_file)
    tmp <- wrap(run_code_file, tmp)
    write_file(tmp, run_code_file)
  }
  
  run_code_file
}

files <- list.files(KAGGLE_KERNELS_DIR, "kernel-metadata\\.json$", full.names=TRUE, recursive=TRUE)

metadata_json <- map(files, ~fromJSON(file = .))
metadata <- map_dfr(
  metadata_json, 
  ~tibble(id=str_replace(.$id, "/", "-"), language=.$language, kernel_type=.$kernel_type, competition=.$competition_sources, code_file=.$code_file)
)

kernels <- 
  metadata %>%
  mutate(
    path=path(KAGGLE_KERNELS_DIR, id)
  ) %>% 
  filter(dir_exists(path))

known_competitions <- semi_join(kernels, tibble(competition=params$competitions), by="competition")

if (dir_exists(KAGGLE_RUN_DIR)) {
  warning("*** ", KAGGLE_RUN_DIR, " exists")
}

extraction_lst <- pbapply::pbapply(known_competitions, 1, cl=16, function(x) {
  tryCatch({
    tibble(id=x["id"], run_file=extract_kernel_code(x["path"], x["code_file"], x["competition"]))
  }, error=function(e) {
    tibble(id=x["id"], error=e$message)
  })
})
extraction <- bind_rows(extraction_lst)

kernels_supported <- 
  known_competitions %>%
  left_join(extraction, by="id") %>%
  filter(file_exists(run_file))

sloc <- map_dfr(path(KAGGLE_RUN_DIR, params$competitions), function(x) {
  out <- system2("cloc", c("--include-lang=R", "--by-file-by-lang", "-q", "--csv", shQuote(x)), stdout = TRUE)  
  out_r <- out[startsWith(out, "R,")]
  out_csv <- c("language,filename,blank,comment,code", out_r)
  df <- read_csv(out_csv)
}) %>%
  mutate(
    kernel=basename(dirname(filename))
  ) %>%
  select(kernel, code)

stopifnot(any(!duplicated(sloc$kernel)))

kernels_supported <- 
  kernels_supported %>%
  left_join(sloc, by=c("id"="kernel")) %>%
  mutate(runnable=ifelse(is.na(code), FALSE, code > 0))

kernels_runnable <- filter(kernels_supported, runnable)
write_csv(kernels_supported, KERNELS_FILE)
write_lines(dirname(kernels_runnable$run_file), SCRIPTS_FILE)

print(count(kernels_supported, runnable))