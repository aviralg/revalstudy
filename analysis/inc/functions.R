is_outlier_min <- function(x, m=1.5) quantile(x, 0.25) - m * IQR(x)

is_outlier_max <- function(x, m=1.5) quantile(x, 0.75) + m * IQR(x)

is_outlier <- function(x, m=1.5) {
  (x < is_outlier_min(x, m)) | (x > is_outlier_max(x, m))
}

show_url <- Vectorize(function(path, name=basename(path), base_url=params$http_base_url) {
  str_glue('<a target="_blank" href="{base_url}{URLencode(path)}">{name}</a>')
}, vectorize.args=c("path", "name"))

read_parallel_task_result <- function(path) {
  file <- if (dir.exists(path)) file.path(path, "parallel.csv") else path

  read_csv(file) %>%
    select(-Seq, -Send, -Receive, -Signal, -Stdout, -Stderr) %>%
    rename_all(tolower) %>%
    rename(hostname=host) %>%
    mutate(
      hostname=map_chr(str_split(hostname, " "), ~tail(., 1)),
      starttime=lubridate::as_datetime(starttime),
      jobruntime=lubridate::as.duration(jobruntime),
      exitval=as.integer(exitval),
      result=classify_exitval(exitval)
    )
}

classify_exitval <- function(x) {
  case_when(
    x == 0L   ~ "Success",
    x == 1L   ~ "Client code exception",
    x == 2L   ~ "Client code exception",
    x == 124L ~ "Timeout",
    x == 139L ~ "Segfault",
    x > 127L  ~ str_c("Failure signal: ", x - 127L),
    TRUE      ~ str_c("Failure: ", x)
  )
}

cs_count <- function(df, ...) {
  cnt <- count(df, ..., sort=TRUE, name="n")
  N <- sum(cnt$n)
  mutate(cnt, p=n/N*100, cp=cumsum(p))
}

cs_print <- function(df, cp=90) {
  stopifnot("cp" %in% colnames(df))
  .N <- cp
  print(filter(df, cp<=.N), n=Inf)
}

gen_provenance_class <- function(dataset) {
  dataset %>%
    mutate(provenance_class = case_when(
      !is.na(expr_parsed_expression) ~ "string",
      !is.na(expr_match_call) ~ "reflection",
      expr_resolved_type_tag == "language" ~ "constructed",
      expr_resolved_type_tag == "expression" ~ "constructed",
      TRUE ~ NA_character_
    ))
}


