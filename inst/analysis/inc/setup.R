is_outlier_min <- function(x, m=1.5) quantile(x, 0.25) - m * IQR(x)

is_outlier_max <- function(x, m=1.5) quantile(x, 0.75) + m * IQR(x)

is_outlier <- function(x, m=1.5) {
  (x < is_outlier_min(x, m)) | (x > is_outlier_max(x, m))
}

show_url <- Vectorize(function(path, name=basename(path), hostname=params$hostname, port=params$port) {
  str_glue('<a href="http://{hostname}:{port}/{URLencode(path)}">{name}</a>')
}, vectorize.args = "path")

read_runr_log <- function(path) {
  log <- read_csv(
    path,
    col_types = cols(
      Seq = col_double(),
      Host = col_character(),
      Starttime = col_double(),
      JobRuntime = col_double(),
      Send = col_double(),
      Receive = col_double(),
      Exitval = col_double(),
      Signal = col_double(),
      Command = col_character(),
      V1 = col_character(),
      Stdout = col_character(),
      Stderr = col_logical()
    )
  ) %>%
    rename_all(tolower) %>%
    mutate(
      package=basename(v1),
      run_path=path(dirname(path), package),
      output_file=path(run_path, "task-output.txt")
    ) %>%
    select(
      package,
      run_path
    )
  
  task_status <- map2_dfr(log$package, log$run_path, function(package, run_path) {
    tryCatch({
      read_csv(
        path(run_path, "task-stats.csv"), 
        col_types=cols(
          exitval=col_character(),
          hostname=col_character(),
          start_time=col_integer(),
          end_time=col_integer(),
          command=col_character()
        )
      ) %>%
      add_column(package, .before=1)
    }, error=function(e) tibble(package, start_time=NA, end_time=NA, error=e$message))
  })
  
  left_join(log, task_status, by="package") %>%
    mutate(
      start_time=as_datetime(start_time),
      end_time=as_datetime(end_time),
      elapsed=end_time-start_time
    ) %>%
    select(-hostname)
}
