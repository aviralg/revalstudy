library(rlang)


nb_eval_call_sites <- function(eval_calls)
{
  # Two cases: with srcref, without srcref
  
  nb_with_srcref <- eval_calls %>% select(eval_call_srcref) %>% n_distinct(na.rm = TRUE)
  nb_without_srcref <- eval_calls %>% filter(is.na(eval_call_srcref)) %>% select(caller_package, caller_function) %>% n_distinct(na.rm = TRUE)
  
  return(nb_with_srcref + nb_without_srcref)
}


get_expr <- function(eval_call)
{
  # Special case for expressions starting with _ (such as _inherit in ggproto)
  escaped_eval_call <- if(startsWith(eval_call, "_"))
  {
    paste0("`", eval_call, "`")
  }
  else
  {
    eval_call
  }
  
  exp <- NA
  # Would fail for instance for 
  # "`$<-`(new(\"C++Field\", .xData = <environment>), \"read_only\", TRUE)" (classInt package)
  # exp <- tryCatch(
  #   parse(text = escaped_eval_call)[[1]],
  #   error = function(e) {return(NA)})
  try(exp <- parse(text = escaped_eval_call)[[1]], silent = TRUE)

  
  return(exp)
}

function_name <- function(eval_call)
{
  exp <- get_expr(eval_call)
  if(is.call(exp))
  {
    return(paste(deparse(exp[[1]]), collapse = "\n"))
  }
  return(NA)
}


function_arguments <- function(eval_call)
{
  exp <- get_expr(eval_call)
  if(is.call(exp))
  {
    return(map_chr(as.list(exp[-1]), function(chr) { paste(deparse(chr), collapse = "\n")}))
  }
  return(NA)
}

# placeholder for the `parse_only` function of library xfun
parse_only <- function(code) {}
.myparse <- function(text) {}
parse_all <- function(x, filename,allow_error) {}

extract_args_parse <- function(eval_call) 
{
  exp <- get_expr(eval_call)
  exp <- call_standardise(exp)
  args <- map_chr(as.list(exp[-1]), function(chr) { paste(deparse(chr), collapse = "\n")})
  
  return(args[str_detect(names(args), "file|text|n|s|keep.source|srcfile")])# "file|text|n|s|prompt|keep.source|srcfile|code"
}

# See extract_inner_exp which takes a str
# This one takes an expression
# We assume that eval_expr_call is a call
extract_inner_exp_aux <- function(eval_expr_call)
{
  args <- as.list(eval_expr_call[-1])
  leaves <- list()
  no_calls <- TRUE
  for(arg in args)
  {
    if(!missing(arg)) # we cannot have the two conditions in the same if
    {
      if(is.call(arg))
      {
        leaves <-  c(leaves, extract_inner_exp_aux(arg))
        no_calls <- FALSE
      }
    }
  }
  if(no_calls)
  {
    return(c(leaves, deparse(eval_expr_call)))
  }
  return(leaves)
}

# Extract the inner calls and arguments
# For instance, in f(g(h(e))), we would get h(e)
# For f(g(e), t), we will get g(e)
extract_inner_exp <- function(eval_call)
{
  #cat(eval_call, "\n")
  exp <- get_expr(eval_call)
  if(is.call(exp))
  {
    return(extract_inner_exp_aux(exp))
  }
  return(NA)
}

constant_leaves_expr <- function(eval_exp_call)
{
  if(is.call(eval_exp_call))
  {
    args <- as.list(eval_exp_call[-1])
    return(every(args, constant_leaves_expr))
  }
  else
  {
    return(!is.language(eval_exp_call))
  }
}



# Test if all the expression in the call graphs are not symbols but constant
# It would imply that there's no need for the eval!
constant_leaves <- function(eval_call)
{
  if(is.na(eval_call))
  {
    return(FALSE)
  }
  exp <- get_expr(eval_call)
  return(constant_leaves_expr(exp))
}

# Check if there is only one expression and that it is a call
check_call <- function(arg)
{
  exp <- parse(text = arg)
  return(length(exp) == 1 & is.call(exp[[1]]))
}


# Transforms a call in a character string in eval(parse(text = str)) into a do.call 
text_call_to_do_call <- function(eval_call)
{
  exp <- get_expr(eval_call)
  stopifnot(exp[[1]] == "eval")
  args <- exp[-1]
  stopifnot(is.call(args[[1]]) & args[[1]][[1]] == "parse")
  text <- args[[1]]$text
  
  # TODO: handle environment

  f_name <- function_name(text)
  f_args <- function_arguments(text)
  return(paste0("do.call(", f_name, ", list(", paste0(f_args, collapse = ","), "))"))
}

SEXP_TYPES <- tribble(~sexp_type, ~name,
                      0, "NILSXP",
                      1, "SYMSXP",
                      2, "LISTSXP",
                      3, "CLOSXP",
                      4, "ENVSXP",
                      5, "PROMSXP",
                      6, "LANGSXP",
                      7, "SPECIALSXP",
                      8, "BUILTINSXP",
                      9, "CHARSXP",
                      10, "LGLSXP",
                      13, "INTSXP",
                      14, "REALSXP",
                      15, "CPLXSXP",
                      16, "STRSXP",
                      17, "DOTSXP",
                      18, "ANYSXP",
                      19, "VECSXP",
                      20, "EXPRSXP",
                      21, "BCODESXP",
                      22, "EXTPTRSXP",
                      23, "WEAKREFSXP",
                      24, "RAWSXP",
                      25, "S4SXP",
                      30, "NEWSXP",
                      31, "FREESXP",
                      99, "FUNSXP"
)


SEXP_TYPES <- SEXP_TYPES %>% mutate(name = factor(name))

resolve_sexp_name <- function(df, var) {
  en_var <- enquo(var)
  by <- "sexp_type"
  names(by) <- as.character(substitute(var))
  df %>%
    left_join(SEXP_TYPES, by=by) %>%
    select(-!!en_var) %>%
    rename(!!en_var:=name)
}


extract_package_name <- function(src_ref, file)
{
  # There are 5 possibilities for a srcref:
  # - NA
  # - /tmp/Rtmp..../R.INSTALL....../packagename/R/file:linenumbers
  # - /mnt/nvme0/R/project-evalR/library/4.0/instrumentr/srcref/packagename/4.0.4/file:linenumbers
  # - /R/* : core packages (we cannot distinguish between them yet so we write core for the package name)
  # - /testit/... or /testthat/... : it is the testit or testthat packages
  # - :/R : extract the package name from the file path in the column path
  case_when(
    is.na(src_ref) ~ "base",
    str_starts(src_ref, fixed("./R/")) ~ "core",
    str_starts(src_ref, fixed("/tmp/")) ~ str_match(src_ref, "/tmp/Rtmp[^/]*/R\\.INSTALL[^/]*/([^/]+)/.*")[[2]],
    str_starts(src_ref, fixed("/mnt/nvme0/")) ~ str_match(src_ref, "/mnt/nvme0/R/project-evalR/library/4.0/instrumentr/srcref/([^/]*)/.*")[[2]],
    str_starts(src_ref, fixed("test")) ~ str_match(src_ref, "([^/]*)/.*")[[2]],
    str_starts(src_ref, ":/") ~ str_match(file, "[^/]*/[^/]*/([^/]*)/.*")[[2]],
    TRUE ~ "unknown"
  )
}


groupify_function <- function(expr_function)
{
  case_when(
    str_starts(expr_function, fixed("(function(")) ~ "anonymous",
    str_starts(expr_function, fixed(".Primitive(")) ~ "primitive",
    TRUE ~ expr_function
  )
}


