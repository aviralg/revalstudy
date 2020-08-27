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
  if(is.na(eval_call))
  {
    return(NA)
  }
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
  exp <- tryCatch(call_standardise(exp),
                  error = function(c) 
                    if(length(exp) >= 2 && exp[[2]] == "...") { names(exp)[[2]] <- "dots" ; return(exp)}
                    else stop(paste0("extract_arg failed with: ", eval_call))
  )
  args <- map_chr(as.list(exp[-1]), function(chr) { paste(deparse(chr), collapse = "\n")})
  names(args) <- map_chr(names(args), function(chr) paste0("parse_args_", chr))
  
  return(args[str_detect(names(args), "^parse_args_(file|text|n|s|keep\\.source|srcfile|dots)$")])# "file|text|n|s|prompt|keep.source|srcfile|code"
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

R_LIB_SRC <- "/var/lib/R/project-evalR/R-4.0.2/src/library"
CORE_PACKAGES <- c(
  "compiler",
  "graphics",
  "grDevices",
  "grid",
  "methods",
  "parallel",
  "profile",
  "splines",
  "stats",
  "stats4",
  "tcltk",
  "tools",
  "utils"
)
core_package_files <- map_dfr(CORE_PACKAGES, function(x) {
  p <- file.path(R_LIB_SRC, x, "R")
  f <- list.files(p, pattern="\\.R$", recursive=FALSE)
  f <- f[!str_ends(f, "all\\.R")]
  f <- file.path("./R", f)
  tibble(package=x, file=f)
})

# match eval that are not called (but passed to a higher order function)
is_eval <- function(s)
{
  return(!is.na(s) && str_detect(s, "(eval(q|\\.parent)?|local)[^\\(_\\.q]"))
}

# To use if srcref is NA and caller_function is not one of the base ones
# Will not always work but should work for lapply ones
package_name_from_call_stack <- function(caller_stack_expr, caller_stack_expr_srcref)
{
  stack_expr <- str_split(caller_stack_expr, fixed("\n"))
  stack_srcref <- str_split(caller_stack_expr_srcref, fixed("\n"))
  
  eval_pos <- detect_index(stack_expr[[1]], is_eval)
  if(eval_pos != 0)
  {
    srcref <- stack_srcref[[1]][[eval_pos]]
    return(if(srcref == "NA") "base?" else srcref)
  }
  
  return("base?")
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
  # - .../kaggle-run/<id>/run.R:...
  case_when(
    is.na(src_ref) ~ "base?",
    str_starts(src_ref, fixed("./R/")) ~ "core",
    str_starts(src_ref, fixed("/tmp/")) ~ str_match(src_ref, "/tmp/Rtmp[^/]*/R\\.INSTALL[^/]*/([^/]+)/.*")[[2]],
    str_starts(src_ref, fixed("/mnt/nvme0/")) ~ "base",
    str_starts(src_ref, fixed("test")) ~ str_match(src_ref, "([^/]*)/.*")[[2]],
    str_starts(src_ref, fixed("/:")) ~ str_match(file, "[^/]*/[^/]*/([^/]*)/.*")[[2]],
    TRUE ~ "unknown"
  )
}

replaceable_functions <- c("+", "*", "/", "-", "%%", 
                           "<-", "[[<-", "[<-", "$<-", "<<-", "=",
                           "[", "[[", "$",
                           "@<-",
                           "slot", "@",
                           "&", "&&", "|", "||", "!",
                           "<", ">", "==", "<-", ">=", "!=",
                           "if")


is_replaceable <- function(expr)
{
  # We are going to traverse the ast recursively
  # We cannot replace in 2 cases: the function name in a call is too complex (we could do a do.call though), it is bytecode
  if(is.call(expr))
  {
    function_name <- expr[[1]]
    function_args <- expr[-1]
    return(as.character(function_name) %in% replaceable_functions && every(function_args, is_replaceable))
    
  }
  else if(is.expression(expr))
  {
    return(every(expr, is_replaceable))
  }
  else if(typeof(expr) == "bytecode")
  {
    return(FALSE)
  }
  else # Symbols, promises, vector types, closure and builtins, S4 objects. There should not be "..." here
  {
    return(TRUE)
  }
}



is_replaceable_str <- function(expr)
{
  e <- get_expr(expr)
  return(is_replaceable(e))
}

expr_depth <- function(expr) 
{
  if(is.call(expr))
  {
    return(1L + max(map_int(expr[-1], expr_depth)))
  }
  else if(is.expression(expr))
  {
    return(max(map_int(expr, expr_depth)))
  }
  else
  {
    return(1L)
  }
}

expr_depth_str <- function(expr)
{
  e <- get_expr(expr)
  return(expr_depth(e))
}

expr_size <- function(expr)
{
  if(is.call(expr))
  {
    return(1L + sum(map_int(expr[-1], expr_size)))
  }
  else if(is.expression(expr))
  {
    return(sum(map_int(expr, expr_size)))
  }
  else
  {
    return(1L)
  }
}

expr_size_str <- function(expr)
{
  e <- get_expr(expr)
  return(expr_size(e))
}

groupify_function <- function(expr_function)
{
  case_when(
    str_starts(expr_function, fixed("(function(")) ~ "anonymous",
    str_starts(expr_function, fixed(".Primitive(")) ~ "primitive",
    TRUE ~ expr_function
  )
}

extract_write_envir <- function(env_class)
{
  return(str_split(env_class, fixed("+"), n = 2)[[1]])
}


extract_envir <- function(env_class, envir_type, envir_expression)
{
  # First element is write, second is read
  case_when(
    is.na(env_class) && envir_type == "VECSXP" ~ c("list", "enclos"),
    is.na(env_class) && envir_type == "NILSXP" ~ c("NULL", "enclos"),
    is.na(env_class) && envir_type == "INTSXP" ~ {e <- paste0("sys.call(", envir_expression, ")"); c(e, e)},
    str_ends(env_class, fixed("global")) ~ c(extract_write_envir(env_class), "global"),
    str_ends(env_class, fixed("base")) ~ c(extract_write_envir(env_class), "base"),
    str_ends(env_class, fixed("callee")) ~ c(extract_write_envir(env_class), "callee"),
    str_ends(env_class, fixed("empty")) ~ c(extract_write_envir(env_class), "empty"),
    str_ends(env_class, "caller-.*") ~ c(extract_write_envir(env_class), str_extract(env_class, "caller-[0-9]*")), 
    str_ends(env_class, "loop") ~ c("loop", "loop"),
    str_ends(env_class, "package:.*") ~ c(extract_write_envir(env_class), str_extract(env_class, "package:.*"))
  )
}

