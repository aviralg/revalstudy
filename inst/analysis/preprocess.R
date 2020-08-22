# Preprocess the dataset:
# - remove duplicates
# - add some columns (such as `eval_call_package`)
# - cut the dataset into smaller ones

# Datasets
# Input: raw.fst
# Outputs:
#   - summarized-core.fst
#   - summarized-packages.fst

source("insights.R")

deduplicate <- function(dataset)
{
  return(dataset %>% count(across(c(-eval_call_id, -starts_with("caller_stack"))), name = "nb_ev_calls"))
}

add_types <- function(dataset)
{
  return(dataset %>%
           resolve_sexp_name(expr_expression_type) %>%
           resolve_sexp_name(expr_resolved_type) %>%
           resolve_sexp_name(enclos_type) %>%
           resolve_sexp_name(envir_type))
}

add_parse_args <- function(dataset)
{
  return(dataset %>% mutate(parse_args = map(expr_parsed_expression, function(e) 
    { if(!is.na(e) && str_starts(e, "(parse|str2lang|str2expression)\\(")) 
        extract_args_parse(e) 
      else list() }))  %>% unnest_wider(parse_args))
}

eval_base_functions <- c("autoload", "autoloader", "bquote", "by.default", "by.data.frame", "invokeRestartInteractively", "Ops.data.frame", "dget", 
                         "eval", "eval.parent", "evalq", "local", "with.default", "within.data.frame", "within.list", "replicate", "subset.data.frame",
                         "subset.matrix", "transform.data.frame", "match.arg", "char.expand", "max.col", "parseNamespaceFile", "source", "sys.source",
                         "stopfinot", "as.data.frane.table", "match.fun", "trace", "untrace", ".doTrace")

# TODO: add support for package name extraction from caller_stack_expression for when 
# eval_call_srcref is NA
find_package_name <- function(caller_function, srcref, file)
{
  if(caller_function %in% eval_base_functions)
    return("base")
  else
    return(extract_package_name(srcref, file))
}

add_eval_source <- function(dataset)
{
  return(dataset %>% 
           mutate(eval_source = pmap_chr(list(caller_function, eval_call_srcref, file),
                                                         find_package_name)) )
}

add_eval_source_type <- function(dataset)
{
  return(dataset %>% 
           mutate(eval_source_type = case_when(eval_source %in% c("base", "core") ~ "core",
                                               eval_source == "base?"  ~ "<undefined>", 
                                               TRUE ~ "package" )))
}

