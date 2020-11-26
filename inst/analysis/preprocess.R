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

library(fs)
#library(future.apply)
#plan(multiprocess) # multicore (fork) with fallback to multisession (create processes and then copy)

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
                         "stopifnot", "as.data.frame.table", "match.fun", "trace", "untrace", ".doTrace", "Vectorize")


find_package_name <- function(caller_function, caller_package, caller_expression, srcref, file)
{
  if(caller_function %in% eval_base_functions)
    return("core") #"Actually base but we generalize
  else
  {
    tempPack <- extract_package_name(srcref, file)
    if(tempPack == "base?") # It means the srcref was NA
    {
       if(caller_package == "foreach" && caller_expression == "e$fun(obj, substitute(ex), parent.frame(), e$data)")
       {
         # This eval is defined in function doSEQ in do.R of package foreach
         return("foreach")
       }
      else
      {
        return("base?")
      }
    }
    else
      return(tempPack)
  }
}

find_package_name_second_chance <- function(file, caller_stack_expr, caller_stack_expr_srcref)
{
  new_srcref <- package_name_from_call_stack(caller_stack_expr, caller_stack_expr_srcref)
  return(extract_package_name(new_srcref, file))
}

add_eval_source <- function(dataset, dataset_with_stacks)
{ 
  dataset_c <- dataset %>% 
           mutate(eval_source = pmap_chr(list(caller_function, caller_package, caller_expression, eval_call_srcref, file),
                                                         find_package_name)) 
  
  # dataset_stacks <- dataset_c %>% filter(eval_source == "base?") %>% 
  #     left_join(select(dataset_with_stacks, -ends_with("_type"))) %>% 
  #     select(-eval_call_id, -caller_stack_expression_raw) %>% 
  #     distinct()
  # dataset_stacks <- dataset_stacks %>% 
  #   distinct(file, caller_stack_expression, caller_stack_expression_srcref) %>% 
  #   mutate(eval_source = pmap_chr(list(file, caller_stack_expression, caller_stack_expression_srcref),
  #                                 find_package_name_second_chance)) %>% 
  #   select(-starts_with("caller_stack_")) %>%
  #   distinct() # There might be a problem with duplicated rows with same nb_ev_calls?
  
  #dataset_c <- bind_rows(dataset_c %>% filter(eval_source != "base?"), dataset_stacks)
    
  return(dataset_c)
}

add_eval_source_type <- function(dataset)
{
  return(dataset %>% 
           mutate(eval_source_type = case_when(eval_source %in% c("base", "core") ~ "core",
                                               eval_source == "base?"  ~ "<undefined>", 
                                               TRUE ~ "package" )))
}

add_fake_srcref <- function(dataset)
{
  return(dataset %>%
           mutate(eval_call_srcref = if_else(is.na(eval_call_srcref) & eval_source_type != "<undefined>", 
                                             str_c(eval_source, caller_function, eval_call_expression, sep="::"), eval_call_srcref)))
}


add_ast_size <- function(dataset)
{
  print("Creating cluster")
  cluster <- new_cluster(parallel::detectCores() - 10)
  print("Cluster created. Copying functiond and libraries.")
  cluster_copy(cluster, "get_expr")
  cluster_copy(cluster, "expr_size")
  cluster_copy(cluster, "expr_size_str")
  cluster_library(cluster, "tidyverse")
  print("Functions and libraries copied. Partitionning.")
  dataset_c <- dataset %>% select(expr_resolved) %>% distinct() %>% group_by(expr_resolved) %>% partition(cluster)
  print("Partionned. Computing.")
  dataset_c <- dataset_c %>% 
           mutate(expr_resolved_ast_size = map_int(expr_resolved, expr_size_str)) %>%
           collect()
  print("Finished computing. Left joining.")
  dataset <- dataset %>% left_join(dataset_c)
  print("Finished.")
  return(dataset)
}

add_package <- function(dataset)
{
  mutate(
    dataset, 
    package = basename(dirname(dirname(file)))
  )
}



keep_only_corpus <- function(dataset, corpus_files)
{
  return(dataset %>% 
           filter(eval_source %in% c(corpus_files, "core", "base", "base?")))
}

get_externals <- function(dataset, corpus_files)
{
  return(dataset %>% 
           filter(!eval_source %in% c(corpus_files, "core", "base", "base?")))
}

