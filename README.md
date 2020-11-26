# Eval In R

Template obtained from [here](https://www.ieee.org/conferences/publishing/templates.html).

## Setup

To build the paper, run the following command:

```sh
make
```

To clean build artifacts, run the following command:

```r
make clean
```


### Enrich
- we don't know what are the inputs to enrich
- split into multiple stages to save time
  - impute_srcrefs.R 

- $(RUN_CALLS_FILE) merges all calls files
  $(RUN_PROGRAM_FILE) merges all programs files
    - second line of the rule repeats the step of corpus-stage3 because it subsets on the corpus obtained from corpus-stage2
- there should not be a corpus-stage3 and all rules should be in makefile

program.fst calls.fst

- make trace-packages
  - make merge-package-data

- make trace-kaggle
  - make merge-kaggle-data


make trace:
   make trace-packages
   mkae trace-kaggle
   
make subset: # run after make trace
  subset wrt corpus from stage2
  
make summarize:
   - fixes srcrefs
   - splits datasets into base/core packages
   - outputs summarized data

   
