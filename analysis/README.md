# Generating the reports for the notebooks

You can easily generate a report for each dataset, *kaggle*, *base* and *package* (CRAN) with the following `make` target:

```bash
make dir/dataset-notebook.ext
```

This will generate `dataset-notebook.ext`in directory `dir` fron notebook `notebook.Rmd`.


For instance:

```bash
make html/package-normalized.html
```

It will generate `package-normalized.html`in `html` directory from the `normalized.Rmd` notebook. 

The only constraint on the notebook is to have a parameter `base_dir` and expecting it to point to one of the dataset. It will be equal to `Data/kaggle/`, `Data/base/`or `Data/package/`.

You can also generate all the reports for all the datasets for one given notebook, as follows:

```bash
make dir/notebook.ext
```