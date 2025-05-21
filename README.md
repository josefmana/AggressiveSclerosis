# AggressiveSclerosis
A package that gathers code for Tomas Uher's project for better understanding of a (rare) aggressive variant of Multiple Sclerosis

## Using the package

This package is not intended for public use. The idea is to clone the repository and
work on its content. 

```r
# clone the repository, open it in R
# install missing dependencies, if any
devtools::install_deps() 

# main pipeline should be used via targets
targets::tar_make()
```

Maybe in future, we publish the code. We do not plan to publish it as a package,
rather as `_targets.R`, `R/` and maybe `reports/` or later `manuscript/` folders.

## Importing data

If you have the raw data Excel file, save it in `data-raw` as `some_data_name.xlsx`.
Then run the following code to classify patient into "aggressive disease" category:
```r
p <- here::here("data-raw", "some_data_name.xlsx")
d <- prepare_data(p)
data <- determine_aggressive_phenotype(d$id, d$relapses, d$edss, T)
```
