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
rather as `_target.R`, `R/` and maybe `reports/` or later `manuscript/` folders.

```
