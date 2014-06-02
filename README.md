# Test
[Testthat](https://github.com/hadley/testthat) is used for testing. You may 
install testthat by downloading the 
[testthat package from CRAN](http://cran.r-project.org/web/packages/testthat/index.html) 
and installing the package by running ```R CMD INSTALL ~/Downloads/testthat_X.X.X.tgz```,
replacing the path for the location of the downloaded package.

Tests are executed by loading R and executing the following command from
the R command line interface:

```R
test_dir(".");
```
