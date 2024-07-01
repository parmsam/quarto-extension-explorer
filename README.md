
# quarto-extension-explorer

<!-- badges: start -->
<!-- badges: end -->

The goal of quartoextexp is to provide a pretty simple Shiny app to explore the extensions available from the official [Quarto Extension Listing](https://quarto.org/docs/extensions/) and easily add one or more of them into a Quarto project.

## Installation

You can install the development version of quartoextexp from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("parmsam/quarto-extension-explorer")
```

## Example

This is a basic example which shows you how to run the app (pointing to your working directory). Note that you should point it to your Quarto project directory not your `_extensions` folder.

``` r
library(quartoextexp)
## basic example code
quartoextexp::run_app()
```

You can change the default installation directory by providing the `install_dir` argument. You can do it like this:

```r 
quartoextexp::run_app(install_dir = "path/to/your/quarto-project")
```

After installing it, you should see `quartoextexp` in the RStudio Addins menu. Click on it to launch the Shiny app.
