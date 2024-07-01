
# quartoextexp

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

This is a basic example which shows you how to solve a common problem:

``` r
library(quartoextexp)
## basic example code
quartoextexp::run_app()
```

After installing it, you should see `Quarto Extension Explorer` in the RStudio Addins menu. Click on it to launch the Shiny app.
