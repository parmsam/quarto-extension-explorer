
# quarto-extension-explorer

<!-- badges: start -->
<!-- badges: end -->

As mentiond on the Quarto website, Quarto Extensions are a powerful way to modify or extend the behavior of Quarto, and can be created and distributed by anyone. The goal of quartoextexp is to provide a Shiny gadget to explore the extensions available from the official [Quarto Extension Listing](https://quarto.org/docs/extensions/) and easily add one or more of them into a Quarto project.

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

<img src="man/figures/readme-rstudio-addin.jpg" width="55%" />

You can select one or more extensions in the **Available Extensions** data table then click `Setup R Code` (or press `enter` on your keyboard) to generate the R code to apply on the selected extensions to your Quarto project. Then you can click `Apply on Selected Extensions` (or press `shift + enter`) to run the R code you generated. By default, all of the extensions in the Quarto Extension Listing will appear in the **Available Extensions** data table. You can click on the Extension category buttons to filter the data table down (for example by Journal Articles or Revealjs). You can also search for a specific extension by typing in the search box.

<img src="man/figures/readme-small-screenshot.png" width="55%" />

You can see your installed extensions in the **Installed Extensions** data table. You can manage extensions by changing the mode from add extension to update or remove then selecting the relevant extension in the `Available Extensions` table. Then, same as before, you can setup your R code again and apply it on the selected extensions.

<img src="man/figures/readme-example-usage.gif" width="90%" />
