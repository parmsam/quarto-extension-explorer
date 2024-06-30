
# quarto-extension-explorer

<!-- badges: start -->
<!-- badges: end -->

The goal of quarto-extension-explorer is to provide a pretty simple Shiny app to explore the extensions available from the official [Quarto Extension Listing](https://quarto.org/docs/extensions/) and easily add one or more of them into a Quarto project.

You can access the app by cloning the repo or running the following command in R while in your project working directory. 

```r
shiny::runGitHub("quarto-extension-explorer", "parmsam", "master", destdir = ".")
```

Be sure to change your installation directory in the Shiny app if using the above function.
