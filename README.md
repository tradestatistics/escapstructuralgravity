# escapstructuralgravity

The goal of escapstructuralgravity is to simulate the effect of policy variables 
over international trade flows for different countries. This project was funded by UN ESCAP.

## Run the dashboard locally

You can install the development version of escapstructuralgravity from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
install_github("tradestatistics/visualization-with-shiny", subdir = "common")
remotes::install_github("pachadotdev/escapstructuralgravity")
```

The `otsshinycommon` provides a set of specific functions to work with
international trade data in Shiny and `escapstructuralgravity` provides
all the visuals and statistics required to display the dashboard.

Then run the application locally with:

```r
escapstructuralgravity::run_app()
```

There are many options to run this in a server. In my own case, I relied on
systemd, which is a Linux-specific tool to run services.
