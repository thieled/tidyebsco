
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyebsco

<!-- badges: start -->
<!-- badges: end -->

The goal of tidyebsco is to read xml files exported from EBSCOhost in a
tidy format.

## Installation

You can install the development version of tidyebsco from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thieled/tidyebsco")
```

## Example

This is a basic example:

``` r
library(tidyebsco)

# download and unzip the xml file (that you received by email from EBSCOhost export):
xml_file <- load_file("https://exports.ebscohost.com/sdc/9f196fc3-aa83-44f5-95fa-0c7d945b31a3.zip")

# Read, parse and tidy xml file:
df <- tidy_ebsco_xml(xml_file)

# Inspect dataframe
tibble::glimpse(df)
```
