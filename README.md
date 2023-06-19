<!-- README.md is generated from README.Rmd. Please edit that file -->

# wfpthemes

## Overview

The **wfpthemes** package provides a `ggplot2` theme and a set of
colour palettes for making graphics based on WFP Data Visualization Guidelines (in progress) .
The goal of this package is to assist in the creation of charts, tables and maps
while promoting the WFP visual identity with a predefined `ggplot` theme, as well as a set of colour palettes and scales.

## Installation

This package is not on yet on CRAN and to install it, you will need the
remotes package.

``` r
install.packages("remotes")
remotes::install_github("WFP-VAM/wfpthemes")
```

## Content

A package with all necessary elements to quickly implement WFP Data Visualization Guidelines in your statistical
products and data stories:

1.  Adjusted `ggplot2` theme
2.  A series of color palettes for:
    
## Fonts

WFP uses **Open Sans** as its main font for publications and data
visualizations. Fonts often creat headaches in R so if you have problems its not your fault.

## Usage

``` r
library(tidyverse)
library(scales)
library(wfpthemes)
```

### Base ggplot2 theme

### WFP color palette

All recommended data visualization colors are accessible as **palettes**
or **scales** (color/fill).

``` r
display_wfp_all()
```

<img src="man/images/palettes.png" width="2100" />

### Base theme and color scale

