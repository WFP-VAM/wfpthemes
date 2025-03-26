<!-- README.md is generated from README.Rmd. Please edit that file -->

# wfpthemes

## Overview

The **wfpthemes** package provides a `ggplot2` theme and a set of
colour palettes for making graphics based on WFP Data Visualization Guidelines (forthcoming).
The goal of this package is to assist in the creation of charts, tables and maps
while promoting coherent data visualizations aligned with WFP visual identity with a predefined `ggplot` theme, as well as a set of colour palettes and scales.

## Installation (dev)
> NB This is the development version of the wfpthemes, it is frequently updated yet not stable.

Since this package is not yet on CRAN, you will need the remotes package to install it:

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

WFP uses **Open Sans** as its main font for publications and data visualizations. For other visual aspects, such as fonts and logo usage, please refer to the [WFP Visual Identity Guide](https://multimedia.wfp.org/Package/20SIJQCC99H).

## Usage

### WFP color palette

All recommended data visualization colors are accessible as **palettes**
or **scales** (color/fill).

``` r
display_wfp_all()
```

<img src="man/images/palettes.png" width="2100" />

### Example Chart

``` r
library(tidyverse)
library(wfpthemes)
library(scales)
library(labelled)

data(sampledataenglish, package = "wfpthemes")
glimpse(sampledataenglish)

#Script copied and pasted from 
#https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Reduced-coping-strategy-index/rCSI_tidyverse.R

data <- sampledataenglish %>% mutate(rCSI = rCSILessQlty + 
                          (rCSIBorrow * 2) + 
                          rCSIMealNb + 
                          rCSIMealSize + 
                          (rCSIMealAdult * 3))

# Create table of rCSI by ADMIN1 (unweighted) ----------------------------------------------#

rcsi_admin1_table_long <- data %>% 
  mutate(ADMIN1Name_lab = to_factor(ADMIN1Name)) %>% 
  group_by(ADMIN1Name_lab) %>% 
  drop_na(rCSI) %>%   
  summarise(meanrCSI = round(mean(rCSI),1))

# Create bar graph of rCSI ----------------------------------------------------# 

rcsi_barplot <- rcsi_admin1_table_long %>% ggplot() +
  geom_col(aes(
    x = meanrCSI,
    y = reorder(ADMIN1Name_lab, meanrCSI),
  ),
  fill = wfp_pal(n = 1, "pal_blue"),
  width = 0.8
  ) +
  labs(
    tag = "Figure 7",
    title = "Reduced Coping Strategy Index (rCSI) by State | April 2023",
    subtitle = "Average rCSI  per Household by State",
    x = "rCSI",
    y = "State",
    caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  ) + geom_text(aes(x = meanrCSI,
                    y = ADMIN1Name_lab, label = meanrCSI),
                hjust = -0.5,
                size = 8 / .pt
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0.1)),
    breaks = pretty_breaks(n = 7),
    labels = label_number()
  ) + theme_wfp(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") 

rcsi_barplot
```
<img src="man/images/rcsi_example.png" width="2100" />

## Quick Tutorial 

For a walkthrough on choosing color palettes and adjusting theme_wfp(), check out the [documentation](https://github.com/WFP-VAM/wfpthemes/blob/main/man/tutorial.md#quick-tutorial-on-wfpthemes).

## Getting help

If you run into issues or bugs, report them on the [GitHub Issues]((https://github.com/WFP-VAM/wfpthemes/issues)) page. Including a minimal reproducible example will make it easier to troubleshoot.

## License
All code and content in this repository is licensed under the GNU Affero General Public License v3.0 license.

## Contributing
We welcome contributions from the open source community! If you're interested in collaborating, please review our [WFP-VAM contribution page](https://github.com/WFP-VAM/.github/blob/main/profile/CONTRIBUTING.md).