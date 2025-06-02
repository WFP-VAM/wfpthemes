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


## Getting started

Let's say you want to create a graph based on one of the Household Hunger Strategy questions.

### Step 1: Create a Table of Results

First, create a table of results in a format that's easy to use for graphing:

``` r
library(tidyverse)
library(wfpthemes)

data(sampledataenglish, package = "wfpthemes")

HHSNoFood_admin1_table_long <- sampledataenglish %>% 
  group_by(ADMIN1Name_lab = labelled::to_factor(ADMIN1Name)) %>%
  count(HHSNoFood_lab = labelled::to_factor(HHSNoFood)) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>% mutate_if(is.numeric, round, 1) 
  
glimpse(HHSNoFood_admin1_table_long)  
```


### Step 2: Display Available Color Palettes

Next, let's take a look at the available color palettes:

``` r
display_wfp_all()
```

<img src="images/palettes.png" width="2100" />


Since we have two response options  let's ise the main  pal_wfp_main, to create our graph.

#### Step 3: Create the Graph

Let's create the graph:

``` r
HHSNoFood_admin1_barplot <- ggplot(HHSNoFood_admin1_table_long) +geom_bar(
    aes(x = ADMIN1Name_lab, y = perc, fill = HHSNoFood_lab, group = HHSNoFood_lab), 
    stat='identity', position=position_dodge(.7),  width = 0.6,
  ) +
  geom_text(
    aes(x = ADMIN1Name_lab, y = perc, label = perc, group = HHSNoFood_lab),
    position = position_dodge(width = 0.6),
    vjust = -0.5, size = 2.5
  )+
  scale_fill_wfp_b(palette = "wfp_main_8cat") +
  labs(
    title = "Percentage of Households Reporting No Food to Eat | April 2024",
    subtitle = "Based on the question: 'In the past [4 weeks/30 days], was there ever no food to eat of
any kind in your house because of lack of resources to get food?'",
    caption = "Source: Emergency Food Security Assessment, data collected April 2024"
  ) + theme_wfp()

plot(HHSNoFood_admin1_barplot)
```

<img src="images/HHSNoFood_admin1_barplot_ugly" width="1500" />


### Step 4: Tweak the Graph Settings

Lastly, we applied theme_wfp(), but it still looks cluttered. Let's tweak the settings to remove unnecessary elements. Since the title description already indicates that the y-axis represents percentages and the data labels contain the values, we can remove all y-axis labels, text, and grid lines. For the x-axis, we only need to show the axis text (the names of the states).


``` r
HHSNoFood_admin1_barplot <- HHSNoFood_admin1_barplot + theme_wfp(
    grid = FALSE,
    axis = FALSE,
    axis_title = FALSE,
    axis_text = "x")

plot(HHSNoFood_admin1_barplot)
```

<img src="images/HHSNoFood_admin1_barplot_less_uggly" width="1500" />

## Additional examples

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
  fill = wfp_pal(n = 1, "blue_sequential"),
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

## Getting help

If you run into issues or bugs, report them on the [GitHub Issues]((https://github.com/WFP-VAM/wfpthemes/issues)) page. Including a minimal reproducible example will make it easier to troubleshoot.

## License
All code and content in this repository is licensed under the GNU Affero General Public License v3.0 license.

## Contributing
We welcome contributions from the open source community! If you're interested in collaborating, please review our [WFP-VAM contribution page](https://github.com/WFP-VAM/.github/blob/main/profile/CONTRIBUTING.md).

Special thanks to Nicole Wu (@nicolefindstar), William Olander (@olanderb) and William McFall (@willmcfall) for starting this project.
