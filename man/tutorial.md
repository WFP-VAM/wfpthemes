# Getting started with `wfpthemes`

## Selecting color palettes

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

### Step 3: Create the Graph

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


Step 4: Tweak the Graph Settings

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