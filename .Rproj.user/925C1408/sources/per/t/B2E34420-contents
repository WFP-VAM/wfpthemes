#------------------------------------------------------------------------------#

#	                        WFP RAM Standardized Scripts
#                      Calculating and Visualizing FCS

#------------------------------------------------------------------------------#



## Load Packages --------------------------------------------------------------#

library(tidyverse)
library(dplyr)
library(labelled)
library(expss)
library(haven)
library(officer)
library(gtsummary)

# WFP themes package will be updated frequently - reinstall it everytime for now
library(devtools)
install_github("WFP-VAM/wfpthemes")
library(wfpthemes)

# Load Sample Data ------------------------------------------------------------#

data <- haven::read_sav("data/sampledataenglish.sav")

# Calculate FCS & FCG ---------------------------------------------------------# 
# script copied and pasted from 
# https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Food-consumption-score/FCS_indicator_tidyverse.R

data <- data %>% mutate(FCS = (FCSStap  * 2) + 
                              (FCSPulse * 3) +
                              (FCSPr    * 4) +
                              (FCSDairy * 4) + 
                               FCSVeg +
                               FCSFruit +
                              (FCSFat   * 0.5) +
                              (FCSSugar * 0.5))
var_label(data$FCS) <- "Food Consumption Score"

# Create FCG groups based on 21/55 or 28/42 thresholds
# Use this when analyzing a country with low consumption of sugar and oil - thresholds 21-35
data <- data %>% mutate(FCSCat21 = case_when(
                                   FCS <= 21 ~ 1, 
                                   between(FCS, 21.5, 35) ~ 2, 
                                   FCS > 35 ~ 3),
                        FCSCat28 = case_when(
                                   FCS <= 28 ~ 1, 
                                   between(FCS, 28.5, 42) ~ 2, 
                                   FCS > 42 ~ 3))
val_lab(data$FCSCat21) = num_lab("
             1 Poor
             2 Borderline
             3 Acceptable
")
var_label(data$FCSCat21) <- "FCS Categories: 21/35 thresholds"

val_lab(data$FCSCat28) = num_lab("
             1 Poor
             2 Borderline
             3 Acceptable
")
var_label(data$FCSCat28) <- "FCS Categories: 28/42 thresholds"

#creates long table 
fcscat21_admin1_table_long <- data %>% 
  group_by(ADMIN1Name_lab = to_factor(ADMIN1Name)) %>%
  count(FCSCat21_lab = as.character(FCSCat21)) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>% mutate_if(is.numeric, round, 1) 

## Create the bar graph -------------------------------------------------------# 
#create a palette of fcs based on wfpthemes palette and set ordering of values
#this will make sure proper color gets assigned to proper value no mater how table of values was created
pal_fcs <- wfp_pal("pal_stoplight_3pt", n = 3)
order_fcs <- c("Acceptable", "Borderline", "Poor")
pal_fcs <- setNames(pal_fcs, order_fcs)

#and now the graph - option1 - no y axis 
fcscat21_barplot <- fcscat21_admin1_table_long %>% 
  ggplot() +
  geom_col(
    aes(x = fct_reorder2(ADMIN1Name_lab,
                         perc,  
                         FCSCat21_lab,
                         \(x,y) sum(x*(y=="Acceptable"))), 
               y = perc,
               fill = factor(FCSCat21_lab,level=order_fcs)), 
           width = 0.7) +
  geom_text(aes(x = ADMIN1Name_lab,
                y = perc,
                color = factor(FCSCat21_lab,level=order_fcs),
                label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            size = 10/.pt) +
  scale_color_manual(
    values = c(main_white, main_black, main_white)
    ) +
  labs(tag = "Figure 1",
       title = "Household Food Consumption Score (FCS) Groups by State | April 2023",
       subtitle = "Percentage of Households per FCS Group per State in Example Country",
       caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  ) +  scale_fill_manual(values = pal_fcs) + theme_wfp(grid = FALSE, axis_text = "x", axis = F, axis_title = F) 

fcscat21_barplot
