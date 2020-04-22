# 3_find_extreme_abstracts.R
# find abstracts with an extreme number of acronyms
# April 2020
library(broom)
library(dplyr)
library(lme4) # for random effects
library(merTools) # for prediction intervals
library(stringr)
source('99_make_analysis_data.R') # creates `for.model` depending on the acronym size 
