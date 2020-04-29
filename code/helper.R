
library(tidyverse)
library(lubridate)
library(broom)
library(data.table)
library(ggridges)

theme_set(theme_bw(base_size = 12))
# library(TMB)
# library(tidyverse)
# library(here)
# library(funcr)
# theme_set(theme_report())


# scale data to 1
range01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

# Format ggplot figures with ticked axes (especially good for marking year and
# age) 

# Depends on dplyr
tickr <- function(
  data, # dataframe
  var, # column of interest
  to # break point definition 
){
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>% 
    distinct(!!VAR) %>%
    ungroup(!!VAR) %>% 
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    select(breaks = UQ(VAR), labels)
}
