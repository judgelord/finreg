
# Working directory
=

# Load packages

library(tidyverse)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load, merge, clean data ----


#CompuStat names
compustat_names <- read_csv("data/CompustatNames.csv")


#Yahoo financial data/info on stocktickers (long)
yahoo <- read_csv("data/yahoo_tic-summaries.csv")



# Left join test (market cap)

compustat <-
  left_join(
    compustat_names,
    yahoo %>%
      filter(var == "Market Cap") %>%
      select(tic, marketcap = value))

#Missing: 57%  !!!
nrow(compustat %>% filter(is.na(marketcap)))/nrow(compustat)

# Missing example: SERVADYNE INC
# http://www.findata.co.nz/markets/stockquote/nasdaq/serv.htm



# Cleaning

#Millions/billions to numeric




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Commonalize ??? ----


#






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export ----


write_csv(compustat, "data/merged_resources/compustat_resources.csv")







