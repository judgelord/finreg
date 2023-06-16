# MOVE TO DOCKET TABLE SCRIPT ?
source("code/setup.R")
library(tidyverse)

load(here:here("data", "commenter_assets.Rdata"))

d <- commenter_assets


# helper function
false <- function(x){x == FALSE}

# table
docket_table <- d %>%
  ungroup() %>%
  mutate(docket_id = str_split(docket_id, ";")) %>%
  unnest(docket_id) %>%
  #add_count(docket_id, name = "docket_comment_count") %>%
  #group_by(docket_id, docket_comment_count) %>%
  group_by(comment_agency, docket_id) %>%
  mutate_all(is.na) %>%
  mutate_all(false) %>%
  summarise_all(sum)

names(docket_table)

docket_table %<>% mutate(Percent  = scales::percent( org_resources / comment_docket_count, accuracy = 1))

# select vars we want in the table
docket_table %<>%
  dplyr::select(Agency = comment_agency,
         docket_id,
         docket_comment_count = comment_docket_count,
         any_org_assets_measure = org_resources,
         Percent,
         contains("ASSET"),
         contains("revenue"),
         contains("marketcap"),
         contains("amount")
         )
# drop things we don't want in the table

# docket_table %<>%
#   select(#-`FDIC_Institutions-bestMatch:STNAME`,
#     -`nonprofits_resources-bestMatch:ein`,
#     -`SEC_Institutions-bestMatch:Ticker`,
#     -`compustat_resources-bestMatch:naics`
#     #-`opensecrets_resources_jwVersion-bestMatch:MeanContribAmountPerYearContributed`
#   )

names(docket_table) %<>% str_replace_all("-bestMatch:|_resources_|jwVersion_|_Institutions_", "_")

names(docket_table) %<>% str_replace_all("-bestMatch:|_resources_|jwVersion_|_Institutions_|__", "_")

names(docket_table) %<>% str_remove_all("__Total_")

names(docket_table)


# name for presentation
docket_table %<>%
  rename(Docket = docket_id,
         Comments = docket_comment_count,
         `Any Resources Measure` = any_org_assets_measure,
         `FDIC Assets` = ASSET,
         `Market Cap` = marketcap,
         `Assets (Credit Unions)` = `Total Assets`,
         `501c3 Assets` = assets,
         `501c3 Revenue` = revenue,
         `Campaign Donations` = MeanContribAmount)

docket_table %<>% select(-marketcap2)

names(docket_table)

docket_table %>%
  write_csv(file = here::here("data", "matches_per_docket.csv"))

view(docket_table)
