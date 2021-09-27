

# Load packages

library(tidyverse)
library(data.table)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load and crosswalk data
# Crosswalking variable: `ein`


# List of non-profit organizations

orgs <- read_csv("data/NonProfitTax.csv")


# IRS tax data on non-profit organizations
#
# irs.gov/statistics/soi-tax-stats-annual-extract-of-tax-exempt-organization-financial-data
# variables: https://www.irs.gov/pub/irs-soi/12eofinextractdoc.xls

irs <- rbind(
  irs990 <- fread("data/py12_990.dat") %>%
    select(ein = EIN,
           assets = totassetsend,
           revenue = totrevenue),
  irs990ez <- fread("data/py12_990ez.dat") %>%
    select(ein, assets = totassetsend, revenue = totrevnue),
  irs990pf <- fread("data/py12_990pf.dat") %>%
    select(ein = EIN,
           assets = TOTASSETSEND,
           revenue = TOTRCPTPERBKS)
  )


# Merged
nonprofits <- left_join(orgs, irs) %>%
  mutate(assets = as.numeric(assets),
         revenue = as.numeric(revenue))


# 32% missing ----- NEED TO RE SCRAPE JSONS !!!
nrow(nonprofits %>% filter(is.na(revenue)))/nrow(nonprofits)

write_csv(nonprofits, "data/merged_resources/nonprofits_resources.csv")



# Density plots on explanatory variables

nonprofits %>% ggplot() + geom_density(aes(x = log(assets)))
nonprofits %>% ggplot() + geom_density(aes(x = log(revenue)))








#
#
# # ! many .json files don't have info --
# # ! instead they contain warnings about scraping !
#
# # .json with >1 KB size (actual info present)
# # j <- dir("data/tax_jsons/")[10]
# library(jsonlite)
#
#
# tax <- NULL
# for(j in dir("data/tax_jsons/")){
#
#   #Iteration
#   j_path <- paste0("data/tax_jsons/", j)
#
#   #Condition met
#   if(file.size(j_path) > 730){
#     json <- enframe(unlist(fromJSON(j_path)))
#     cat("\nJSON file", j, ": Extractable")
#
#     temp <-
#       data.frame(
#         ein = unname(json[json$name == "organization.ein", "value"]),
#         json,
#         stringsAsFactors = FALSE)
#
#     tax <- rbind(tax, temp)
#
#   #Condition not met
#   }else{
#     cat("\n!! JSON file", j, ": NOT extractable !!")
#   }
# }
#
# # Wide -- variables including asset and income amount (for now)
# tax_wide <- tax %>%
#   filter(name %in% c("organization.asset_amount", "organization.income_amount")) %>%
#   pivot_wider(names_from = name, values_from = value) %>%
#   mutate(ein = as.numeric(ein))
#
#
# # Steven: variables vary in each json
# #  account for variable missingness
#
# data.frame(table(tax$name)) %>% arrange(desc(Freq)) %>% View()
#
# # Steven:
# #  grab market cap, asset tender mgmt, availability
#
# # data.frame(table(tax$name)) %>% filter(grepl("cap", Var1))
#
# #
# # asset_amount, income_amount
#
#
#
# # Me: Average any values?
#
#
#
#
#
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Merge and export data
#
#
# nonprofits <- left_join(orgs, tax_wide)
#
#
# # 99% missing ----- NEED TO RE SCRAPE JSONS !!!
# nrow(nonprofits %>% filter(is.na(organization.asset_amount)))/nrow(nonprofits)
#
#
#
