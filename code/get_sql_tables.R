# Create RSQLite database
library(DBI)
# install.packages("RSQLite")
1
library(RSQLite)
library(tidyverse)
library(magrittr)

# Create RSQLite database
con <- dbConnect(RSQLite::SQLite(), here::here("Data", "master.sqlite") %>% str_remove("finreg"))

# fetch results:
comments <- dbGetQuery(con, "SELECT * FROM comments")

head(comments)

# write_csv(distinct(comments, comment_url, organization), file = here::here("data", "comment_url_orgs.csv"))

attachments <- dbGetQuery(con, "SELECT comment_url, attachment_url FROM attachments")

head(attachments)


comments %>% count(agency_acronym, name = "comments table")


attachments %<>% left_join(comments %>% dplyr::select(comment_url, agency_acronym))




# d_raw is from "finreg/code/clean_match_data.R" and pulls in the version of the matched data we are currently using for the analysis
attachments %>%
  count(agency_acronym, name = "attachments_table") %>%
  full_join(comments %>% count(agency_acronym, name = "comments table")) %>%
  full_join(d_raw %>%
              count(comment_agency, name = "covariates_20220510") %>%
              rename(agency_acronym = comment_agency))



#FDIC misnamed?
# No, but need to fix some OCC and CFPB comments, where comment urls are failing to merge, aparently because they are missing from the comment table
attachments %>% filter(is.na(agency_acronym))

# temp fix for counting
attachments %<>% mutate(agency_acronym = coalesce(agency_acronym, attachment_url) %>%
                          str_remove("-..*") %>%
                          str_remove(".*/"))

comments %>% filter(str_detect(comment_url, "OCC-2011-0019-0010"))


# Steves term counts
load(here("data", "Dictionary_Terms.Rdata")  %>% str_remove("finreg")  )

term_counts <- technical_terms_comments %>%
  distinct() %>%
  left_join(comments) %>%
  count(agency_acronym, name = "term counts")

term_counts

status <- attachments %>%
  count(agency_acronym, name = "attachments_table") %>%
  full_join(comments %>%
              count(agency_acronym, name = "comments table")) %>%
  full_join(term_counts)

status

# it appears that FDIC attachments are not merging on comment_url,
# but these NAs are actually OCC and CFPB comments

write_csv(status, file = here::here("data", "master_tables_status.csv"))

