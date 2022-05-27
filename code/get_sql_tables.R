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

attachments <- dbGetQuery(con, "SELECT comment_url, attachment_url FROM attachments")

head(attachments)


comments %>% count(agency_acronym, name = "comments table")


attachments %<>% left_join(comments %>% select(comment_url, agency_acronym))


attachments %>%
  count(agency_acronym, name = "attachments_table") %>%
  full_join(comments %>% count(agency_acronym, name = "comments table")) %>%
  full_join(technical_terms_comments %>%
              distinct() %>%
              left_join(comments) %>%
              count(agency_acronym, name = "term counts"))

attachments %>%
  count(agency_acronym, name = "attachments_table") %>%
  full_join(comments %>% count(agency_acronym, name = "comments table")) %>%
  full_join(d_raw %>%
              count(comment_agency, name = "covariates_20220510") %>%
              rename(agency_acronym = comment_agency))



#FDIC misnamed?
attachments %>% filter(is.na(agency_acronym))

# Steves term counts
load(here("data", "Dictionary_Terms.Rdata")  %>% str_remove("finreg")  )

technical_terms_comments %>%
  distinct() %>%
  left_join(comments) %>%
  count(agency_acronym)
