source("code/setup.R")
library(tidyverse)


d_raw <- here(#"data", "match_data", # old in match_data
              #"finreg_commenter_covariates_df_20210824.csv"
              #"finreg_commenter_covariates_df_20211118.csv"
              "Data", # new copied from JLW-FINREG-PARTICIPATION/data to FINREGRULEMAKE2/Data
              #"finreg_commenter_covariates_df_20220405.csv"
              "finreg_commenter_covariates_df_20220510.csv"
              ) %>%
  str_remove("finreg") %>% # up a level to root
  read_csv()

# remove extraneous column
d_raw %<>%
  select(-`...1`)

d_raw %>% count(comment_agency, name = "covariates_20220510")


# how many exact match
d_raw %>% count(best_match_name == comment_org_name)

# where comments were on multiple dockets, combine docket ids into one cell to get unique rows
d_raw %<>% group_by(comment_url) %>%
  add_count(docket_id, name = "comment_docket_count", sort = T) %>%
  mutate(docket_id = paste(docket_id, collapse = ";")) %>%
  distinct() %>%
  select(docket_id, comment_docket_count, docket_id, comment_url,  everything()) %>%
  ungroup()

# duplicates
d_raw %>%
  add_count(comment_url, name = "comment_url_count") %>%
  filter(comment_url_count > 1)

# matched per type / external dataset
d_raw %>% count(best_match_type)

# identify columns we want to keep (there are a lot of columns)
names(d_raw)

d_raw %>% select(contains("name")) %>% names()

d_raw %>% select(contains("asset")) %>% names()

d_raw %>% select(contains("revenue")) %>% names()

d_raw %>% select(contains("tic")) %>% names()

d_raw %>% select(contains("ein")) %>% names()

d_raw %>% select(contains("contrib")) %>% names()

d_raw %>% select(contains("cap")) %>% names()

# no compustat data
d_raw$`compustat_resources-bestMatch:marketcap` %>% unique()
d_raw$`compustat_resources-bestMatch:best_match_name` %>% unique()

# but we have marketcap for CIK matches that came from compustat, so all of these must also be in compustat
d_raw$`CIK-bestMatch:marketcap` %>% unique()


look <- d_raw %>% select(contains("estmatch:marketcap")) %>% distinct()


# d_raw %>% select(contains("union")) %>% filter(!is.na(`CreditUnions-bestMatch:best_match_name`)) %>% view()


d_raw %>%
  filter(!is.na(`CIK-bestMatch:marketcap`)) %>%
  distinct(comment_org_name, best_match_name, `CIK-bestMatch:marketcap`) %>%
  kablebox()

d_raw %>% select(contains("rssd")) %>% names()


look <- d_raw %>% filter(!is.na(`nonprofits_resources-bestMatch:name`)) %>%
  count(`nonprofits_resources-bestMatch:name`,
        `nonprofits_resources-bestMatch:ein`, sort = T)

look <- d_raw %>% filter(!is.na(`CreditUnions-bestMatch:best_match_name`) ) %>%
                 count(`CreditUnions-bestMatch:RSSD`, sort = T)

d_raw %>%
  filter(!is.na(`compustat_resources-bestMatch:best_match_name`) ) %>%
  count(`compustat_resources-bestMatch:tic`, sort = T)


# identify needed cols
d_raw %>%
  dplyr::select(contains("FDIC_Institutions-bestMatch:")) %>%
  filter(!is.na(`FDIC_Institutions-bestMatch:ASSET`))  %>% kablebox()

# select only best matches
d_best <- d_raw %>% select(best_match_name, contains("orgMatch:best_match_name"))

names(d_best)

d_best %<>% mutate(across(everything(), is.na))

d_best %<>%
  rownames_to_column("id") %>%
  group_by(id) %>%
  mutate(
  matches = sum(
  #`best_match_name`,
  `CIK-orgMatch:best_match_name`,
  `CreditUnions-orgMatch:best_match_name`,
  `FDIC_Institutions-orgMatch:best_match_name`,
  `FFIECInstitutions-orgMatch:best_match_name`,
  `SEC_Institutions-orgMatch:best_match_name`,
  `compustat_resources-orgMatch:best_match_name`,
  `nonprofits_resources-orgMatch:best_match_name`,
  `opensecrets_resources_jwVersion-orgMatch:best_match_name`))

# how many matches
unique(d_best$matches - 7)


d <-  d_raw %>%
  filter(is_likely_org == 1) %>%
  dplyr::select(
  # comment vars
  dplyr::starts_with("docket"),
  dplyr::starts_with("comment"),
  # general org vars
  dplyr::contains("bestMatch:best_match_name"),
  #match_in_sample,
  dplyr::starts_with("best_match"),
  # matched data:
  # FDIC
  `FDIC_Institutions-bestMatch:ASSET`,
  `FDIC_Institutions-bestMatch:STNAME`,
  # # CIK
  `CIK-bestMatch:marketcap`,
  # `CIK-bestMatch:netinc`, # MISSING
  # SEC
  `SEC_Institutions-bestMatch:Ticker`,
  # # COMPUTSTAT
  `compustat_resources-bestMatch:marketcap`,
  # `compustat_resources-bestMatch:netinc`, # MISSING
  `compustat_resources-bestMatch:naics`,
  # # nonprofits
  `nonprofits_resources-bestMatch:assets`,
  `nonprofits_resources-bestMatch:revenue`,
  `nonprofits_resources-bestMatch:ein`,
  # # opensecrets
  `opensecrets_resources_jwVersion-bestMatch:MeanContribAmountPerYearContributed`,
  `opensecrets_resources_jwVersion-bestMatch:TotalContribAmount`
  # is_likely_org
  )


names(d)

sum_na <- . %>% is.na() %>% sum()

tibble(var = names(d) ,
       not_NA = nrow(d) - d %>% summarise_all(sum_na) %>% as.numeric(),
       percent_NA = d %>% summarise_all(sum_na) %>% as.numeric() / nrow(d) ) %>%
  mutate(percent_NA = percent_NA %>% scales::percent(accuracy = 1) ) %>%
  arrange(not_NA) %>%
  kable(format = "markdown")

# 2022 data
# more credit unions
# more CIK names, but only 10% have cap data
# more FDIC institutions (double)
# a few FFIEC now (57 vs 0)
# a log more nonprofits
# more opensecrets
# still no SEC



names(d) %<>%  str_remove("-bestMatch:best_match")

d %>% select(ends_with("name")) %>% names()





# 2k
d %>% filter(!is.na(CreditUnions_name)) %>% select(ends_with("name"))

# 5k
d %>% filter(!is.na(CIK_name)) %>% select(ends_with("name"))

d %>% filter(!is.na(compustat_resources_name)) %>% select(ends_with("name"))


# 1k, now 2k
d %>% filter(!is.na(FDIC_Institutions_name)) %>% select(ends_with("name"))
look <- d_raw %>%
  filter(!is.na(`CIK-submitterMatch:best_match_name`)) %>%
  filter(!is.na(`FDIC_Institutions-bestMatch:best_match_name`)) %>%
  select(ends_with("best_match_name"))

# 0
d %>% filter(!is.na(FFIECInstitutions_name)) %>% select(ends_with("name"))
look <- d_raw %>%
  #filter(!is.na(`FFIECInstitutions-submitterMatch:best_match_name`)) %>%
  filter(!is.na(`FFIECInstitutions-orgMatch:best_match_name`)) %>%
  select(ends_with("best_match_name"))

# 18k, now 20k
d %>% filter(!is.na(nonprofits_resources_name)) %>% select(ends_with("name"))


# 4k
d %>% filter(!is.na(opensecrets_resources_jwVersion_name)) %>% select(ends_with("name"))

# 0
d %>% filter(!is.na(SEC_Institutions_name)) %>% select(ends_with("name"))
d %>% filter(!is.na(`SEC_Institutions-bestMatch:Ticker`)) %>% select(ends_with("name"))


# combine matches across datasets
d %<>% mutate(matches = paste0(CIK_name,"-CIK;",
                               compustat_resources_name,"-Computstat;",
                               CreditUnions_name, "-Credit Union;",
                               FDIC_Institutions_name,"-FDIC;",
                               FFIECInstitutions_name,"-FFIEC;",
                               nonprofits_resources_name,"-Nonprofit;",
                               opensecrets_resources_jwVersion_name,"-OpenSecrets;",
                               SEC_Institutions_name,"-SEC") %>%
                str_remove_all("NA-.*?;|;NA-SEC|NA-SEC|;$") %>%
                str_remove_all(";$") %>%
                str_remove(";NA-SEC"))

d$matches %>% head()

# drop non matches
# d %<>% filter(matches != "")

# multiple matches
d %>% filter(str_detect(matches, ";"))

# make org_type and org_name
d %<>% mutate(org_name = matches %>% str_remove("-.*"),
              org_type = matches %>% str_remove(".*-") %>% str_remove(";"),
              org_resources = coalesce(
                `nonprofits_resources-bestMatch:assets` %>% as.numeric(),
                `FDIC_Institutions-bestMatch:ASSET` %>% as.numeric(),
                `CIK-bestMatch:marketcap` %>% as.numeric(),
                `compustat_resources-bestMatch:marketcap` %>% as.numeric()
                                       ))

# align JW's version
d$best_match_type %<>%
  str_replace("CreditUnions", "Credit Union") %>%
  str_replace("nonprofits", "Nonprofit") %>%
  str_replace("opensecrets", "OpenSecrets") %>%
  str_replace("compustat", "Compustat") %>%
  str_replace("SEC_Institutions", "CIK") %>%
  str_remove_all("_.*|Institutions")

d %>%
  filter(!is.na(`CIK-bestMatch:marketcap`)) %>%
  distinct(comment_org_name, best_match_name, `CIK-bestMatch:marketcap`) %>%
  kablebox()

# check congruence
d %>% count(org_type)

d %>% count(best_match_type)

# adjust variable name
d %<>% mutate(opensecrets_resources_name = opensecrets_resources_jwVersion_name)

# add a count
d %<>% add_count(comment_org_name, name = "org_comments") %>%
  add_count(best_match_name, name = "best_match_org_comments")




# NAMING THINGS

# combining some similar asset data

# campaign contributions and assets
d %<>% mutate(
  MeanContribAmount = coalesce(`opensecrets_resources_jwVersion-bestMatch:MeanContribAmountPerYearContributed`),
  assets = coalesce( `FDIC_Institutions-bestMatch:ASSET`, `nonprofits_resources-bestMatch:assets`) )

# market cap
d  %<>%
  mutate(marketcap = coalesce(`CIK-bestMatch:marketcap`,
                              `compustat_resources-bestMatch:marketcap`# currently NA for everyting
                              ),
         marketcap2 = ifelse(str_detect(marketcap, "k"),
                             marketcap %>% str_remove("k") %>% as.numeric() * 1000,
                             marketcap),
         marketcap2 = ifelse(str_detect(marketcap, "M"),
                             marketcap %>% str_remove("M") %>% as.numeric() * 1000000,
                             marketcap2),
         marketcap2 = ifelse(str_detect(marketcap, "B"),
                             marketcap %>% str_remove("B") %>% as.numeric() * 1000000000,
                             marketcap2),
         marketcap2 = ifelse(str_detect(marketcap, "T"),
                             marketcap %>% str_remove("T") %>% as.numeric() * 1000000000000,
                             marketcap2),
         marketcap2 = marketcap2 %>% str_remove(",") %>%  as.numeric(marketcap2))


#FIXME This is dumb
d %<>% mutate(org_resources =
                coalesce(org_resources,
                         MeanContribAmount,
                         assets,
                         marketcap2))

#d$org_resources

d %>% distinct(comment_url) %>% nrow()
















match_data_clean <- d %>%
  # filter(!is.na(best_match_type)) %>%
  distinct()

# save comment-level match data
save(match_data_clean, file = here("data", "match_data_clean.Rdata"))






















# MOVE TO DOCKET TABLE SCRIPT ?


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
  select(Agency = comment_agency,
         docket_id,
         docket_comment_count = comment_docket_count, any_org_assets_measure = org_resources,
         Percent,
         contains("bestMatch")) %>%
  # drop things we don't want in the table
  select(-`FDIC_Institutions-bestMatch:STNAME`,
         -`nonprofits_resources-bestMatch:ein`,
         -`SEC_Institutions-bestMatch:Ticker`,
         -`compustat_resources-bestMatch:naics`,
         -`opensecrets_resources_jwVersion-bestMatch:MeanContribAmountPerYearContributed`)

names(docket_table) %<>% str_replace_all("-bestMatch:|_resources_|jwVersion_|_Institutions_|ContribAmount", "_")

names(docket_table) %<>% str_replace_all("-bestMatch:|_resources_|jwVersion_|_Institutions_|ContribAmount|__", "_")

names(docket_table) %<>% str_remove_all("__Total_")

names(docket_table)


# name for presentation
docket_table %<>%
  rename(Docket = docket_id,
         Comments = docket_comment_count,
         `Any Resources Measure` = any_org_assets_measure,
         `FDIC Assets` = FDIC_ASSET,
         `Market Cap (CIK)` = CIK_marketcap,
         `Market Cap (Compustat)` = compustat_marketcap,
         `501c3 Assets` = nonprofits_assets,
         `501c3 Revenue` = nonprofits_revenue,
         `Campaign Donations` = opensecrets)

names(docket_table)
view(docket_table)

docket_table %>%
write_csv(file = here::here("data", "matches_per_docket.csv"))

