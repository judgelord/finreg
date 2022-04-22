source("code/setup.R")
library(tidyverse)


d_raw <- here(#"data", #"match_data", # old in match_data
              #"finreg_commenter_covariates_df_20210824.csv"
              # "finreg_commenter_covariates_df_20211118.csv"
              "Data", # new copied from JLW-FINREG-PARTICIPATION/data to FINREGRULEMAKE2/Data
              "finreg_commenter_covariates_df_20220405.csv"
              ) %>%
  str_remove("finreg") %>% # up a level to root
  read_csv()

d_raw %>% count(best_match_type)


head(d_raw)

names(d_raw)

d_raw %>% select(contains("name")) %>% names()

d_raw %>% select(contains("asset")) %>% names()

d_raw %>% select(contains("revenue")) %>% names()

d_raw %>% select(contains("tic")) %>% names()

d_raw %>% select(contains("ein")) %>% names()

d_raw %>% select(contains("contrib")) %>% names()

d_raw %>% select(contains("cap")) %>% names()

look <- d_raw %>% select(contains("estmatch:marketcap")) %>% distinct()

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

# 1k, now 2k
d %>% filter(!is.na(FDIC_Institutions_name)) %>% select(ends_with("name"))

# 0, now 57
d %>% filter(!is.na(FFIECInstitutions_name)) %>% select(ends_with("name"))

# 18k, now 20k
d %>% filter(!is.na(nonprofits_resources_name)) %>% select(ends_with("name"))


# 4k
d %>% filter(!is.na(opensecrets_resources_jwVersion_name)) %>% select(ends_with("name"))

# 0
d %>% filter(!is.na(SEC_Institutions_name)) %>% select(ends_with("name"))

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

# check congruence between slecting
d %>% count(org_type)

d %>% count(best_match_type)

d %<>% add_count(comment_org_name, name = "org_comments")

match_data_clean <- d %>% filter(!is.na(best_match_type))

# save comment-level match data
save(match_data_clean, file = here("data", "match_data_clean.Rdata"))

