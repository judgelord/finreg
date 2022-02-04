source("code/setup.R")
library(tidyverse)


d_raw <- here("data", #"match_data", # old in match_data
              #"finreg_commenter_covariates_df_20210824.csv"
              "finreg_commenter_covariates_df_20211118.csv"
              ) %>%
  read_csv()


head(d_raw)

names(d_raw)

d_raw %>% select(contains("name")) %>% names()

d_raw %>% select(contains("asset")) %>% names()

d_raw %>% select(contains("revenue")) %>% names()

d_raw %>% select(contains("tic")) %>% names()

d_raw %>% select(contains("ein")) %>% names()

d_raw %>% select(contains("contrib")) %>% names()

d_raw %>% select(contains("cap")) %>% names()

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
  dplyr::select(contains("FDIC_Institutions-orgMatch:")) %>%
  filter(!is.na(`FDIC_Institutions-orgMatch:ASSET`))  %>% kablebox()

d <-  d_raw %>% dplyr::select(
  # comment vars
  dplyr::starts_with("docket"),
  dplyr::starts_with("comment"),
  # general org vars
  dplyr::contains("orgMatch:best_match_name"),
  match_in_sample,
  # matched data:
  # FDIC
  `FDIC_Institutions-orgMatch:ASSET`,
  `FDIC_Institutions-orgMatch:STNAME`,
  # # CIK
  # #`CIK-bestMatch:marketcap`,
  # `CIK-bestMatch:netinc`,
  # SEC
  `SEC_Institutions-bestMatch:Ticker`,
  # # COMPUTSTAT
  # `compustat_resources-bestMatch:marketcap`,
  # `compustat_resources-bestMatch:netinc`,
  `compustat_resources-bestMatch:naics`,
  # # nonprofits
  # `nonprofits_resources-bestMatch:assets`,
  # `nonprofits_resources-bestMatch:revenue`,
  `nonprofits_resources-bestMatch:ein`,
  # # opensecrets
  # `opensecrets_resources_jwVersion-bestMatch:MeanContribAmountPerYearContributed`,
  # `opensecrets_resources_jwVersion-bestMatch:TotalContribAmount`
  is_likely_org
  )

d %<>% filter(match_in_sample)


names(d)

sum_na <- . %>% is.na() %>% sum()

tibble(var = names(d) ,
       NAs = d %>% summarise_all(sum_na) %>% as.numeric() ) %>%
  arrange(NAs) %>%
  kable()

names(d) %<>%  str_remove("-orgMatch:best_match")

d %>% select(ends_with("name")) %>% names()





# 2k
d %>% filter(!is.na(CreditUnions_name)) %>% select(ends_with("name"))

# 5k
d %>% filter(!is.na(CIK_name)) %>% select(ends_with("name"))

# 1k
d %>% filter(!is.na(FDIC_Institutions_name)) %>% select(ends_with("name"))

# 0
d %>% filter(!is.na(FFIECInstitutions_name)) %>% select(ends_with("name"))

# 18k
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
                str_remove_all("NA-.*?;|;NA-SEC|NA-SEC") %>%
                str_remove(";NA-SEC")) %>%
  filter(matches != "")

d$matches %>% head()

# drop non matches
d %<>% filter(matches != "")

# multiple matches
d %>% filter(matches %>% str_detect(";"))

# make org_type and org_name
d %<>% mutate(org_name = matches %>% str_remove("-.*"),
              org_type = matches %>% str_remove(".*-") %>% str_remove(";"),
              org_resources = coalesce(`FDIC_Institutions-orgMatch:ASSET`))

# check
d$org_type %>% unique()

match_data_clean <- d

# save comment-level match data
save(match_data_clean, file = here("data", "match_data_clean.Rdata"))

