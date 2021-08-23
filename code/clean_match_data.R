source("code/setup.R")
library(tidyverse)

d_raw <- here("data", "match_data", "finreg_commenter_covariates_df_20210812.csv") %>%
read_csv()

names(d_raw)

# identify needed cols
d_raw %>%
  dplyr::select(contains("FDIC_Institutions-orgMatch:")) %>%
  filter(!is.na(`FDIC_Institutions-orgMatch:ASSET`))  %>% kablebox()

d <-  d_raw %>% dplyr::select(dplyr::starts_with("comment"),
                       dplyr::contains("orgMatch:best_match_name"),
                       is_likely_org,
                       `FDIC_Institutions-orgMatch:ASSET`,
                       `FDIC_Institutions-orgMatch:STNAME`)

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
d %>% filter(!is.na(NonProfitTax_name)) %>% select(ends_with("name"))

# 4k
d %>% filter(!is.na(OpenSecretsOrgs_name)) %>% select(ends_with("name"))

# 0
d %>% filter(!is.na(SEC_Institutions_name)) %>% select(ends_with("name"))

# combine matches across datasets
d %<>% mutate(matches = paste0(CIK_name,"-CIK;",
                               CompustatNames_name,"-Computstat;",
                               CreditUnions_name, "-Credit Union;",
                               FDIC_Institutions_name,"-FDIC;",
                               FFIECInstitutions_name,"-FFIEC;",
                               NonProfitTax_name,"-Nonprofit;",
                               OpenSecretsOrgs_name,"-OpenSecrets;",
                               SEC_Institutions_name,"-SEC") %>%
                str_remove_all("NA-.*?;|;NA-SEC") %>%
                str_remove(";NA-SEC"))

d$matches %>% head

# drop non matches
d %<>% filter(matches != "")

# multiple matches
d %>% filter(matches %>% str_detect(";"))

# make org_type and org_name
d %<>% mutate(org_name = matches %>% str_remove("-.*"),
              org_type = matches %>% str_remove(".*-"),
              org_resources = coalesce(`FDIC_Institutions-orgMatch:ASSET`))

# check
d$org_type %>% unique()

match_data_clean <- d

# save comment-level match data
save(match_data_clean, file = here("data", "match_data_clean.Rdata"))
