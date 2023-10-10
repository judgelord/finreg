source("code/setup.R")
library(tidyverse)

# old data for frs because new matching method did not work so well
d_frs <- here("Data", "finreg_commenter_covariates_df_20230421.csv") %>% read_csv() %>% filter(comment_agency == "FRS")


d_raw <- here(#"data", "match_data", # old in match_data
              #"finreg_commenter_covariates_df_20210824.csv"
              "Data", # new copied from JLW-FINREG-PARTICIPATION/data to FINREGRULEMAKE2/Data
              #"finreg_commenter_covariates_df_20211118.csv"
              #"finreg_commenter_covariates_df_20220405.csv"
              #"finreg_commenter_covariates_df_20220510.csv"
              #"finreg_commenter_covariates_df_20230418.csv"
              #"finreg_commenter_covariates_df_20230421.csv"
              "finreg_commenter_covariates_df_20230427.csv"
              ) %>%
  #str_remove("finreg") %>% # up a level to root
  read_csv()

d_raw %>% filter(comment_agency == "FDIC") %>% select(comment_org_name, comment_url, contains("best_match_name")) %>% distinct() %>%
  add_count(comment_org_name, sort = T) %>% write_csv(here("data", "FDIC-comments.csv"))


#TODO WE NEED COMMENT TITLE BACK IN THE FRS COMMENTS TABLE
#d_raw %>% filter(comment_agency == "FRS") %>% drop_na(`CIK-orgMatch:CIK`) %>% select(contains("name")) %>% kablebox()

d_raw %>% filter(comment_agency == "FRS") %>%
  #drop_na(`CIK-orgMatch:CIK`) %>%
  select(contains("name")) %>% write_csv(here("data", "inspect", "FRS-CIK-matches.csv"))

d_raw %<>%  filter(comment_agency != "FRS")

d_raw %<>% full_join(d_frs)

# remove extraneous column
d_raw %<>%
  select(-`...1`)


# ADD HAND MATCHES
d_raw  %>%
  filter(str_dct(comment_org_name, "faith leader"))



#FIXME best match vs org match only (org match just uses data from org metadata, not also using "submitter" metadata)
d_raw %>% select(contains("bestMatch"))

names(d_raw) <- str_replace_all(names(d_raw), "orgMatch", "bestMatch")

d_raw %>% count(comment_agency, name = "covariates_20230421")

# nonprofits whose names for matching are not their exact names
d_raw %>% filter( `nonprofits_resources-bestMatch:best_match_name` != tolower(`nonprofits_resources-bestMatch:name`)) %>%
  distinct( `nonprofits_resources-bestMatch:best_match_name`, `nonprofits_resources-bestMatch:name`) %>%
  kablebox()

# a bunch of names missing where we have ein because we are only taking high-quality name matches
d_raw %>% filter(comment_agency == "FDIC")%>% drop_na(`nonprofits_resources-bestMatch:ein`)


# credit unions whose names for matching are not their exact names
d_raw %>% filter( `nonprofits_resources-bestMatch:best_match_name` != str_to_lower(`nonprofits_resources-bestMatch:name`,),
                  `CreditUnions-bestMatch:best_match_name` != "NA") %>%
  drop_na(`CreditUnions-bestMatch:best_match_name`) %>%
  distinct(comment_org_name, `CreditUnions-bestMatch:best_match_name`, `nonprofits_resources-bestMatch:best_match_name`, `nonprofits_resources-bestMatch:name`) %>%
  kablebox()

#FIXME ARE OTHER BEST MATCH NAMES NOT GOING TO MATCH IN DATASETS?
# d_raw %<>% mutate(`nonprofits_resources-bestMatch:best_match_name` = str_to_lower(`nonprofits_resources-bestMatch:name`))

# name vars
d_raw %>% select(contains("best_match_name")) %>% names() %>% paste(collapse = "`, `")


# make name variable, coalesce from most to least certian
d_raw %<>% mutate(
  best_match_name_crp = `opensecrets_resources_jwVersion-bestMatch:best_match_name`,
  best_match_name_corp = coalesce(
    `FFIECInstitutions-bestMatch:best_match_name`,
    `FDIC_Institutions-bestMatch:best_match_name`, # FIXME this is prioritized because we have asset data, but it should probably be after
    `SEC_Institutions-bestMatch:best_match_name`,
    `CIK-bestMatch:best_match_name`,
    `compustat_resources-bestMatch:best_match_name`,
  ),
  best_match_name_nonprofit = coalesce(
    `CreditUnions-bestMatch:best_match_name`,
    `nonprofits_resources-bestMatch:best_match_name`,
  ),
  best_match_name = coalesce(best_match_name_corp, best_match_name_nonprofit, best_match_name_crp)
)

# READ IN HAND-VALIDATED MATCHES

hand_match <- read_csv(here::here("data", "hand_match.csv"))

hand_match %<>% mutate(hand_match = tolower(hand_match)) %>%  distinct(comment_org_name, hand_match)

# unique hand matches? CANT HAVE MORE THAN ONE
hand_match %>% count(comment_org_name, sort = T)

hand_match %<>% mutate(hand_match = coalesce(hand_match, "na"))

hand_match %>% filter(hand_match == "na")

# JOIN IN HAND MATCHES
d_raw %<>% left_join(hand_match)

# HOW MANY
d_raw %>% drop_na(hand_match) %>% select(comment_org_name, best_match_name, hand_match)

# TAKE A LOOK AT A FEW MORE
d_raw %>% drop_na(hand_match) %>% select(comment_org_name, best_match_name, hand_match) %>% kablebox()


# REPLACE BEST MATCH WITH HAND MATCH
d_raw %<>% mutate(best_match_name = coalesce(hand_match, best_match_name))

d_raw %>% count(best_match_name, sort = T)
d_raw %>% filter(hand_match == "NA")


# ID unmatched credit unions
unmatched_credit_unions <- d_raw %>% filter(str_detect(comment_org_name, "credit union"),
                 is.na(`CreditUnions-bestMatch:best_match_name`)) %>%
  select(comment_org_name, hand_match, contains("best_match_name")) %>%
  distinct()

unmatched_credit_unions  %>% kablebox()

# save csv of unmatched credit unions
write_csv(unmatched_credit_unions, here("data", "inspect", "unmatched_credit_unions.csv"))

# fix credit unions matched to companies
d_raw %<>% mutate(best_match_name = ifelse(str_detect(comment_org_name, "credit union") & !is.na(best_match_name_nonprofit),
                                          coalesce(best_match_name_nonprofit, best_match_name_crp),
                                          best_match_name))


# EXACT MATCHES
d_raw %>% filter(exact_match_present == 1,
                 best_match_name != comment_org_name,
                 is.na(best_match_name_crp)) %>%
  select(best_match_name, contains("name")) %>%
  distinct() %>%
  kablebox()

# ODD, exact match but no
d_raw  %>% filter(exact_match_present == 1,
                  is.na(best_match_name_crp),
                  is.na(best_match_name_corp),
                  is.na(best_match_name_nonprofit)
                  ) %>%
  #drop_na(best_match_name) %>%
  select(exact_match_present, contains("name")) %>%
  kablebox( )


# ONES WE MAY WANT TO
d_raw  %>% filter(exact_match_present == 1,
                  best_match_name != comment_org_name,
                  is.na(best_match_name_crp)
) %>%
  drop_na(best_match_name) %>%
  select(exact_match_present, best_match_name, contains("name")) %>%
  kablebox( )




# set exact matches to the comment string, unless the exact match is from opensecrets
d_raw %<>% mutate(best_match_name = ifelse(#exact_match_present == 1 & comment_org_name != `opensecrets_resources_jwVersion-bestMatch:best_match_name`,
                                           is.na(hand_match) & exact_match_present == 1 & is.na(best_match_name_crp),
                                           comment_org_name,
                                           best_match_name))


# odd
exact_matches_odd <- d_raw  %>%
  filter(exact_match_present == 1,
         is.na(hand_match),
                  is.na(best_match_name_crp),
                  is.na(best_match_name_corp),
                  is.na(best_match_name_nonprofit)
) %>%
  drop_na(best_match_name)

exact_matches_odd %>%
  select(exact_match_present, contains("name"), comment_url) %>%
  kablebox( )

# how many exact match
d_raw %>% count(is.na(best_match_name))

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


# identify columns we want to keep (there are a lot of columns)
names(d_raw)

d_raw %>% select(contains("name")) %>% names()

d_raw %>% select(contains("tic")) %>% names()

d_raw %>% select(contains("ein")) %>% names()

d_raw %>% select(contains("rssd")) %>% names()

d_raw %>% select(contains("cik")) %>% names()

d_raw %>% select(contains("id")) %>% names()

d_raw %>% select(contains("parent")) %>% names()





# no compustat data
d_raw$`compustat_resources-bestMatch:best_match_name` %>% unique()

# but we have marketcap for CIK matches that came from compustat, so all of these must also be in compustat
d_raw$`CIK-bestMatch:best_match_name` %>% unique()



# INSPECT RSSD ids
rssd <- d_raw %>% group_by(comment_url) %>%
  mutate(rssd_count = c(`CreditUnions-bestMatch:RSSD`,
                                `FDIC_Institutions-bestMatch:FED_RSSD`,
                                `FFIECInstitutions-bestMatch:IDRSSD`) %>%
           unique() %>%
                    length()) %>%
  ungroup() %>%
  select(contains("rssd")) %>% distinct()

matched_multiple_rssd <- rssd %>% filter(rssd_count > 2) %>%
  left_join(d_raw %>% select(contains("name"), contains("rssd")) ) %>%
  select(comment_org_name, best_match_name, contains("name"), contains("rssd") )%>%
  select(-contains("nonprofit")) %>%
  distinct()

matched_multiple_rssd %>% kablebox()

write_csv(matched_multiple_rssd, here("data", "inspect", "matched_multiple_rssd.csv"))


# INSPECT CIK
d_raw %>% group_by(comment_url) %>%
  mutate(cik_count = c(`compustat_resources-bestMatch:cik`,
                        `SEC_Institutions-bestMatch:CIK`,
                        `CIK-bestMatch:CIK`) %>%
           unique() %>%
           length()) %>%
  ungroup() %>%
  select(contains("cik")) %>% distinct()

matched_multiple_cik <- cik %>% filter(cik_count > 2) %>%
  left_join(d_raw %>% select(contains("name"), contains("cik")))  %>%
  select(comment_org_name, best_match_name, contains("name"), contains("cik") ) %>%
  select(-contains("nonprofit")) %>%
  distinct()

matched_multiple_cik %>% kablebox()

write_csv(matched_multiple_cik, here("data", "inspect", "matched_multiple_cik.csv"))

# INSPECT NONPROFITS
d_raw %>% filter(!is.na(`nonprofits_resources-bestMatch:name`)) %>%
  count(`nonprofits_resources-bestMatch:name`,
        `nonprofits_resources-bestMatch:ein`, sort = T) %>%
  kablebox()

d_raw %>% filter(!is.na(`CreditUnions-bestMatch:best_match_name`) ) %>%
                 count(`CreditUnions-bestMatch:RSSD`, sort = T)

d_raw %>%
  filter(!is.na(`compustat_resources-bestMatch:best_match_name`) ) %>%
  count(`compustat_resources-bestMatch:tic`, sort = T) %>%
  kablebox()



# select only best matches
d_best <- d_raw %>% select(hand_match, best_match_name, best_match_name_crp,  best_match_name_nonprofit,
                           contains("orgMatch:best_match_name"),
                           contains("bestMatch:best_match_name"))

names(d_best)

d_best %<>% mutate(across(everything(), is.na))

d_best %<>%
  rownames_to_column("id") %>%
  group_by(id) %>%
  mutate(
  matches = sum(
  #`best_match_name`,
  `CIK-bestMatch:best_match_name`,
  `CreditUnions-bestMatch:best_match_name`,
  `FDIC_Institutions-bestMatch:best_match_name`,
  `FFIECInstitutions-bestMatch:best_match_name`,
  `SEC_Institutions-bestMatch:best_match_name`,
  `compustat_resources-bestMatch:best_match_name`,
  `nonprofits_resources-bestMatch:best_match_name`,
  `opensecrets_resources_jwVersion-bestMatch:best_match_name`))

# how many matches
unique(d_best$matches - 7)

d <- d_raw

d <-  d_raw %>%
  #filter(is_likely_org == 1) %>%
  dplyr::select(
    hand_match,
    # comment vars
    dplyr::starts_with("docket"),
    dplyr::starts_with("comment"),
    # general org vars
    dplyr::contains("best_match_name"),
    #match_in_sample,
    dplyr::starts_with("best_match"),
    exact_match_present,
    # matched data:
    # FDIC
    #`FDIC_Institutions-bestMatch:STNAME`,
    # # CIK
    contains("marketcap"),
    contains("cik"),
    contains("cert"),
    contains("rssd"),
    contains("parent"),
    # `CIK-bestMatch:netinc`, # MISSING
    # SEC
    `SEC_Institutions-bestMatch:Ticker`,
    # # COMPUTSTAT
    # `compustat_resources-bestMatch:netinc`, # MISSING
    `compustat_resources-bestMatch:naics`,
    # # nonprofits
    `nonprofits_resources-bestMatch:ein`
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


# POST HOC FIXES FOCUSING ON FDIC DATA
d %<>%
  mutate(
    # CreditUnions_name = case_when(
    #   str_dct(comment_org_name, "") ~ ""
    #   ),
    nonprofits_resources_name = case_when(
      str_dct(comment_org_name, "richard whiting") ~ "FINANCIAL SERVICES ROUNDTABLE",
      str_dct(comment_org_name, "FINANCIAL SERVICES ROUNDTABLE") ~ "FINANCIAL SERVICES ROUNDTABLE",
      str_dct(comment_org_name, "INDEPENDENT COMMUNITY BANKERS") ~ "INDEPENDENT COMMUNITY BANKERS OF AMERICA",
      str_dct(comment_org_name, "MORTGAGE BANKERS ASSOCIATION") ~ "MORTGAGE BANKERS ASSOCIATION OF AMERICA",
      str_dct(comment_org_name, "INVESTMENT COMPANY INSTITUTE") ~ "INVESTMENT COMPANY INSTITUTE",
      str_dct(comment_org_name, "RURAL UTILITIES COOPERATIVE") ~ "NATIONAL RURAL UTILITIES COOPERATIVE FINANCE CORP",
      str_dct(comment_org_name, "AMERICAN BANKERS ASSOCIATION") ~ "AMERICAN BANKERS ASSOCIATION",
      TRUE ~ nonprofits_resources_name
    ),
    opensecrets_resources_jwVersion_name = case_when(
      str_dct(comment_org_name, "richard whiting") ~ "Financial Services Roundtable",
      str_dct(comment_org_name, "Financial Services Roundtable") ~ "Financial Services Roundtable",
      str_dct(comment_org_name, "Independent Community Bankers") ~ "Independent Community Bankers of America",
      str_dct(comment_org_name, "American Council Life Insurers") ~ "American Council of Life Insurers",
      TRUE ~ opensecrets_resources_jwVersion_name
    ),
    compustat_resources_name = case_when(
      str_dct(comment_org_name, "Wells Fargo Bank") ~ "WELLS FARGO & CO",
      str_dct(comment_org_name, "MARKIT") ~ "MARKIT LTD",
      str_dct(comment_org_name, "IHS MARKIT") ~ "IHS MARKIT LTD",
      str_dct(comment_org_name, "FAIR ISAAC") ~ "FAIR ISAAC CORP",
      str_dct(comment_org_name, "Federated Investors") ~ "FEDERATED INVESTORS INC",
      TRUE ~ compustat_resources_name
    ),
    CIK_name = case_when(
      str_dct(comment_org_name, "FAIR ISAAC") ~ "FAIR ISAAC CORP",
      str_dct(comment_org_name, "MARKIT") ~ "MARKIT LTD.",
      str_dct(comment_org_name, "IHS MARKIT") ~ "IHS MARKIT LTD.",
      str_dct(comment_org_name, "RURAL UTILITIES COOPERATIVE") ~ "NATIONAL RURAL UTILITIES COOPERATIVE FINANCE CORP",
      str_dct(comment_org_name, "Federated Investors") ~ "FEDERATED INVESTORS INC /PA/",
      TRUE ~ CIK_name
    ),
    SEC_Institutions_name = case_when(
      str_dct(comment_org_name, "FAIR ISAAC") ~ "FAIR ISAAC CORP",
      str_dct(comment_org_name, "Federated Investors") ~ "Federated Investors Inc",
      TRUE ~ SEC_Institutions_name
    )
    )

d %<>%
  mutate(
    # CreditUnions_name = case_when(
    #   str_dct(comment_org_name, "") ~ ""
    #   ),
    best_match_name = case_when(
      str_dct(comment_org_name, "richard whiting") ~ "FINANCIAL SERVICES ROUNDTABLE",
      str_dct(comment_org_name, "Wells Fargo Bank") ~ "WELLS FARGO & CO",
      str_dct(comment_org_name, "FINANCIAL SERVICES ROUNDTABLE") ~ "FINANCIAL SERVICES ROUNDTABLE",
      str_dct(comment_org_name, "INDEPENDENT COMMUNITY BANKERS") ~ "INDEPENDENT COMMUNITY BANKERS OF AMERICA",
      str_dct(comment_org_name, "MORTGAGE BANKERS ASSOCIATION") ~ "MORTGAGE BANKERS ASSOCIATION OF AMERICA",
      str_dct(comment_org_name, "INVESTMENT COMPANY INSTITUTE") ~ "INVESTMENT COMPANY INSTITUTE",
      str_dct(comment_org_name, "RURAL UTILITIES COOPERATIVE") ~ "NATIONAL RURAL UTILITIES COOPERATIVE FINANCE CORP",
      str_dct(comment_org_name, "AMERICAN BANKERS ASSOCIATION") ~ "AMERICAN BANKERS ASSOCIATION",
      str_dct(comment_org_name, "richard whiting") ~ "Financial Services Roundtable",
      str_dct(comment_org_name, "Financial Services Roundtable") ~ "Financial Services Roundtable",
      str_dct(comment_org_name, "Independent Community Bankers") ~ "Independent Community Bankers of America",
      str_dct(comment_org_name, "American Council Life Insurers") ~ "American Council of Life Insurers",
      str_dct(comment_org_name, "MARKIT") ~ "MARKIT LTD",
      str_dct(comment_org_name, "IHS MARKIT") ~ "IHS MARKIT LTD",
      str_dct(comment_org_name, "FAIR ISAAC") ~ "FAIR ISAAC CORP",
      str_dct(comment_org_name, "Federated Investors") ~ "FEDERATED INVESTORS INC",
      str_dct(comment_org_name, "FAIR ISAAC") ~ "FAIR ISAAC CORP",
      str_dct(comment_org_name, "MARKIT") ~ "MARKIT LTD.",
      str_dct(comment_org_name, "IHS MARKIT") ~ "IHS MARKIT LTD.",
      str_dct(comment_org_name, "RURAL UTILITIES COOPERATIVE") ~ "NATIONAL RURAL UTILITIES COOPERATIVE FINANCE CORP",
      str_dct(comment_org_name, "Federated Investors") ~ "FEDERATED INVESTORS INC /PA/",
      str_dct(comment_org_name, "FAIR ISAAC") ~ "FAIR ISAAC CORP",
      str_dct(comment_org_name, "Federated Investors") ~ "Federated Investors Inc",
      TRUE ~ best_match_name
    )
  )

d %<>% mutate(across(contains("name"), tolower))


# 2k --> 3k
d %>% filter(!is.na(CreditUnions_name)) %>% select(ends_with("name"))

# 5k ---> 10k
d %>% filter(!is.na(CIK_name)) %>% select(ends_with("name"))

# 1.5 k
d %>% filter(!is.na(compustat_resources_name)) %>% select(ends_with("name"))


# 1k, now 2k ---> 5k
d %>% filter(!is.na(FDIC_Institutions_name)) %>% select(ends_with("name"))


# 0 --> 2.5 k
d %>% filter(!is.na(FFIECInstitutions_name)) %>% select(ends_with("name"))

# 18k, now 20k ---> 12.6k ---> 26k --> 12.6
d %>% filter(!is.na(nonprofits_resources_name)) %>% select(ends_with("name"))


# 4k --> 10 k
d %>% filter(!is.na(opensecrets_resources_jwVersion_name)) %>% select(ends_with("name"))

# 0 --> 4k
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

## ????
d %>% filter(matches == "", is.na(hand_match)) %>% drop_na(best_match_name) %>%  select(exact_match_present, matches, contains("name")) %>%
  kablebox( )

d$matches %>% head()

# drop non matches
# d %<>% filter(matches != "")

# multiple matches
d %>% filter(str_detect(matches, ";"))

# make org_type and org_name
d %<>% mutate(org_name = best_match_name,
              org_type2 = case_when(
                is.na(best_match_name) ~ "NA",
                best_match_name == CreditUnions_name ~ "Credit Union",
                best_match_name %in% c(FDIC_Institutions_name,
                                       FFIECInstitutions_name,
                                       SEC_Institutions_name) ~ "Bank",
                comment_org_name %in% c(FDIC_Institutions_name,
                                       FFIECInstitutions_name,
                                       SEC_Institutions_name) ~ "Bank",
                best_match_name == best_match_name_corp ~ "Other Company",
                comment_org_name == CIK_name ~ "Other Company",
                comment_org_name == compustat_resources_name ~ "Other Company",
                str_detect(comment_org_name, CreditUnions_name) ~ "Credit Union",
                str_detect(CreditUnions_name, comment_org_name) ~ "Credit Union",
                str_detect(best_match_name, CreditUnions_name) ~ "Credit Union",
                str_detect(best_match_name, "credit union") ~ "Credit Union",
                best_match_name == best_match_name_nonprofit ~ "Other Nonprofit",
                comment_org_name == nonprofits_resources_name ~ "Other Nonprofit",
                best_match_name == opensecrets_resources_jwVersion_name ~ "Other PAC Donor"),
              org_type2 = ifelse(org_type2 == "NA", NA, org_type2))

# make org_type and org_name
d %<>% mutate(org_name = best_match_name,
              org_type2 = case_when(
                is.na(best_match_name) ~ "NA",
                best_match_name == CreditUnions_name ~ "Credit Union",
                best_match_name %in% c(FDIC_Institutions_name,
                                       FFIECInstitutions_name,
                                       SEC_Institutions_name) ~ "Bank",
                comment_org_name %in% c(FDIC_Institutions_name,
                                       FFIECInstitutions_name,
                                       SEC_Institutions_name) ~ "Bank",
                best_match_name == best_match_name_corp ~ "Other Company",
                comment_org_name == CIK_name ~ "Other Company",
                comment_org_name == compustat_resources_name ~ "Other Company",
                str_detect(comment_org_name, CreditUnions_name) ~ "Credit Union",
                str_detect(CreditUnions_name, comment_org_name) ~ "Credit Union",
                str_detect(best_match_name, CreditUnions_name) ~ "Credit Union",
                str_detect(best_match_name, "credit union") ~ "Credit Union",
                best_match_name == best_match_name_nonprofit ~ "Other Nonprofit",
                comment_org_name == nonprofits_resources_name ~ "Other Nonprofit",
                best_match_name == opensecrets_resources_jwVersion_name ~ "Other PAC Donor"),
              org_type2 = ifelse(org_type2 == "NA", NA, org_type2))


# edit org_type and org_name based on exact matches
d %<>% mutate(org_type2 = case_when(
                best_match_name %in% c(FDIC_resources$NAME |> tolower(),
                                       sec$Name |> tolower() ) ~ "Bank",
                best_match_name %in% c(compustat$conm |> tolower()) ~ "Other Company",
                best_match_name %in% c(creditunions$CU_NAME |> tolower()) ~ "Credit Union",
                best_match_name %in% c(nonprofit_resources$name |> tolower()) ~ "Other Nonprofit",
                best_match_name %in% c(opensecrets$orgName |> tolower()) ~ "Other PAC Donor",
                TRUE ~ org_type2),
              org_type2 = ifelse(org_type2 == "NA", NA, org_type2))

# edit org_type and org_name based on exact matches
d %<>% mutate(org_type2 = case_when(
                hand_match	 %in% c(FDIC_resources$NAME |> tolower(),
                                       sec$Name |> tolower() ) ~ "Bank",
                hand_match	 %in% c(compustat$conm |> tolower()) ~ "Other Company",
                hand_match	 %in% c(creditunions$CU_NAME |> tolower()) ~ "Credit Union",
                hand_match	 %in% c(nonprofit_resources$name |> tolower()) ~ "Other Nonprofit",
                hand_match	 %in% c(opensecrets$orgName |> tolower()) ~ "Other PAC Donor",
                TRUE ~ org_type2),
              org_type2 = ifelse(org_type2 == "NA", NA, org_type2))

# matched per type / external dataset

d %>% count(org_type2, is.na(best_match_name))

d %>% drop_na(best_match_name) %>% filter(is.na(org_type2)) %>%
  select(hand_match, contains("name"))%>% distinct() %>%
  count(hand_match, sort = T ) %>%
  kablebox()





# check congruence


d %>% count(org_type2)

d %>% count(is.na(org_type2), comment_agency)

# adjust variable name
d %<>% mutate(opensecrets_resources_name = opensecrets_resources_jwVersion_name)

# add a count
d %<>% add_count(comment_org_name, name = "org_comments") %>%
  add_count(best_match_name, name = "best_match_org_comments")




#d$org_resources

d %>% distinct(comment_url) %>% nrow()





# Merge in original organization names
comments <- read_csv(here::here("data", "comment_url_orgs.csv"))

non_orgs <- c("none", "self", "myself", "privat", "private", "person", "trader", "(none)", "broker", "-none-", "[none]", "artist",
              "church", "client", "family", "my own", "no org", "online", "retired", "citizen", "unknown", "realtor", "student",
              "home", "bank", "plan", "retiree", "america", "beehive", "college", "farming", "just me", "manager", "realter", "retierd", "self ..", "traders",
              "n.a.", "nada", "????", "investor", "consumer", "personal","attorney", "american", "congress", "customer",
              "disabled", "invester", "the bank", "a friend", "autonomy",
              "1123", "1967", "1961")

comments %<>% drop_na(organization) %>%
  mutate(organization = str_to_lower(organization) %>% str_squish()) %>%
  filter(nchar(organization) >3,
         !organization %in% non_orgs)

comments %>%  filter(nchar(organization) == 8 ) %>%
  count(organization, sort = T) %>% kablebox()

d %<>% left_join(comments)

match_data_clean <- d %>%
  # filter(!is.na(best_match_type)) %>%
  distinct()

# save comment-level match data
save(match_data_clean, file = here("data", "match_data_clean.Rdata"))


# SNIFF TEST
# TO INVESTIGATE
d_raw %>% filter(str_detect(best_match_name_nonprofit, "petroleum")) %>%
  select(comment_org_name, best_match_name, contains("best_match_name_"), contains("name")) %>% distinct() %>%  kablebox()

# nonprofit_resources %>% filter(str_detect(str_to_lower(name), "petroleum institute"))  %>%   distinct() %>% kablebox()


d_raw %>% filter(str_detect(best_match_name, "jp morgan chase bank")) %>%
  select(comment_org_name, best_match_name, contains("best_match_name_"), contains("name")) %>% distinct() %>%  kablebox()

# nonprofit_resources %>% filter(str_detect(str_to_lower(name), "jp morgan chase bank")) %>% distinct() %>% kablebox()

d_raw %>% filter(str_detect(best_match_name_nonprofit, "yale")) %>%
  select(comment_org_name, best_match_name, contains("best_match_name_"), contains("name")) %>% distinct() %>%  kablebox()

# nonprofit_resources %>% filter(str_detect(str_to_lower(name), "yale (u|l|c)")) %>%  distinct() %>% kablebox()


d %>% filter(comment_agency == "FDIC") %>% drop_na(best_match_name)

d %>% filter(comment_agency == "FDIC") %>% filter(is.na(best_match_name)) %>%
  count(comment_org_name,  sort = T) %>% kablebox()

d %>% filter(comment_agency == "FDIC")%>%
  filter(is.na(best_match_name)) %>%
  drop_na(`nonprofits_resources-bestMatch:ein`)




# NONPROFITS
nonprofit_resources %>% filter(str_dct(name, "Securities Industry and Financial Markets Association")) %>%
  select(name, assets) %>% kablebox()

# IN DATA
d %>% filter(str_dct(best_match_name, "markit")) %>%
  select(hand_match, contains("best_match")) %>% kablebox()

d_raw %>% filter(comment_agency == "FDIC")%>% filter(is.na(best_match_name)) %>% drop_na(`nonprofits_resources-bestMatch:ein`)

