source("code/setup.R")


load(here("data", "match_data_clean.Rdata"))

d <- match_data_clean

org_count <- d %>%
  filter(is_likely_org ==1) %>%
  count(org_name, org_type, org_resources)


# FDIC
FDIC_resources <- read_csv(here("data", "merged_resources", "FDIC_Institutions.csv"))

names(FDIC_resources)

FDIC_resources %<>% dplyr::select(org_name = NAME, STALP, STNAME, BKCLASS, ASSET)

head(FDIC_resources)

FDIC_resources %<>% mutate(org_name = str_to_lower(org_name),
                                commented = org_name %in% org_count$org_name) %>%
  left_join(org_count %>% dplyr::select(org_name, n))

FDIC_resources %>% filter(!is.na(n))

save(FDIC_resources, file = here("data", "FDIC_resources.Rdata"))


# nonprofit data
nonprofit_resources <- read_csv(here("data", "merged_resources", "nonprofits_resources.csv"))

names(nonprofit_resources)

nonprofit_resources %<>% dplyr::select(org_name = name, state, sub_name, subseccd, assets, revenue)

head(nonprofit_resources)

nonprofit_resources %<>% mutate(org_name = str_to_lower(org_name),
                                commented = org_name %in% org_count$org_name) %>%
  left_join(org_count %>% dplyr::select(org_name, n))

nonprofit_resources %>% filter(!is.na(n))

save(nonprofit_resources, file = here("data", "nonprofit_resources.Rdata"))

#

