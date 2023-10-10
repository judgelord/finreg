# CSVs for simplicity
FDIC_resources <- read_csv(here::here("data", "merged_resources", "FDIC_Institutions.csv"))

FDIC_resources$ASSET <- FDIC_resources$ASSET*1000

#load the full population of campaign donors
creditunions <- read_csv(here::here("Data" , "creditunions.csv"))

#load the full population of campaign donors
opensecrets <- read_csv( here::here("data", "Lobbying_and_Contributions.csv")  )

# drop orgs with no resource data
opensecrets %<>%
  filter(!is.na(MeanContribAmount) | !is.na(MeanLobbyingAmount) | !is.na(TotalLobbyingAmount))

opensecrets %>% skimr::skim()

# all nonprofit data from IRS
nonprofit_resources <- read_csv(here("data", "merged_resources", "nonprofits_resources.csv"))

# load IRS data with c3-c6 variable (this is large)
load(here::here("data", "IRS990-2012-2021.RData"))
# skimr::skim(IRS990_2012_2021)

IRS990_2012_2021 %<>% distinct(EIN, NAME, SUBSECTION, AFFILIATION, .keep_all = T)

IRS990_2012_2021 %>% count(SUBSECTION)

IRS990_2012_2021 %>% count(EIN, sort = T)

IRS990_2012_2021 %<>% distinct(EIN, ORGANIZATION, SUBSECTION) %>%
  mutate(ein = as.double(EIN))

ein_subsection <- IRS990_2012_2021 %>%
  dplyr::select(EIN,SUBSECTION) %>%
  mutate(ein = as.double(EIN)) %>%
  dplyr::select(-EIN) %>%
  distinct() #%>%
#group_by(EIN) %>% tally() %>% arrange(desc(n))

nonprofit_resources %<>% left_join(ein_subsection)

nonprofit_resources %>%
  filter(
    #str_dct(name, "^chamber of commerce$")
    ein == 530045720
  ) %>%
  arrange(-assets) %>%  kablebox()

# CIK org names + market cap from compustat (in cases where the compustat version of the name matched better )
cik <- read_csv(here("data/merged_resources/CIK.csv"))

compustat <- read_csv(here("data/merged_resources/compustat_resources.csv"))

compustat %>% add_count(cik, marketcap) %>% arrange(-n) %>% kablebox()

# CIK org names + market cap from compustat (in cases where the compustat version of the name matched better )
sec <- read_csv(here("data/merged_resources/SEC_Institutions.csv")) %>%
  rename(cik = CIK)


org_lookup <-nonprofit_resources %>% distinct(name) %>% mutate(source = "nonprofit") %>%
  full_join(
    compustat %>% select(name = conm) %>% distinct() %>% mutate(source = "compustat" )
    ) %>%
    full_join(
      creditunions %>% select(name = CU_NAME) %>% distinct() %>% mutate(source = "creditunions" )
    ) %>%
    full_join(
      FDIC_resources %>% select(name = NAME) %>% distinct() %>% mutate(source = "FDIC_resources" )
    ) %>%
    full_join(
      sec %>% select(name = Name) %>% distinct() %>% mutate(source = "sec" )
    ) %>%
    full_join(
      cik %>% select(name = company_name) %>% distinct() %>% mutate(source = "cik" )
    ) %>%
  full_join(
    opensecrets %>% select(name = parentName) %>% distinct() %>% mutate(source = "crp" )
  )

save(org_lookup, file = here::here("data", "org_lookup.Rdata"))

lookup <- function(x){
filter(org_lookup, str_dct(name, x))
}

lookup("markit")





