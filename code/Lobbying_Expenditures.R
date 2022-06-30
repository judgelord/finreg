Before I start the child care portion of the evening - I have a combined contribution/lobbying dataset here: FINREGRULEMAKE2/finreg/data/Lobbying_and_Contributions.csv

I’ve added the variables:    MeanLobbyingAmount = mean(Amount, na.rm = T), TotalLobbyingAmount = sum(Amount, na.rm = T), and NYearsLobbying = n().  

To get those summary stats I summed by orgName.  This was not a simple choice, as some organizations were bought/sold and changed parents during the study.  For example, 1-800 Contacts was on its own but was then bought by AEA Investors in 2016.  Since in Jacob’s data the org and the parent are always the same

#' Lobbying Data
#' https://www.opensecrets.org/bulk-data/downloads

#' codebook 
#' Industry: https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20lob_indus.htm
#' All CRP: https://www.opensecrets.org/resources/create/data_doc.php

#' can also do by sector via catcodes https://www.opensecrets.org/downloads/crp/CRP_Categories.txt

library(tidyverse)
library(here)
library(ggridges)
library(tidylog)

`%!in%` <- Negate("%in%")

here::i_am("code/Lobbying_Expenditures.R")

industry <- readr::read_delim("data/lob_indus.txt", delim = ",", 
                              quote = "|", col_names = c("parentName", "orgName","Amount","Year","Industry_Code"))

industry %>%
  ungroup() %>%
  skimr::skim()

# Check no billion dollar lobbyists 
industry %>%
  filter(Amount > 10000000) %>%
  arrange(desc(Amount))

# Check no negative lobbing amounts 
industry %>%
  filter(Amount < 0) %>%
  arrange(desc(Amount))

# Plot by year for less than 75th percentile
industry %>%
  filter(Amount < 140000) %>%
  ggplot( aes(x = Amount, y =  fct_rev(as.character(Year)), fill = ..x..) ) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c() +
  labs(title = "Lobbying By Organizations", fill = "Lobbying [$]") + 
  xlab("$ Per Year") +
  ylab("Year") +
  theme_minimal()

# Create summary data to plug into Devin's work

#' in Jacob's data (opensecrets_resources_jwVersion.csv), the organization name (orgName) and 
#' it's parent are the same. This is NOT the case in the lobbying data.  To merge the two, we sum the lobbying data
#' by the organization name.  This means that, for example, we treat 1-800 Contacts as the same
#' before and after it becomes part of AEA Investors

Lobbying_Summary <- industry %>%
  filter(Year >= 2013 & Year <= 2018) %>%
  filter(Amount > 0) %>%
  #group_by(orgName, parentName) %>%
  group_by(orgName) %>%
  dplyr::select(-parentName) %>%
  summarize(
    MeanLobbyingAmount = mean(Amount, na.rm = T),
    TotalLobbyingAmount = sum(Amount, na.rm = T),
    NYearsLobbying = n()
    )

Lobbying_Summary %>%
  filter(orgName == "1-800 Contacts") 

# Check summary for anything that seems off
Lobbying_Summary %>%
  ungroup() %>%
  skimr::skim()

# Now merge with campaign contributions 
JW_Campaign_Contributions <- readr::read_csv("data/merged_resources/opensecrets_resources_jwVersion.csv")

Lobbying_and_Contributions <- JW_Campaign_Contributions %>%
  full_join(Lobbying_Summary, by = "orgName")

write.csv(x = Lobbying_and_Contributions, file = "data/Lobbying_and_Contributions.csv")

######################################################
#
# Show why we group by org name not parent org name
#
#######################################################

Lobbying_Summary <- industry %>%
  filter(Year >= 2010 & Year <= 2018) %>%
  filter(Amount > 0) %>%
  group_by(orgName, parentName) %>%
  #group_by(orgName) %>%
  dplyr::select(-parentName) %>%
  summarize(
    MeanLobbyingAmount = mean(Amount, na.rm = T),
    TotalLobbyingAmount = sum(Amount, na.rm = T),
    NYearsLobbying = n()
  )

# Match orgs with only one parent org on parent org
JW_Same_Parent_Org <- JW_Campaign_Contributions %>%
  group_by(parentName) %>%
  add_tally() %>%
  filter(n == 1) %>% 
  ungroup()

JW_Same_Parent_Org %>%
  anti_join(Lobbying_Summary, by = "parentName")

# Want a dataset where we get orgs with different parents 
JW_Different_Parent_Org <- JW_Campaign_Contributions %>%
  group_by(parentName) %>%
  add_tally() %>%
  filter(n > 1) %>% ungroup()

# Match both organization and parent organization name - so confident about these matches!
Contributions_and_Lobbying <- JW_Same_Parent_Org %>%
  inner_join(Lobbying_Summary, by = c("orgName","parentName")) %>%
  mutate(org_parent_combo = c(paste(orgName, "-", parentName)))

# Contributions no Lobbying
Contributions_No_Lobbying <- JW_Same_Parent_Org %>%
  mutate(org_parent_combo = c(paste(orgName, "-", parentName))) %>%
  filter(org_parent_combo %!in% Contributions_and_Lobbying$org_parent_combo)

# Lobbying no Contributions 
Lobbying_No_Contributions <- Lobbying_Summary %>%
  mutate(org_parent_combo = c(paste(orgName, "-", parentName))) %>%
  filter(org_parent_combo %!in% Contributions_and_Lobbying$org_parent_combo)

sum(Contributions_No_Lobbying$orgName %in% Lobbying_No_Contributions$orgName)
sum(Lobbying_No_Contributions$orgName %in% Contributions_No_Lobbying$orgName)

Contributions_No_Lobbying$orgName[Contributions_No_Lobbying$orgName %in% Lobbying_No_Contributions$orgName]
Lobbying_No_Contributions$orgName[Lobbying_No_Contributions$orgName %in% Contributions_No_Lobbying$orgName]

# The different parentNames are the acquiring companies 
Contributions_No_Lobbying %>% 
  inner_join(Lobbying_No_Contributions, by = "orgName") %>%
  rename(Contributions.parentName = parentName.x, 
         Lobbying.parentName = parentName.y) %>%
  select(orgName, Contributions.parentName, Lobbying.parentName) 


# Look at merges, figure out disconnect

#' Disconnect in JW data and OS data over Parent Company Names.  See e.g., 1800 Contacts
#' issue was that AEA investors took over 1-800 Contacts in 2016 
#' https://www.aeainvestors.com/aea-completes-sale-of-1-800-contacts-2/

JW_Campaign_Contributions %>%
  filter(orgName == "1-800 Contacts") %>%
  dplyr::select(orgName, parentName)

Lobbying_Summary %>%
  filter(orgName == "1-800 Contacts") %>%
  dplyr::select(orgName, parentName)

JW_not_in_Lobbying <- JW_Campaign_Contributions %>%
  tidylog::anti_join(Lobbying_Summary, by = c("parentName"))

Lobbying_not_in_JW <- Lobbying_Summary %>%
  tidylog::anti_join(JW_Campaign_Contributions, by = c("parentName"))

# Check that these random 5 don't have lobbying profiles in OS
set.seed(115)
JW_not_in_Lobbying %>%
  sample_n(5) %>%
  select(orgName, parentID) 

  # mutate(
  #   URL = c(paste0("https://www.opensecrets.org/federal-lobbying/clients/summary?id=", parentID))
  # ) %>%
  # select(URL) %>% 
  # pull()

#' Looking up 5 random misses using https://www.opensecrets.org/federal-lobbying/firms/lobbyists?id=DNUMBERHERE
# orgName                        parentID                                                                        
# <chr>                          <chr>                                                                          
# 1 Cigna Dental                   D000000222 - in Lobbying Data it's Cigna Corp
# 2 Imagine Schools                D000034640 - Only lobbying in 2005, 2006
# 3 Prudential Carruthers Realtors D000045584 - Doesn't exist
# 4 Ergotron Inc                   D000061173 - Doesn't exist
# 5 Fx Direct Dealer LLC           D000051158 - No Lobbying post 2009

Lobbying_Summary  %>%
  filter(grepl(pattern = "Cigna", ignore.case = T, x = orgName))

set.seed(115)
JW_not_in_Lobbying %>%
  filter(!is.na(MeanContribAmount)) %>%
  sample_n(5) %>%
  select(orgName, parentID) #%>%
  # mutate(
  #   URL = c(paste0("https://www.opensecrets.org/federal-lobbying/clients/summary?id=", parentID))
  # )

# orgName                     parentID  
# <chr>                       <chr>     
# 1 Northrop Corp               D000000170 - In Lobbying Data as Northrop Grumman
# 2 Teamsters Local 399         D000000066 - In Lobbying Data as Teamsters
# 3 Honeywell Inc               D000000334 - In Lobbying Data as Honeywell Intelligrated
# 4 Kaiser Foundation Hospitals D000034986 - Not in lobbying data
# 5 GMAC Real Estate            D000034090 - In Lobbying as Ally Financial

Lobbying_Summary  %>%
  filter(grepl(pattern = "Ally", ignore.case = T, x = orgName))

industry  %>%
  filter(grepl(pattern = "Northrop", ignore.case = T, x = orgName)) %>%
  select(parentName) %>%
  distinct()

Lobbying_Summary  %>%
  filter(grepl(pattern = "Northrop Corp", ignore.case = T, x = parentName))

JW_Campaign_Contributions  %>%
  filter(grepl(pattern = "Northrop", ignore.case = T, x = orgName)) %>%
  select(parentName) %>%
  distinct()
