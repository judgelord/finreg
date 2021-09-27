
# Working directory

if (Sys.info()["user"]=="jello"){
  wd <- "D:/Dropbox/JLW-FINREG-PARTICIPATION/"  #Angelo
} else if (Sys.info()["user"]=="brianlibgober"){
  wd <- "~/Dropbox/JLW-FINREG-PARTICIPATION/"
} else {
  wd <- getwd()
}
setwd(wd)



# Load packages

library(tidyverse)

library(jsonlite)
library(XML)

library(foreach)
library(doParallel)
registerDoParallel(cores = 2)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data


#OpenSecrets org names
orgs <- read_csv("data/OpenSecretsOrgs.csv")


#OpenSecrets resources: contributions
contributions <- read_csv("data/OpenSecretsOrgLevelPacSummaries.csv")
contributions[,1] <- NULL

#OpenSecrets resources: lobbying 
lobbying = src_sqlite('data/opensecretslobbying.sqlite') %>%
  tbl("lobbying") %>%
  filter(use=='y') %>%
  filter(Year>=2010,Year<2017)  %>%
  group_by(Ultorg) %>%
  summarise(total_lobbying=sum(Amount)) %>%
  collect()

#Merged resources
summaries <- full_join(lobbying, contributions)

# Left join
opensecrets <- left_join(orgs, summaries, by=c("parentName" = "Ultorg"))



# 70% of organizations missing contribution data
nrow(opensecrets %>% filter(is.na(MeanContribAmount)))/nrow(orgs)

# 63% of organizations missing contribution data
nrow(opensecrets %>% filter(is.na(total_lobbying)))/nrow(orgs)


# Density plot of total lobbying
opensecrets %>% ggplot() + geom_density(aes(x=log(total_lobbying)))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alternate Open Secrets data from API


#OpenSecrets API

apikey <- "3ee71ffa72b419a93c49bcd96549345e"

root <- "http://www.opensecrets.org/api/?method=orgSummary&id="
apipre <- "&apikey="
apisuffix <- "&topnumcycle=2012"

# #ex: Acton PAC, D000023748
# id <- "D000023748"


t1 <- Sys.time()
summaries <- NULL
for(id in unique(orgs$parentID)[1:100]){
  tryparse <- try(xmlParse(paste0(root, id, apipre, apikey, apisuffix)))
  if(class(tryparse) != "try-error"){
    cat("\nLoad in API table for ID", id)
    
    orgsummary <- data.frame(
      parentID = id,
      data.frame(
        xmlToList(xmlParse(
          paste0(root, id, apipre, apikey, apisuffix) ))) %>%
        tibble::rownames_to_column('var'))  
    summaries <- rbind(summaries, orgsummary)
  }else{
    cat("\n!!! Cannot load API table for ID", id, "!!!")
  }
}


t2 <- Sys.time()
t2 - t1









# 
# # Parallel version ??
# 
# summaries <-
#   foreach(
#     id %in% unique(orgs$parentID)[1:10],
#     combine = 'rbind') %dopar% {
#       data.frame(
#         parentID = id,
#         xmlToList(xmlParse(
#           paste0(root, id, apipre, apikey, apisuffix) ))) %>%
#         tibble::rownames_to_column('var') %>% rename(value = organization) %>%
#         pivot_wider(names_from = var, values_from = value)
#     }
# 
# 
# 
