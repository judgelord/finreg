
# Working directory

if (Sys.info()["user"]=="jello"){
  wd <- "D:/Dropbox/JLW-FINREG-PARTICIPATION/"  #Angelo
}else {
  #wd <- ""
}
setwd(wd)



# Load packages

library(tidyverse)
library(data.table)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data


#OpenSecrets org names
orgs <- read_csv("data/OpenSecretsOrgs.csv")

#OpenSecrets resources

fread("data/opensecrets_bulk/expends12.txt", nrows=2)






# Left join
opensecrets <- left_join(orgs, summaries, by=c("parentName" = "Ultorg"))




# 70% of organizations missing resources data !
nrow(opensecrets %>% filter(is.na(MeanContribAmount)))/nrow(orgs)

# Density plot of MeanContribAmount
opensecrets %>% ggplot() + geom_density(aes(x = log(MeanContribAmount)))






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export ----


write_csv(opensecrets, "data/merged_resources/opensecrets_resources_v2.csv")







