# Create RSQLite database
library(DBI)
# install.packages("RSQLite")
1
library(RSQLite)
library(tidyverse)
library(magrittr)

# Create RSQLite database
con <- dbConnect(RSQLite::SQLite(), here::here("Data", "master.sqlite") %>% str_remove("finreg"))

# fetch results:
attachments <- dbGetQuery(con, "SELECT * FROM attachments")

head(attachments)
