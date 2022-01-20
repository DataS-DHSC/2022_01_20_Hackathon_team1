# LIBRARIES

library(tidyr)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(rstudioapi)
library(stringi)


#----------------------------------------------------------------------------------
# DIRECTORIES
# get path for app where data used in app saved (long-term spend scenarios)
current_path <- getActiveDocumentContext()$path 
# set working directory to this path
setwd(dirname(current_path))
# make sure you are in the right directory
print( getwd() )

setwd(stri_join(dirname(current_path), "/data/output tables"))

merseyside_lsoas <- read.csv("agg_results_merseyside_10km_25km.csv") 

names(merseyside_lsoas)

setwd(stri_join(dirname(current_path), "/data"))

lookup <- read.csv("national_lsoa_carehome_lookup_25km.csv")

merseyside_CQCIDs <- lookup %>% filter(lsoa11cd %in% merseyside_lsoas$lsoa11cd)


n_distinct(merseyside_CQCIDs$cqclocationid)

nrow(merseyside_CQCIDs %>% select(lsoa11cd) %>% unique())
nrow(merseyside_CQCIDs %>% select(cqclocationid) %>% unique())

write.csv(merseyside_CQCIDs, "merseyside_CQCIDs.csv")

