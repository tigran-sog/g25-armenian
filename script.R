library(tidyverse)
library(googlesheets4)

#// This script takes data from the curated Armenian collection table on Google Sheets
#// and produces a set of txt files containing G25 coords for correctly labeled
#// individuals and averages across groups and regions


## Set version of collection
# v1 for 03/12/2024 release
version <- 'v1' 

## Extracting raw coordinates for all individuals
# Connecting to curated Google Sheets table
individuals_raw <- read_sheet('https://docs.google.com/spreadsheets/d/1Fuxwtly8BPgrZgbaVerLk5pa698iILiMpybEfov9NJA') %>%
  mutate(`rawCoord` =  str_extract(`original code`, "(?<=,).*")) %>%
  arrange(id) %>% arrange(group)


## Filtering for individuals with 4 grandparents from the same group only
individuals_eligible <- individuals_raw %>%
  filter(type == 'individual',
         `4 grandparents same group?` == 'y') %>%
  mutate(coord = paste0(group,":",id,",",rawCoord)) %>%
  filter(group != 'Armenian_Yerevan') %>% # remove Armenian_Yerevan for now
  arrange(coord)

writeLines(individuals_eligible$coord, 
           paste("Armenian Collection", version, "individuals.txt"))

## Creating averages for each group
individuals_averages <- individuals_eligible %>%
  separate(rawCoord, into = paste0("PCA_", 1:25), sep = ",", convert = TRUE, remove = FALSE) %>%
  group_by(group) %>% summarise(n = n(), across(starts_with("PCA_"), ~ round(mean(.x, na.rm = TRUE), 6)), .groups = "drop") %>%
  rowwise() %>%
  mutate(coord = paste(paste0(group,"_(n=",n,")"),
                       paste(c_across(starts_with("PCA_")), collapse = ","),
                       sep = ",")) %>%
  ungroup() %>% arrange(coord)

# Exporting averages for all groups
writeLines(individuals_averages$coord,
           paste("Armenian collection", version, "averages all.txt"))

# Exporting averages, excluding single-sample groups
writeLines(individuals_averages %>% filter(n > 1) %>% pull(coord),
           paste("Armenian collection", version, "averages.txt"))

## Creating regional averages
individuals_region <- individuals_raw %>%
  filter(group != 'Armenian_Yerevan') %>% # filtering out Yerevan for now
  filter(type == 'individual',
         `4 grandparents same region?` == 'y',
         is.na(`include into region average`)) %>%
  mutate(regionCoord = paste0("Armenian_",region,":",group,":",id,",",rawCoord)) %>%
  mutate(regionCoordSimple = paste0(region,":",group,":",id,",",rawCoord)) %>%
  arrange(id) %>% arrange(region)

# Table of groups to region for reference
individuals_region %>% distinct(group, .keep_all = TRUE) %>%
  select(region, group) %>% View()

writeLines(individuals_region$regionCoord,
           paste("Armenian collection", version, "region individuals.txt"))

individuals_region_average <- individuals_region %>%
  separate(rawCoord, into = paste0("PCA_", 1:25), sep = ",", convert = TRUE, remove = FALSE) %>%
  group_by(region) %>% summarise(n = n(), across(starts_with("PCA_"), ~ round(mean(.x, na.rm = TRUE), 6)), .groups = "drop") %>%
  rowwise() %>%
  mutate(coord = paste(paste0("Armenian_",region,"_(n=",n,")"),
                       paste(c_across(starts_with("PCA_")), collapse = ","),
                       sep = ",")) %>%
  ungroup() %>% arrange(coord)

writeLines(individuals_region_average$coord,
           paste("Armenian collection", version, "region averages.txt"))

