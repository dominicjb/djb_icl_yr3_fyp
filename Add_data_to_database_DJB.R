#Script to add your data
getwd()
setwd("~/Documents/FYP_WP/code")

#Load packages
library(tidyverse)
library(data.table)
library(readxl)

#Read in OG data 
BEF_DAT <- read_csv("../data/BEF_DATA_COMBINED_NOT STANDARDISED_3col_removed.csv",
                    col_types = cols(GPS_latitude_centre = col_number(),
                                     GPS_longitude_centre = col_number(),
                                     Biodiversity_value_x = col_number(),
                                     Ecosystem_function_value_y = col_number()))


#Function to take in excel spreadsheet and return dataframe
read.ra.data <- function(filename){
  
  #read in data
  data <- read_excel(filename, sheet=2)
  metadata <- read_excel(filename, sheet=1)
  
  #rename columns data
  data<- data %>% rename(DOI = DOI_linked, 
                         Location = `Location(if multiple)_linked`, 
                         Taxon = `Taxon(if multiple)_linked` ,
                         Ecosystem_function_y_axis_description = `Ecosystem_function_y_axis_description (and ledgend description if relevant and seperated by anything other than location - temp treat/drought/ control/ect)`
  ) 
  
  #rename columns metadata
  metadata<- metadata %>% rename(Location = `Location description (locale, city/state, country)`, 
                                 Taxon = Study_common_taxon,
                                 spatial_grain_m2 = `spatial_grain (size of the unit of measurement (i.e quadrat size of 1m^2)`
  ) 
  
  data <- data %>% 
    mutate_at(vars(DOI, Location, Taxon), na_if, y = "") %>% 
    fill(DOI, Location, Taxon)
  
  metadata <- metadata %>% 
    mutate_at(vars(Initials), na_if, y = "") %>% 
    fill(Initials)
  
  #merge
  merged_data <- left_join(metadata, data, by = c("DOI","Location","Taxon")) 
  
  return(merged_data)
}

#Run function for your excel data
data2add<-read.ra.data("../data/DJB_collected_data_no_notes.xlsx")

# I added this to move around columns
data2add_2 <- data2add %>% relocate(21, .after=1)
data2add_3 <- data2add_2 %>% relocate(21, .after=1)
data2add_4 <- data2add_3 %>% relocate(1, .after=3)
data2add_5 <- data2add_4 %>% select(-Notes)
#

#Combine your data with the OG data
BEF_DAT_combined <- rbind(BEF_DAT, data2add)

####################################
###Data manipulation###

BEF_DAT<-data_combined
#remove any strange characters in location
BEF_DAT_combined$Location <- gsub("[[:punct:]]","",as.character(BEF_DAT_combined$Location))

#add group numbers/ labels
BEF_DAT_combined <- setDT(BEF_DAT_combined)[,label:=.GRP, by = c("DOI", "Location", "Taxon", "Biodiversity_x_axis_description","Ecosystem_function_y_axis_description", "Biodiversity_metric")]

###################################
# Drop labels where we have no/little data * >5
###################################
entries <- table(BEF_DAT_combined$label)
entries <- entries[entries > 5]
BEF_DAT_combined <- BEF_DAT_combined[BEF_DAT_combined$label %in% names(entries),]

#biodiv/ eco funt. needs to have at least 3 distinct values
BEF_DAT_combined <- BEF_DAT_combined %>%
  group_by(label) %>%
  mutate(count = n_distinct(Biodiversity_value_x)) %>% 
  filter(count !=2) %>% 
  filter(count !=1) %>% 
  select(-count)

BEF_DAT_combined <- BEF_DAT_combined %>%
  group_by(label) %>%
  mutate(count = n_distinct(Ecosystem_function_value_y)) %>% 
  filter(count !=2) %>% 
  filter(count !=1) %>% 
  select(-count)

BEF_DAT_combined = subset(BEF_DAT_combined, !is.na(Ecosystem_function_value_y))
BEF_DAT_combined = subset(BEF_DAT_combined, !is.na(Biodiversity_value_x))


##############################################
#standardise the data within a study & factor
new.scale <- function(x){
  x <- as.numeric(scale(x))
  x <- x - min(x)
  if(any(x < 0))
    stop("You got negatives! What?!")
  return(x)
}


BEF_DAT_combined <- BEF_DAT_combined %>% 
  group_by(label) %>% 
  mutate_at(c("Biodiversity_value_x", "Ecosystem_function_value_y"), ~(new.scale(.) %>%
                                                                         as.vector))


###########################
# SAVE COMBINED DATA FILE #
###########################
write.csv(BEF_DAT_combined, "DJB_BEF_DATA_COMBINED_standardised.csv") 
