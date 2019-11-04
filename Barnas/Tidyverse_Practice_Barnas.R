rm(list=ls()) # clear wd

# load packages
library(tidyverse) # includes the following three and other packages
# library (tidyr)
# library (dplyr)
# library (readr)

# read in dataframe and omit "Location" data b/c repetitive in dataframe
Invert <- read_csv("data/MCR_LTER_Annual_Survey_Herbiv_Invert.csv", # use your location of this file
                   col_names = TRUE,
                   col_types=list(Location=col_skip()))
View(Invert)

# View total sightings of each species at Location
Invert %>% 
  filter(Site == "LTER 1",Habitat == "Backreef",Transect == "1",Count > 0) %>% # filters dataset to only work with data within specified columns
  group_by(Taxonomy) %>% # groups remaining data based on the column specified
  summarise(species=n()) # shows new dataframe with the filtered and grouped data with new column "species". n() counts number of observations
  
# View species richness at Location
Invert %>% 
  # filter(Site == "LTER 1",Habitat == "Backreef",Transect == "1") %>%
  group_by(Site,Habitat) %>% # comment out the above line to see richness for all data grouped by site and habitat
  summarise(richness=n_distinct(Taxonomy)) # n_distinct() counts number of unique observations - no repeated species accounted for

# View species abundance at Location
Invert %>% 
  filter(Site == "LTER 1",Habitat == "Backreef",Transect == "1",Count > 0) %>%
  group_by(Taxonomy) %>%
  summarise(sum(Count))      # summarise shows the new dataframe (as before) but with the summations for each species' abundance count

# If we want to transform the data to see the species data across one row for each location variable
WideInvert <- Invert %>%     # store new dataframe as WideInvert
  pivot_wider(               # transform function
  names_from = Taxonomy,     # vector to transform from column to row
  values_from = Count        # values to transform with the above vector from down the column to across the rows
) %>%
  replace(is.na(.),0) # replace all NA values with 0. Takes care of any inconsistencies in data input
View(WideInvert)

# I was just using this to view the no invert observed data
# was useful before we replaced NA's to see off the bat that some values are NA and some values are 0 in this No Invert vector
#WideInvert %>%
  #select("Habitat","No invertebrate observed") # the two column vectors shown in the Console when run


# Create new dataframe DiademaData with specific information "selected" from WideInvert
DiademaData <- WideInvert %>% 
  select("Site","Habitat","Transect",starts_with("Diadema"))

View(DiademaData)
         
# Remove the NA data (if you didn't already replace NA with 0 above)
# Retroactively realized this code actually removes data from both columns if either column has an NA. Not ideal
# Open to suggestions on how to only remove data for when both colums are NA
#DiademaData <- WideInvert %>%
  #select("Site","Habitat","Transect",starts_with("Diadema")) %>%
  #drop_na(starts_with("Diadema")) 

# Rename the column headings to omit spaces
DiademaData <- DiademaData %>%
  rename(D.savi = `Diadema savignyi`,D.seto = `Diadema setosum`)

# Total abundance across all surveys for Diadema setosum
TotalAb <- DiademaData %>%
  select(D.seto)
sum(TotalAb)

# Abundances by habitat
Back <- DiademaData %>%
  filter(Habitat == "Backreef") %>%
  select(D.seto) %>%
  sum()
Back # View the sum

Fringe <- DiademaData %>%
  filter(Habitat == "Fringing") %>%
  select(D.seto) %>%
  sum()

Outer10 <- DiademaData %>%
  filter(Habitat == "Outer 10") %>%
  select(D.seto) %>%
  sum()
  
Outer17 <- DiademaData %>%
  filter(Habitat == "Outer 17") %>%
  select(D.seto) %>%
  sum()

# Getting comfortable with R basics
# To create a new dataframe from separate vectors
xHabitat <- c("backreef","fringe","outer10","outer17")   # Create and fill a new vector with your habitats
d_savi <- vector(mode="double",length=length(xHabitat))  # create new vector of same length as xHabitat for each species
d_seto <- vector(mode="double",length=length(xHabitat)) 
d_seto[1:4]=c(Back,Fringe,Outer10,Outer17)     # assign abundance data to d_seto for each habitat
# The above code simplifies the following four lines
#d_seto[1]=Back 
#d_seto[2]=Fringe
#d_seto[3]=Outer10
#d_seto[4]=Outer17

# create a new matrix by combining your two vectors xHabitat and d_seto
# use cbind again with d_savi data for a full matrix of abundances once d_savi's been assigned values
MyMatrix <- cbind(xHabitat,d_seto) 
View(MyMatrix)

# Create a csv file
write_csv(DiademaData,"data/Diadema_Data.csv")
