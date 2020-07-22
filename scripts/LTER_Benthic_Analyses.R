### Working with large datasets - tidy, analyze, visualize
###  Benthic survey data from the MCR LTER
###  Scripted by Danielle Becker, Jamie Kerlin, and Danielle Barnas
# clear environment
rm(list=ls())

library(tidyverse)
library(vegan)
library(ggplot2)
library(here)

here()


# load data
mcr_data<-read_csv("data/MCR_LTER_Annual_Survey_Benthic_Cover_LTER1.csv")

# group species into taxonomic categories
mcr_grouped <- mcr_data %>% mutate(group = recode(Taxonomy_Substrate_Functional_Group, 'Algal Turf' = "algae", 'Amansia rhodantha' = "algae", 'Actinotrichia fragilis' = "algae", 'Amphiroa fragilissima' = "algae", 'Asparagopsis taxiformis' = "algae", 'Boodlea kaeneana' = "algae", 'Caulerpa racemosa' = "algae", 'Caulerpa serrulata' = "algae", 'Chlorodesmis fastigiata' = "algae", 'Chnoospora implexa' = "algae", 'Cladophoropsis membranacea' = "algae", 'Cyanophyta' = "algae", 'Damselfish Turf' = "algae", 'Dictyosphaeria cavernosa' = "algae", 'Dictyota bartayresiana' = "algae", 'Dictyota friabilis' = "algae", 'Dictyota sp.' = "algae", 'Galaxaura filamentosa' = "algae", 'Galaxaura rugosa' = "algae", 'Gibsmithia hawaiiensis' = "algae", 'Halimeda discoidea' = "algae", 'Halimeda distorta' = "algae", 'Halimeda incrassata' = "algae", 'Halimeda macroloba' = "algae", 'Halimeda minima' = "algae", 'Halimeda opuntia' = "algae", 'Halimeda sp.' = "algae", 'Halimeda taenicola' = "algae", 'Lobophora variegata' = "algae", 'Padina boryana' = "algae", 'Peyssonnelia bornetii' = "algae", 'Peyssonnelia inamoena' = "algae", 'Peyssonnelia sp.' = "algae", 'Ralfsia sp.' = "algae", 'Sargassum pacificum' = "algae", 'Symploca hydnoides' = "algae", 'Turbinaria ornata' = "algae", 'Valonia ventricosa' = "algae")) %>%
  mutate(group = recode(group, 'Coral' = "coral", 'Crustose Corallines' = "cca", 'Sand' = "sand", 'Coral Rubble' = "coral rubble", 'Sponge' = "other inverts", 'No data' = "na", 'Ascidian' = "other inverts", 'Tridacna sp.' = "other inverts", 'Corallimorpharia' = "other inverts", 'Millepora platyphylla' = "coral", 'Bare Space' = "sand"))

# Calculating percent cover of each group for entire habitat for each year (5 transects and 10 quadrats)
mcr_grouped <- mcr_grouped %>%
  group_by(Year, Habitat, group) %>%
  summarize(Percent_Cover=mean(Percent_Cover)) %>%
  ungroup() %>%
  filter(group != "na") # remove na's

# percent cover for all taxonomic groups, wide format
mcr_wide <- mcr_grouped%>%
  pivot_wider(names_from = group,values_from = Percent_Cover)%>%
  replace(is.na(.),0) # replace "na" percent cover with "0" %cover

# excluding Year and  Habitat columns
group_cover <- mcr_wide %>%
  select(-c(Year, Habitat))

## Statistical analysis
##  permanova
##  Independent variables: year and habitat
##  Dependent variables: percent cover of 6 groups

formula<-group_cover~Year*Habitat
stats<-adonis2(formula,data=mcr_wide,method="bray",by="terms")
stats

# Visualization
# nMDS plot

#extract columns with abundance
com <- mcr_wide[,3:ncol(mcr_wide)]

#df to matrix
com_matrix = as.matrix(com)

#run metaMDS, set.seed is to maintain consistency 
set.seed(123)
nmds <- metaMDS(com_matrix, distance = "bray", k=2, trymax = 50, autotransform = FALSE)
nmds

stressplot(nmds)
ordiplot(nmds, type = "text")

#extract NMDS x and y coordinates and put in new data frame
nmds_df <- as.data.frame(scores(nmds))

#add columns from original data frame to new nmds df
nmds_df$year = mcr_wide$Year
nmds_df$habitat = mcr_wide$Habitat

head(nmds_df)


#make year categorical variable
nmds_df2 <- nmds_df %>%
  mutate_at(c("year"), as.factor)


nmds_ggplot <- ggplot(nmds_df, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 6, aes(shape = habitat, color = year)) + 
  scale_color_continuous("year")
theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
      axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
      legend.text = element_text(size = 12, face ="bold", colour ="black"), 
      legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
      axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
      legend.title = element_text(size = 14, colour = "black", face = "bold"), 
      panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
      legend.key=element_blank()) + 
  labs(x = "NMDS1", color = "year", y = "NMDS2", shape = "habitat")

nmds_ggplot
