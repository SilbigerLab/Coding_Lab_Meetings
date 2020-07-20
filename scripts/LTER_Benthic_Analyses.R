# Create new variable "group" for grouping taxonomy
library(tidyverse)
library(vegan)
rm(list=ls())
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

group_cover <- mcr_wide %>%
  select(-c(Year, Habitat))
# Statistical analysis
# permanova
# Independent variables: year and habitat
# Dependent: percent cover of 6 groups

formula<-group_cover~Year*Habitat
stats<-adonis2(formula,data=mcr_wide,method="bray",by="terms")
stats

# Visualization
# nMDS plot

