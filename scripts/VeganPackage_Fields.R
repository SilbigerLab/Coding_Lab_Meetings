#Coding lab meeting 1.22.2020
#Going Vegan!
#--intro to species richness, diversity, & evenness
#--nMDS and permanova test with plot
#Jenn Fields

rm(list=ls()) #Clears the environment

#load libraries
library(vegan)
library(tidyverse)
library(gganimate)

#load data from community comp surveys
#source("scripts/tidepoolphysicalparameters.R")
Sessiles <- read_csv("data/SessilesAll.csv")

#replace NA with 0
Sessiles[is.na(Sessiles)]<-0 

#convert characters to numeric in sessile sheet
Sessiles$Epiactis.prolifera<-as.numeric(Sessiles$Epiactis.prolifera)
Sessiles$Chaetomorpha.linum<-as.numeric(Sessiles$Chaetomorpha.linum)
Sessiles$Costaria.costata<-as.numeric(Sessiles$Costaria.costata)
Sessiles[is.na(Sessiles)]<-0 

# Make all the community data a relative percent
PercentSessile<-100*Sessiles[7:ncol(Sessiles)]/Sessiles$Squares #change to rock--end spp

#normalize to the sum of the total cover (since it can be greater than 100%)
SessileCover<- 100*PercentSessile/rowSums(PercentSessile)

TransformedSessiles<-sqrt(sqrt(SessileCover)) #sqrt data to reduce large #s

#adding back in PoolId, foundation spp, removal/control, and comm comp period to covered sessile data
SessilesAllquad <- data.frame(Sessiles$PoolID, Sessiles$Foundation_spp, Sessiles$Removal_Control, 
                              Sessiles$Before_After,TransformedSessiles)

#take out immediate time period from community comp
SessilesByPoolquad <- SessilesAllquad %>%
  filter(Sessiles.Before_After != "Immediate")

#selecting columns for nMDS and richness and diversity 
Sessilespplist<- SessilesByPoolquad %>%
  select(Diatoms:Stylantheca.spp) #selects columns diatoms-->last spp in datasheet 
#excludes foundation spp and substrate of each pool
Sessilespplist<-as.data.frame(Sessilespplist)

###Species Richness and diversity Data for Sessile Organisms#####

#species richness
SessilesByPoolquad$Richness<-specnumber(Sessilespplist)

#species diversity
SessilesByPoolquad$H<-diversity(Sessilespplist, index = "shannon", MARGIN = 1, base = exp(1))
#Shannon or Shannon–Weaver (or Shannon–Wiener) index is defined as H = -sum p_i log(b) p_i, where p_i is the 
#proportional abundance of species i and b is the base of the logarithm.

#species evenness
SessilesByPoolquad$Evenness<-SessilesByPoolquad$H/(log(specnumber(Sessilespplist))) 
#evenness is the diversity over the log of species richness

#########nMDS of Sessile Data############
#Quadroot Data

#Set seed to be consistent with stocastic calcuations so it will alway calculate it the same say needs 
#to be the same before every run you do
#select species name:speciesname
set.seed(267)
Sessiles2Dquad<-metaMDS(Sessilespplist,k=2, distance='bray', trymax = 50, autotransform = FALSE) #add more iterations
#only selectng spp to incorporate into nMDS plot

#let's look at the 2D stress. Is it < 0.3? 
Sessiles2Dquad$stress #0.1903921

#stress plot for both transformed data
stressplot(Sessiles2Dquad)

# basic plot
ordiplot(Sessiles2Dquad) # dots represent tide pools and 
#+ represents species

# add species names
ordiplot(Sessiles2Dquad, type = 'text')

set.seed(267)
permanovaSessilemodel<-adonis(Sessilespplist~Sessiles.Foundation_spp * Sessiles.Removal_Control * Sessiles.Before_After, SessilesByPoolquad, 
                              permutations = 999, 
                              method="bray")
permanovaSessilemodel
#adonis is Analysis of variance using distance matrices — for partitioning distance matrices among sources of variation and fitting linear models (e.g., factors, polynomial regression) 
#to distance matrices; uses a permutation test with pseudo-F ratios
#method is based of dissimilarity indices, Bray-Curtis is one of the most common, but there are others 
#Gower, Bray–Curtis, Jaccard and Kulczynski indices are good in detecting underlying ecological gradients (Faith et al. 1987)
#?vegdist gives you more descriptions
#P = (#times permutedF > actualF) + 1 / (total number of permutations + 1)


SessilesnMDSpts<-data.frame(Sessiles2Dquad$points)
SessilesnMDS<-cbind(SessilesnMDSpts,SessilesByPoolquad$Sessiles.Foundation_spp,
                    SessilesByPoolquad$Sessiles.Removal_Control,
                    SessilesByPoolquad$Sessiles.Before_After)


#Rename column names
colnames(SessilesnMDS)[3:5]<- c("Foundation_spp","Removal_Control","Before_After")

#make mds column numeric
SessilesnMDS$MDS1<-as.numeric(SessilesnMDS$MDS1)
SessilesnMDS$MDS2<-as.numeric(SessilesnMDS$MDS2)



## add a column combining after before and foundation species
SessilesnMDS$AB_F<-factor(paste(SessilesnMDS$Before_After, SessilesnMDS$Foundation_spp))

#create dataframe for centroids with median from x and y axes
centroids <- aggregate(cbind(MDS1,MDS2)~Foundation_spp*Before_After*Removal_Control*AB_F,SessilesnMDS,median)
#aggregate function Splits the data into subsets, computes summary statistics for each, and returns the 
#result in a convenient form

#for arrows fucnction:
#for arrows fucnction:
x0 <- centroids %>%
  filter(Before_After == "Before") %>%
  select(MDS1)
x0<-as.matrix(x0)

y0 <- centroids %>%
  filter(Before_After == "Before") %>%
  select(MDS2)
y0<-as.matrix(y0)

x1<-centroids %>%
  filter(Before_After == "After") %>%
  select(MDS1)
x1<-as.matrix(x1)

y1<-centroids %>%
  filter(Before_After == "After") %>%
  select(MDS2)
y1<-as.matrix(y1)

#create groupings for shape labels
groupings<-c("After Mytilus" = 1, "After Phyllospadix" = 2, "Before Mytilus" = 16, 
             "Before Phyllospadix" = 17)

SessilesnMDSplot<- ggplot(SessilesnMDS, aes(x = MDS1 , y= MDS2, color = Removal_Control, shape = AB_F, frame = Before_After)) + #basic plot
  geom_point(size = 3, alpha = 0.2) + geom_point(data=centroids,size=7) +
  scale_color_manual(values = c("#3182bd","#bdbdbd")) +
  theme_classic() +
  labs(x ='nMDS1', y = 'nMDS2', shape='Foundation Species', color ='Control or Removal') +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x1[1], yend = y1[1]), #segment with arrow for Mussels before/after control
               arrow = arrow(length = unit(0.3, "cm"),type = "closed")) +
  geom_segment(aes(x = x0[2], y = y0[2], xend = x1[2], yend = y1[2]), #segment with arrow for Mussels before/after removal
               arrow = arrow(length = unit(0.3, "cm"),type = "closed")) +
  geom_segment(aes(x = x0[3], y = y0[3], xend = x1[3], yend = y1[3]), #segment with arrow for phyllospadix before/after control
               arrow = arrow(length = unit(0.3, "cm"),type = "closed")) +
  geom_segment(aes(x = x0[4], y = y0[4], xend = x1[4], yend = y1[4]), #segment with arrow for phyllospadix before/after removal
               arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  theme(legend.text = element_text(size=22, face ="italic"),
        legend.title = element_text(size = 22)) +
  scale_shape_manual(values=c(groupings)) +
  theme(axis.text = element_text(color = "black", size = 18), 
        axis.title.x = element_text(color="black", size=24, face="bold"), 
        axis.title.y = element_text(color="black", size=24, face="bold"), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) 
#ggsave("Output/CentroidmedianSessileplot.pdf",useDingbats = FALSE, width=25, height=22,dpi=300, unit="cm")
gg_animate(SessilesnMDSplot)
