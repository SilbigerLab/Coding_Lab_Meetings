##Using a correlation matrix to see general relationships with multiple plots between all parameters and data frames for environmental and physiological parameters
#correlation matrix is used to investigate the dependence between multiple variables at the same time. The result is a table containing the correlation coefficients between each variable and the others.

#clear dataframe
rm(list=ls())

##Install packages
# load packages
library(tidyverse)
library(reshape2)
library(corrplot)
library(here)

#set wd
here()

#load in data files for parameters
all.params.data <- read.csv("../data/physio.environ.data.csv") 

#delete unnecessary columns from the new joined data sheet 
#could also use select to not hardcode, using a pipe to select specific environ variables
#or could make a vector with names and then call that to make a data frame
all.params.data <- all.params.data %>%
  dplyr::select(mean.L, mean.T, NH4, N.N, P, N.ST, N.AT, trap.accumulation.rate, N)

#rename various columns in data frame
names(all.params.data)[1] <- "Mean Light Intensity"
names(all.params.data)[2] <- "Mean Temperature"
names(all.params.data)[3] <- "Ammonium (NH4)"
names(all.params.data)[4] <- "Nitrate (NO3) and Nitrite (NO2)"
names(all.params.data)[5] <- "Phosphate (PO4)"
names(all.params.data)[6] <- "Sedimentation Rate"
names(all.params.data)[7] <- "Percent Nitrogen (N)"


#correlation matrix code for all params 
#compute the correltaion matrix (correlation values organized into data frame)
cormat <- round(cor(all.params.data),4)
head(cormat)

#melt the correlation matrix means it reassembles data frame to be more effective to complete corr matrix
#to long format
melted_cormat <- melt(cormat)
head(melted_cormat)

#visulaize the correlation matrix in general
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#save general correlation matrix
ggsave(filename = "../output/gen.correlation.matrix.png", device = "png", width = 20, height = 10)

# Get lower and upper triangle of the correlation matrix
#Note that, a correlation matrix has redundant information. We’ll use the functions below to set half of it to NA
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

#apply upper tri calculation to graphc
upper_tri <- get_upper_tri(cormat)
upper_tri

#melt the correlation matrix
#melt the correlation data and drop the rows with NA values 
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#heatmap of correlation matrix
#negative correlations are in purple color and positive correlations in red
#scale_fill_gradient2 is used with the argument limit = c(-1,1) as correlation coefficients range from -1 to 1
#coord_fixed() : this function ensures that one unit on the x-axis is the same length as one unit on the y-axis
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "midnightblue", high = "firebrick4", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#reorder the correlation matrix according to the correlation coefficient
#useful to identify the hidden pattern in the matrix
#hclust for hierarchical clustering order is used 
#helper function to reorder correlation matrix
reorder_cormat <- function(cormat){
  
  #use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

#reorder the correlation data visualiztaion
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

#melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap with basic characteristics, etc. 
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "darkred", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Print the heatmap
print(ggheatmap)

#add correlation coefficients to the heatmap
#geom_text() to add the correlation coefficients on the graph
#guides() to change the position of the legend title
#if else statement in melted data frame to quotes of black and white to adjust text color 
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18, face="bold", color="black"),
        axis.text.y = element_text(size = 18, face="bold", color="black"),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.text = element_text(size = 20, face="bold", color="black"),
        legend.position = c(0.48, 0.75),
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 12, barheight = 2, 
                               title.position = "top", title.hjust = 0.5, title.vjust = 1.0))

ggsave(filename = "../output/final.correlation.matrix.png", device = "png", width = 10, height = 10)





