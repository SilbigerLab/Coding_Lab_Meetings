rm(list=ls()) #Clears the environment

#load libraries
library(DiagrammeR)
library(rsvg) #to export to a pdf
library(DiagrammeRsvg) #to export to a pdf

#Visit http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html for more options
#Black point DAG showing the interaction terms
Mytilus <- grViz("
digraph boxes_and_circles { 
#this is the type of diagram you are using

# add node statements 
node [shape  = rectangle,
      penwidth = 1.5,
      fontname = Helvetica,
      fontsize = 15]

MytilusLoss[label = 'M. californianus Loss'];NEC; NEP; pH; 
FleshyProducerCover[label = 'Producer Cover']; 
SAtoVRatio[label = 'SA:V']; Light;
TemperatureResiduals[label = 'Temperature Residuals']; 
NtoPRatio[label = 'N:P']; 

#End statements
# color red is significant negative; where as blue is significant positive; 
#grey is nonsignificant (p>0.05)
# size of path represents the standardized coefficients * 5 
# labels are significant standardized coefficients


MytilusLoss-> FleshyProducerCover[color = SteelBlue3, penwidth = 2.8, label = 0.57,fontsize = 13,
fontname = Helvetica, minlen = 2]
MytilusLoss -> Light[color = SteelBlue3, penwidth = 3.1, label = 0.62,fontsize = 13,fontname = Helvetica, minlen = 2]
MytilusLoss -> TemperatureResiduals[color = grey, penwidth = 1.4, minlen =2]
MytilusLoss-> NtoPRatio[color =  OrangeRed2, penwidth = 2.5, label = -0.50,fontsize = 13,fontname = Helvetica, minlen =2]
MytilusLoss -> NEC[color = grey, penwidth = 1.3, minlen =2]
SAtoVRatio-> FleshyProducerCover[color = grey, penwidth = 1.4, minlen =2]
SAtoVRatio->Light[color = grey, penwidth = 0.56, minlen =2]
SAtoVRatio->TemperatureResiduals[color = grey, penwidth = 0.66, minlen =2]
SAtoVRatio-> NtoPRatio[color = OrangeRed2, penwidth = 2.8, label = -0.57,fontsize = 13,fontname = Helvetica, minlen =2]
SAtoVRatio-> NEP[color = grey, penwidth = 1.0, minlen =2]
SAtoVRatio-> pH[color = grey, penwidth = 0.91, minlen =2]
SAtoVRatio-> NEC[color = grey, penwidth = 1.9, minlen =2]
FleshyProducerCover-> NEP[color = SteelBlue3, penwidth =4.6, label = 0.91, fontsize = 13, fontname = Helvetica, minlen =2]
Light -> NEP[color = grey, penwidth = 0.17, minlen =2]
TemperatureResiduals -> NEP[color = grey, penwidth = 0.18, minlen =2]
NtoPRatio-> NEP[color = grey, penwidth = 0.52, minlen =2]
NEP -> pH[color = grey, penwidth = 2.2, minlen =2]
TemperatureResiduals -> NEC[color = grey, penwidth = 0.06, minlen =2]
pH -> NEC[color = grey, penwidth = 1.2, minlen =2]
MytilusLoss->pH[color = SteelBlue3, penwidth = 3.8, label = 'CE 0.76',fontsize = 13, fontname = Helvetica, minlen = 2]
NEP->NEC[color = SteelBlue3, penwidth = 2.9, label = 'CE 0.59', fontsize = 13, fontname = Helvetica,minlen = 2]

# add a graph statement
graph [nodesep = 0.1]
}")
Mytilus

#export image to a PDF
Mytilus %>%
  export_svg %>% 
  charToRaw %>% 
  rsvg_pdf("Output/MytilusDAG.pdf")

Phyllospadix<-
grViz("
digraph boxes_and_circles {

# add node statements
node [shape  = rectangle,
      penwidth = 1.5,
      fontname = Helvetica,
      fontsize = 15]

PhyllospadixLoss[label = 'Phyllospadix spp. Loss'];NEC; NEP; pH; 
FleshyProducerCover[label = 'Producer Cover']; 
SAtoVRatio[label = 'SA:V']; Light;
TemperatureResiduals[label = 'Temperature Residuals']; 
NtoPRatio[label = 'N:P']; 

#End statements
# color red is significant negative; where as black is significant positive; 
#grey is nonsignificant (p>0.05)
# size of path represents the standardized effect size * 5 labels are significant standardized ES

PhyllospadixLoss-> FleshyProducerCover[color = SteelBlue3, penwidth = 3.4, label = 0.68,fontsize = 13, fontname = Helvetica, minlen = 1.5]
PhyllospadixLoss -> Light[color = SteelBlue3, penwidth = 3.3, label = 0.66,fontsize = 13, fontname = Helvetica, minlen = 1.5]
PhyllospadixLoss-> TemperatureResiduals[color = grey, penwidth = 0.89, minlen = 1.5]
PhyllospadixLoss-> NtoPRatio[color = grey, penwidth = 0.15, minlen = 1.5]
FleshyProducerCover-> NEP[color = grey, penwidth =0.24, minlen = 1.5]
Light -> NEP[color = SteelBlue3, penwidth = 2.9, label =0.59, fontsize = 13, fontname = Helvetica, minlen = 1.5]
TemperatureResiduals -> NEP[color = grey, penwidth = 0.93, minlen = 1.5]
NtoPRatio-> NEP[color = grey, penwidth = 0.84, minlen = 1.5]
NEP -> pH[color = SteelBlue3, penwidth = 2.3, label = 0.46,fontsize = 13, fontname = Helvetica, minlen = 1.5]
TemperatureResiduals -> NEC[color = grey, penwidth = 1.1, minlen = 1.5]
pH -> NEC[color = SteelBlue3, penwidth = 5.7, label = 1.1,fontsize = 13, fontname = Helvetica, minlen = 1.5]
SAtoVRatio-> FleshyProducerCover[color = grey, penwidth = 0.8, minlen = 1.5]
SAtoVRatio->Light[color = grey, penwidth = 0.28, minlen = 1.5]
SAtoVRatio->TemperatureResiduals[color = grey, penwidth = 2.8, minlen = 1.5]
SAtoVRatio-> NtoPRatio[color = grey, penwidth = 1.1, minlen = 1.5]
SAtoVRatio-> NEP[color = grey, penwidth = 0.34, minlen = 1]
SAtoVRatio-> pH[color = SteelBlue3, penwidth = 2.3, label = 0.64,fontsize = 13, fontname = Helvetica, minlen = 1.5]
SAtoVRatio-> NEC[color = OrangeRed2, penwidth = 4.3, label = -0.85,fontsize = 13, 
fontname = Helvetica, minlen = 1.5]
# add a graph statement
graph [nodesep = 0.1]
}")
Phyllospadix

#export image to a PDF
Phyllospadix %>%
  export_svg %>% 
  charToRaw %>% 
  rsvg_pdf("Output/PhyllospadixDAG.pdf")
