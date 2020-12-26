'clear all'
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

## Load package
#library(tidyverse)
library(igraph)
library(ggplot2)
library(hrbrthemes)
library(fmsb)
library(maps)
#library(dplyr)
#library(tidyr)
#library(viridis)
library(jpeg)
library(png)

'===== function includes ====='
source("EuGraph.R")
source("EuNames.R")
source("MarketCreation.R")
source("Display.R")

'===== subject ====='
'
Etude de la robustesse du réseau energétique européen,
Adaptation de l offre et de la demande
Répartition des productions et consommations, analyse des flux et surcharges
'

'===== functions definition ====='
'Coloration du graph'
ColorGraph <- function(g,M) {'graph, Matrice infection input'
  colrs <- c("green", "red", "blue")
  V(g)$color <- colrs[M+1]
  #plot(g)
  return(g)
}


#bkgnd <- readJPEG("Europe-countries.jpg")
#plot(1:2, type='n', main="", xlab="x", ylab="y")
#lim <- par()
#rasterImage(bkgnd, 
#            xleft=-1, xright=1, 
#            ybottom=-1, ytop=1)
#grid()

#création d'un graph vide
g_eu <- graph.empty()
g_eu <- EuGraph()
D = Market(1)

Names = EuNames()
L = length(Names)

#affichage graphs

#NormalPlot(g_eu)
#DynamicPlot(g_eu)
#TwinHist(D)
# 1 = Sort by prod, 2 = Sort by demand
#Spider(D,1)
#Donut(D,1)
EuMap(g_eu,D)

'
for (i in 1:Mlr){
  for (j in 1:Mlr){
    if (L_er[[(i-1)*Mlr +j]]<p_er){
      #graph
      if(i != j){
        if (get.edge.ids(g_er,c(i,j)) == 0){
          g_er <- add.edges(g_er, c(i,j))
        }
      }
    }
  }
}
'