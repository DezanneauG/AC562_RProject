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
library(dplyr)
library(jpeg)
library(png)
library(RColorBrewer)
library(geosphere)


'===== functions definition ====='
'Sort and Rotate'
SortRotate <- function(donnees,switchValue) {
  #Rotation Dataframe
  Names = EuNames()
  L = length(Names)
  
  if(switchValue == 1){#Sort by production
    donnees = donnees[order(donnees$Production),]
  }else{#Sort by Demand
    donnees = donnees[order(donnees$Demand),]
  }
  
  #Sort Names
  Names = row.names(donnees)
  
  
  #Nouveau dataframe vide
  M <- matrix(c(rep(0,L*2)),2,L)
  #liste
  liste <- list(M)
  #data.frame
  data <- data.frame(liste)
  #nommer les colonnes et les lignes
  rownames(data) <- c("Production","Demand")
  colnames(data) <- c(Names)
  
  for(i in 1:L){
    data["Production",Names[i]] =  donnees[Names[i],"Production"]
    data["Demand",Names[i]] = donnees[Names[i],"Demand"]
    
  }
  return(data)
}

'Normal Plot'
NormalPlot <- function(g_eu) {
  C  = curve_multiple(g_eu, start = 1)
  E(g_eu)$weight <- edge.betweenness(g_eu)
  deg <- degree(g_eu)
  layout <- layout_nicely(g_eu)+7
  
  plot(g_eu, 
       edge.width=2, 
       edge.color = "red", 
       edge.arrow.size = 0,
       vertex.size=7,
       vertex.color="white", 
       layout=layout,
       edge.curved = C)
}

'Dynamic Plot'
DynamicPlot <- function(g_eu) {
  C  = curve_multiple(g_eu, start = 1)
  E(g_eu)$weight <- edge.betweenness(g_eu)
  deg <- degree(g_eu)
  layout <- layout_nicely(g_eu)+7
  
  tkplot(g_eu,
         edge.width=E(g_eu)$weight/10,
         edge.color = "red",
         vertex.color="yellow",
         vertex.size=deg*L/20,
         layout=layout)
}

'Twin Histogram'
TwinHist <- function(data) {
  # Chart
  p <- ggplot(data, aes(x=x) ) +
    geom_histogram( aes(x = var1, y = ..density..), fill="#69b3a2" ) +
    geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
    geom_histogram( aes(x = var2, y = -..density..), fill= "#404080") +
    geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
    theme_ipsum() +
    xlab("value of x")
  
  plot(p)
}

'Spider'
Spider <- function(donnees,switchValue) {
  
  data = SortRotate(donnees,switchValue)
  
  # Echelle du graph
  VMin = floor(min(data))
  VMax = floor(max(data))+1
  data <- rbind(rep(VMax,5) , rep(0,5) , data)
  
  # Color vector
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  
  # plot with default options:
  radarchart( data  , axistype=2 , 
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
              #custom labels
              vlcex=0.8 
  )
  
  # Add a legend
  legend(x=1.3, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
}

'Donut'
Donut <- function(donnees,switchValue) {
  
  myPalette <- brewer.pal(5, "Set2") 
  
  #Sort Dataframe
  Names = EuNames()
  L = length(Names)
  
  if(switchValue == 1){#Sort by production
    donnees = donnees[order(donnees$Production),]
  }else{#Sort by Demand
    donnees = donnees[order(donnees$Demand),]
  }
  
  #Sort Names
  Names = row.names(donnees)
  
  # Compute percentages
  if(switchValue ==1){
    donnees$fraction = donnees$Production / sum(donnees$Production)
  }else{
    donnees$fraction = donnees$Demand / sum(donnees$Demand)
  }
  
  if(switchValue == 1){#Sort by production
    pie(donnees$Production , labels = Names, border="white", col=myPalette)
  }else{#Sort by Demand
    pie(donnees$Demand , labels = Names, border="white", col=myPalette)
  }
  
}

'Map'
EuMap <- function(g,donnees) {
  data <- EuPoints()
  
  # Background map
  map('world',
      col="#b2b2b2", fill=TRUE, bg="white", lwd=0.05,
      mar=rep(0,4),border=0, xlim=c(-10,30), ylim=c(35,73) 
  )
  
  # Dot for cities
  points(x=data$long, y=data$lat, col="slateblue", cex=1, pch=20)
  
  # Compute the connection between Buenos Aires and Paris
  #inter <- gcIntermediate(Paris,  Buenos_aires, n=50, addStartEnd=TRUE, breakAtDateLine=F)
  
  # Show this connection
  #lines(inter, col="slateblue", lwd=2)
  
  # Between Paris and Melbourne
  #inter <- gcIntermediate(Melbourne,  Paris, n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  #lines(inter, col="slateblue", lwd=2)
}
