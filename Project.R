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

#création d'un graph vide
g_eu <- graph.empty()
g_eu <- EuGraph()
D = Market(2)

Names = EuNames()
L = length(Names)

#affichage graphs

NormalPlot(g_eu)
#DynamicPlot(g_eu)
#TwinHist(D)
# 1 = Sort by prod, 2 = Sort by demand
#Spider(D,1)
#Donut(D,1)
#EuMap(g_eu,D)

# Calcul des flux
FlowUnit = 1
FlowCount = 0
# Export Sum
ExportSum = 0
for(i in 1:length(D$Export)){
  if(D$Export[i]>0){
    ExportSum = ExportSum +D$Export[i]
  }
}
# Exports actualisés
Act = D
# Matrice d'adjacence
M = as_adjacency_matrix(g_eu)
# Matrice Résultats
T = M # Init selon la matrice d'adjacence
for(i in 1:L){
  for(j in 1:L){
    T[i,j] = 0
  }
}
# Boucle de remplissage
while(FlowCount < ExportSum){
  # Choix du node a traiter
  for(i in 1:L){
    if(Act$Export[i] == max(Act$Export)){
      NomInit = Names[i]
      Index = i
    }
  }
  # Nouvelle data Export (tout égal a 0 sauf NomInit)
  Exp = Act
  for(i in 1:L){
    if(Exp$Export[i] > 0){
      if(i != Index){# tous les nodes positifs  = 0
        Exp$Export[i] = 0
      }else{ # sauf le node etudié
        Exp$Export[i] = FlowUnit
      }
    }
  }
  #print(Exp)
  # Traitement
  # BFS
  BFS1 = bfs(g_eu,NomInit)
  Ordre = BFS1$order$name
  # Sortest Path
  Path = get.shortest.paths(g_eu, NomInit, mode = "all")
  # Node receveur
  Rec = 0
  for(i in 1:L){#parcous de l'ordre du BFS
    if((Rec == 0) && (Exp[Ordre[i],"Export"] < 0)){
      Rec = i
    }
  }
  # quel path 
  for(i in 1:length(Path$vpath)){
    RT = Path$vpath[i]
    if(Ordre[Rec] == RT[[1]]$name[length(RT[[1]]$name)]){
      Path_Index = i
    }
  }
  # ajout du flux sur le path
  RT = Path$vpath[Path_Index]
  for(j in 2:length(RT[[1]]$name)){
    #print(c(RT[[1]]$name[j-1],RT[[1]]$name[j]))
    T[RT[[1]]$name[j-1],RT[[1]]$name[j]] = T[RT[[1]]$name[j-1],RT[[1]]$name[j]] + FlowUnit
  }
  # mise a jour du node
  Act$Export[Index] = Act$Export[Index] - FlowUnit
  #print(Act)
  Act$Export[Rec] = Act$Export[Rec]  + FlowUnit
  FlowCount = FlowCount + FlowUnit
}

# Mise en forme de la matrice T (T = -T^Transpose)
for(i in 1:L){
  for(j in i:L){
    if(T[i,j] != 0){
      T[j,i] = -T[i,j]
    }else if(T[j,i] != 0){
      T[i,j] = -T[j,i]
    }
  }
}
# Affichage
print(T)
# Verification
Verif <- matrix(c(rep(0,L)),L,1)
for(i in 1:L){# lignes
  for(j in 1:L){# Colonnes
    Verif[i] = Verif[i] + T[i,j]
  }
}
#print(D)
#print(Verif)
Flag = "OK"
for(i in 1:L){
  if(D$Export[i] != Verif[i]){
    Flag = "KO"
  }
}
print(Flag)
