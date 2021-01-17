'===== libraries ====='
library(igraph)

source("EuNames.R")

'===== functions definition ====='
'Europe Graph'
EuGraph <- function() {
  #Noms des Pays
  Names = EuNames()
  L = length(Names)
  
  #création d'un graph vide
  g_eu <- graph.empty()
  #ajout de 36 nodes
  g_eu <- add.vertices(g_eu, L)
  #nommage des nodes
  V(g_eu)$name <- Names[1:L]
  #ajout des liens
  g_eu <- g_eu + edge("Germany","Belgium")
  g_eu <- g_eu + edge("Germany","France")
  g_eu <- g_eu + edge("Belgium","France")
  g_eu <- g_eu + edge("Ireland","United Kingdom")
  g_eu <- g_eu + edge("United Kingdom","France")
  
  #passer le graph en non dirigé
  g_eu <- as.undirected(g_eu, mode = c("collapse"))
  
  return(g_eu)
}