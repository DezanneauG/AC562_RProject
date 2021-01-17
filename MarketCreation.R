'===== libraries ====='
library(igraph)

source("EuNames.R")

'===== functions definition ====='
'Market = Offer + Demand'
Market <- function(SwitchValue) {'switch Value to choose the generation type'
  Names = EuNames()
  L = length(Names)
  
  #création à partir de 3 vecteurs de valeurs
  Production <- c(rep(0,L))
  Demand <- c(rep(0,L))
  Export <- c(rep(0,L))
  
  Total = 2000;
  
  if(SwitchValue == 1){'repartition uniforme si Packets est grand'
    Packets = 1000;
    for(i in 1:Packets){
      k = floor(runif(2,1,L+1))
      Production[k[1]] = Production[k[1]] + Total/Packets
      Demand[k[2]] = Demand[k[2]] + Total/Packets
    }
  }else if(SwitchValue == 2){'repartition faite a la main pour verif'
    #ordre : GE BE FR UK IR
    Production <- c(20, 20, 30, 20, 10)
    Demand <- c(10, 20, 10, 30, 30)
  }
  
  for(i in 1:L){
    Export[i] = Production[i] - Demand[i]
  }
  
  #liste
  liste <- list(Production,Demand,Export)
  #data.frame
  donnees <- data.frame(liste)
  #nommer les colonnes et les lignes
  colnames(donnees) <- c("Production","Demand","Export")
  rownames(donnees) <- c(Names)
  
  
  return(donnees)
}

# Immediate Validation Code
# D = Market(1)
# print("Data Frame")
# print(D)
# 
# Names = EuNames()
# L = length(Names)
# SP = 0
# SD = 0
# ST = 0
# X <- c(rep(0,L))
# 
# for(i in 1:L){
#   SP = SP  + D[Names[i],"Production"]
#   SD = SD  + D[Names[i],"Demand"]
#   ST = ST  + D[Names[i],"Export"]
#   
#   X[i] = D[Names[i],"Production"]
# }
# 
# print("Somme Production")
# print(SP)
# 
# print("Somme Demand")
# print(SD)
# 
# print("Somme Export")
# print(ST)
# 
# P(0.0)
# P = ecdf(X)
# plot(P)