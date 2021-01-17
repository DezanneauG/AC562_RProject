# Dplyr for data wrangling and pipe function
library(dplyr)

'===== functions definition ====='
'Europe Countries Names'
EuNames <- function() {
  #Noms des Pays
  Names = c("Germany",
            "Belgium",
            "France",
            "United Kingdom",
            "Ireland")
  
  return(Names)
}

'Europe Countries Points'
EuPoints <- function() {
  #Noms des Pays
  Names = EuNames()
  L = length(Names)

  Germany <- c(10,51)#
  Belgium <- c(5,51)#
  France <- c(2,48)#
  United_Kingdom <- c(-1,52)#
  Ireland <- c(-7,53)#
  
  temp <- rbind(
        Germany,
        Belgium,
        France,
        United_Kingdom,
        Ireland)
  
  #liste
  liste <- list(temp)
  #data.frame
  donnees <- data.frame(liste)
  #nommer les colonnes et les lignes
  colnames(donnees) <- c("long","lat")
  rownames(donnees) <- c(Names)
  return(donnees)
}
