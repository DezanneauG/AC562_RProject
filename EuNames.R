# Dplyr for data wrangling and pipe function
library(dplyr)

'===== functions definition ====='
'Europe Countries Names'
EuNames <- function() {
  #Noms des Pays
  Names = c("Finland",
            "Norway",
            "Sweden",
            "Denmark",
            "Germany",
            "Netherlands",
            "Belgium",
            "France",
            "United Kingdom",
            "Ireland",
            "Spain",
            "Portugal",
            "Estonia",
            "Latvia",
            "Lithuania",
            "Poland",
            "Czech Republic",
            "Austria",
            "Switzerland",
            "Italy",
            "Slovenia",
            "Croatia",
            "Bosnia",
            "Serbia",
            "Albania",
            "Greece",
            "Belaruss",
            "Ukraine",
            "Slovakia",
            "Hungary",
            "Romania",
            "Bulgaria",
            "Macedonia",
            "Turkey",
            "Moldova",
            "Russia")
  
  return(Names)
}

'Europe Countries Points'
EuPoints <- function() {
  #Noms des Pays
  Names = EuNames()
  L = length(Names)
  
  Finland <- c(28,65)#
  Norway <- c(14,66)#
  Sweden <- c(-5,50)
  Denmark <- c(-5,50)
  Germany <- c(10,51)#
  Netherlands <- c(6,53)#
  Belgium <- c(5,51)#
  France <- c(2,48)#
  United_Kingdom <- c(-1,52)#
  Ireland <- c(-7,53)#
  Spain <- c(-3,41)#
  Portugal <- c(-8,39)#
  Estonia <- c(-5,50)
  Latvia <- c(-5,50)
  Lithuania <- c(-5,50)
  Poland <- c(-5,50)
  Czech_Republic <- c(-5,50)
  Austria <- c(-5,50)
  Switzerland <- c(-5,50)
  Italy <- c(-5,50)
  Slovenia <- c(-5,50)
  Croatia <- c(-5,50)
  Bosnia <- c(-5,50)
  Serbia <- c(-5,50)
  Albania <- c(-5,50)
  Greece <- c(-5,50)
  Belaruss <- c(-5,50)
  Ukraine <- c(-5,50)
  Slovakia <- c(-5,50)
  Hungary <- c(-5,50)
  Romania <- c(-5,50)
  Bulgaria <- c(-5,50)
  Macedonia <- c(-5,50)
  Turkey <- c(35,39)#
  Moldova <- c(-5,50)
  Russia <- c(0,0)
  
  temp <- rbind(Finland,
        Norway,
        Sweden,
        Denmark,
        Germany,
        Netherlands,
        Belgium,
        France,
        United_Kingdom,
        Ireland,
        Spain,
        Portugal,
        Estonia,
        Latvia,
        Lithuania,
        Poland,
        Czech_Republic,
        Austria,
        Switzerland,
        Italy,
        Slovenia,
        Croatia,
        Bosnia,
        Serbia,
        Albania,
        Greece,
        Belaruss,
        Ukraine,
        Slovakia,
        Hungary,
        Romania,
        Bulgaria,
        Macedonia,
        Turkey,
        Moldova,
        Russia)
  
  #liste
  liste <- list(temp)
  #data.frame
  donnees <- data.frame(liste)
  #nommer les colonnes et les lignes
  colnames(donnees) <- c("long","lat")
  rownames(donnees) <- c(Names)
  return(donnees)
}
