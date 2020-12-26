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
  
  Finland <- c(0,0)
  Norway <- c(0,0)
  Sweden <- c(0,0)
  Denmark <- c(0,0)
  Germany <- c(0,0)
  Netherlands <- c(0,0)
  Belgium <- c(0,0)
  France <- c(2,49)
  United_Kingdom <- c(0,0)
  Ireland <- c(0,0)
  Spain <- c(0,0)
  Portugal <- c(0,0)
  Estonia <- c(0,0)
  Latvia <- c(0,0)
  Lithuania <- c(0,0)
  Poland <- c(0,0)
  Czech_Republic <- c(0,0)
  Austria <- c(0,0)
  Switzerland <- c(0,0)
  Italy <- c(0,0)
  Slovenia <- c(0,0)
  Croatia <- c(0,0)
  Bosnia <- c(0,0)
  Serbia <- c(0,0)
  Albania <- c(0,0)
  Greece <- c(0,0)
  Belaruss <- c(0,0)
  Ukraine <- c(0,0)
  Slovakia <- c(0,0)
  Hungary <- c(0,0)
  Romania <- c(0,0)
  Bulgaria <- c(0,0)
  Macedonia <- c(0,0)
  Turkey <- c(0,0)
  Moldova <- c(0,0)
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
