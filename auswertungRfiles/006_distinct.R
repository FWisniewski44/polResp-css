################################################################################ libraries
library(tidytext)
library(readr)
library(summarytools)
library(rvest)
library(expss)
library(lubridate)
library(stringi)
library(qdap)
library(mgsub)
library(dirichletprocess)
library(igraph)
library(ggraph)
library(esquisse)
library(ggforce)
library(ggh4x)
library(plotly)
library(data.table)
library(tidytable)
library(gridExtra)
library(ggpubr)
library(scales)
library(TSstudio)
library(caret)
library(ggsci)
library(tidyverse)
library(pals)
library(Polychrome)
library(hrbrthemes)
library(ggpmisc)
library(ggfortify)
library(changepoint)
library(strucchange)
library(knitr)
library(kableExtra)
library(DT)
library(flextable)

###

setwd(dir = "~/Documents/uni/masterarbeit/methods/representative-speech/regionalisierung/")
getwd()
gc()

# ==============================================================================================================================================
# ==============================================================================================================================================
# ==============================================================================================================================================

# ziel: duplikate in den daten entfernen
# 1. R -> duplikate entfernen, entweder mittels distinct oder mit einer funktion wie drop_if + unique
# 2. Python -> bag of words neu laufen lassen, um die bereinigten datensätze zu nutzen
# 3. R -> allePolitiker und alleMedien neu updaten lassen und dann analysen weiter durchführen

### zur kontrolle: politiker müssten ~145k sein; bei medien machen die duplikate einen nur verschwindend geringen anteil aus (~1k)

# ==============================================================================================================================================
##################################################################### POLITIKER ################################################################
# ==============================================================================================================================================

# BATCH LOADING: POLITIKERDATEN
filelist_pol_bereinigung <- list.files(path = "./politiker-regionalisiert/",
                           pattern=".csv",
                           full.names = T)

filelistNames_pol_bereinigung <- mgsub(filelist_pol_bereinigung, c("./politiker-regionalisiert//",".csv"), c("",""))

dat_politiker_bereinigung <- list()

for (i in unique(filelist_pol_bereinigung)){
  dat_politiker_bereinigung[[i]] <- readr::read_csv(i)
}

dat_politiker_bereinigung <- dat_politiker_bereinigung %>% purrr::set_names(filelistNames_pol_bereinigung)

allePolitiker_vorher <- reduce(dat_politiker_bereinigung, .f = full_join)

gc()
# View(dat_politiker)

# ==============================================================================================================================================

# for loop über die länge von dat_politiker
# operation -> entfernung duplikate + behalten aller variablen

for(i in 1:length(dat_politiker_bereinigung)){

  dat_politiker_bereinigung[[i]] <- dat_politiker_bereinigung[[i]] %>% distinct(text, .keep_all = T)
}

# reduce zur kontrolle
allePolitiker_bereinigt <- reduce(dat_politiker_bereinigung, .f = full_join)
allePolitiker_bereinigt <- as_tidytable(allePolitiker_bereinigt)
allePolitiker_bereinigt$dateTime <- lubridate::date(allePolitiker_bereinigt$dateTime)

## stimmt besser überein
dim(allePolitiker_vorher)
dim(allePolitiker_bereinigt)

# ==============================================================================================================================================
###################################################################### MEDIEN ##################################################################
# ==============================================================================================================================================

# BATCH LOADING: MEDIENDATEN
filelist_med_bereinigung <- list.files(path = "./demojized/",
                           pattern=".csv",
                           full.names = T)

filelistNames_med_bereinigung <- mgsub(filelist_med_bereinigung, c("./demojized//",".csv"), c("",""))

dat_medien_bereinigt <- list()

for (i in unique(filelist_med_bereinigung)){
  dat_medien_bereinigt[[i]] <- readr::read_csv(i)
}

dat_medien_bereinigt <- dat_medien_bereinigt %>% purrr::set_names(filelistNames_med_bereinigung)

alleMedien_vorher <- reduce(dat_medien_bereinigt, .f = full_join)

gc()
# View(dat_politiker)

# ==============================================================================================================================================

# for loop über die länge von dat_politiker
# operation -> entfernung duplikate + behalten aller variablen

for(i in 1:length(dat_medien_bereinigt)){

  dat_medien_bereinigt[[i]] <- dat_medien_bereinigt[[i]] %>% distinct(text, .keep_all = T)

}

# reduce zur kontrolle
alleMedien_bereinigt <- reduce(dat_medien_bereinigt, .f = full_join)
alleMedien_bereinigt <- as_tidytable(alleMedien_bereinigt)
alleMedien_bereinigt$dateTime <- lubridate::date(alleMedien_bereinigt$dateTime)

## stimmt besser überein
dim(alleMedien_vorher)
dim(alleMedien_bereinigt)

# ==============================================================================================================================================
# speichern der dfs für schritt 2

library(glue)

# POLITIKER
for(i in 1:length(dat_politiker_bereinigung)){

  bezeichnung <- names(dat_politiker_bereinigung[i])
  write_csv(x = dat_politiker_bereinigung[[i]], file = glue::glue("politiker-regionalisiert/{bezeichnung}_bereinigt.csv"))

}

# MEDIEN
for(i in 1:length(dat_medien_bereinigt)){

  bezeichnung <- names(dat_medien_bereinigt[i])
  write_csv(x = dat_medien_bereinigt[[i]], file = glue::glue("demojized/{bezeichnung}_bereinigt.csv"))

}





