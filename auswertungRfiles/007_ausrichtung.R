# INFORMATION:
# diese datei dient dazu, alle zur auswertung notwendigen datensätze herzustellen, in die richtigen formen zu bringen, etc.
# außerdem werden hier namensgebungsaufgaben vorgenommen
# die datei sollte, bevor wieder in eine graphische auswertung gegangen wird, einmal durchlaufen lassen
# alternativ kann diese datei auf ein leeres global environment ausgeführt werden, danach dann einfach das environment speichern
# dann kann dieses wieder neu geladen werden

################################################################################ libraries
library(tidytext)
library(readr)
library(summarytools)
library(rvest)
library(expss)
# library(hunspell)
library(lubridate)
# library(quanteda)
# library(tm)
# library(topicmodels)
# library(stopwords)
library(stringi)
#library(rvisidata)
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

################################################################################

library(knitr)
library(kableExtra)
library(DT)
# library(tm)
# library(topicmodels)
# library(reshape2)
# library(ggplot2)
# library(wordcloud)
# library(pals)
# library(SnowballC)
# library(lda)
# library(ldatuning)
library(flextable)
# activate klippy for copy-to-clipboard button
# klippy::klippy()

# emoji clipping
# library(emoji)
# library(textclean)

################################################################################
################################################################################
################################################################################

setwd(dir = "~/Documents/uni/masterarbeit/scraping/polResp-css/auswertungRfiles/")

# BATCH LOADING: POLITIKERDATEN
filelist_pol <- list.files(path = "./politikerdaten",
                           pattern=".csv",
                           full.names = T)

filelistNames_pol <- mgsub(filelist_pol, c("./politikerdaten/","Kategorisiert.csv"), c("",""))

dat_politiker <- list()

for (i in unique(filelist_pol)){
  dat_politiker[[i]] <- readr::read_csv(i)
}

dat_politiker <- dat_politiker %>% purrr::set_names(filelistNames_pol)
gc()
# View(dat_politiker)

# ## politiker brd; datum = tagesebene
allePolitiker <- reduce(dat_politiker, .f = full_join)
allePolitiker <- as_tidytable(allePolitiker)
allePolitiker$dateTime <- lubridate::date(allePolitiker$dateTime)
dim(allePolitiker)

# nochmal durchführen, da immer noch ~200 tweets aus dem nichts
allePolitiker <- allePolitiker %>% distinct(text, .keep_all = T)

########################################################################

# BATCH LOADING: MEDIENDATEN
filelist_medien <- list.files(path = "./mediendaten",
                              pattern=".csv",
                              full.names = T)

filelistNames_medien <- mgsub(filelist_medien, c("./mediendaten/","Kategorisiert.csv"), c("",""))

dat_medien <- list()

for (i in unique(filelist_medien)){
  dat_medien[[i]] <- readr::read_csv(i)
}

dat_medien <- dat_medien %>% purrr::set_names(filelistNames_medien)
gc()
# View(dat_medien)

alleMedien <- reduce(dat_medien, .f = full_join)
alleMedien <- as_tidytable(alleMedien)
alleMedien$dateTime <- lubridate::date(alleMedien$dateTime)
dim(alleMedien)
dim(distinct(alleMedien, text, .keep_all = T))

# ===============================================================================================================================================
# ===============================================================================================================================================
# ===============================================================================================================================================

# HERSTELLEN DER POLITIKERDATEN ZUR AUSWERTUNG
## SO MUSS ES AM ENDE AUSSEHEN
#
# es kann hier bereits vorgefiltert werden (vgl. partei, bundesland, user, ...)
# es können mehrere dimensionen hinzugefügt werden (hinterer abteil aggregate, nach tilde):
## so z. b. möglich, nicht nur die themen über datum mit immer einem tag referenz aufzusummieren, sondern auch gleich die parteien oder auch die bundesländer mit einzubeziehen
#
#  es kommt dann nur auf die summierung an
#  wichtig: wenn nach ~ noch weitere variablen mit "+" ergänzt werden, dann wird zugleich auch über diese aggregiert
#  hier ist vorsicht geboten, damit der sinn erhalten bleibt!

### für die politiker auf userebene: aggregiert die posts nach thema pro politiker, um zu schauen, wer wozu wie viel tweetet
### wichtig: ohne zeitaspekt!
politikerAnalysedaten_Politiker <- allePolitiker %>%
  as_tidytable(allePolitiker) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user, sum)

### auf parteienebene: aggregiert posts nach themen pro partei
### auch hier KEIN zeitaspekt
politikerAnalysedaten_Parteien <- allePolitiker %>%
  as_tidytable(allePolitiker) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ partei, sum)

### nach datum: aggregiert die posts pro thema pro tag
politikerAnalysedatenThemen <- allePolitiker %>%
  as_tidytable(allePolitiker) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ dateTime, sum)

### versuch des aggregierens pro user pro tag
### GEGLÜCKT: macht sinn, möglichkeit der analyse von daten pro tag pro politiker
politikerAnalysedaten_TopicsUndUser <- allePolitiker %>%
  as_tidytable(allePolitiker) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user + dateTime, sum)

### kombination aus user und parteien
### politi
politikerAnalysedaten_userParteien <- allePolitiker %>%
  as_tidytable(allePolitiker) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user + partei, sum)

### versuch des aggregierens pro partei und pro user am tag
### SINN: somit werden neben den user-tweets pro tag auch deren parteien mit erfasst
politikerAnalysedaten_TopicsUndParteien <- allePolitiker %>%
  as_tidytable(allePolitiker) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ partei + dateTime, sum)

politikerAnalysedaten_TopicsBundesland <- allePolitiker %>%
  as_tidytable(allePolitiker) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ dateTime + bundesland, sum)

### versuch aggregation mit maximalem sinnvollen ausmaß:
### aggregation von posts pro tag (dateTime) pro user (user) aus jeweiliger partei (partei), gegliedert außerdem nach herkunft (bundesland)
politikerAnalysedaten_maximus <- allePolitiker %>%
  as_tidytable(allePolitiker) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ partei + user + bundesland + dateTime, sum)



# ===============================================================================================================================================
# ===============================================================================================================================================
# ===============================================================================================================================================

# ERGÄNZUNG DER BUNDESLÄNDER IN DEN MEDIENDATEN
# für die regionalen medien waren die bundesländer bereits von anfang an mit integriert
# nun sollen diese auch bei den digitalen, den überregionalen und dem ÖRR ergänzt werden
# folgender code erledigt das:

# überregionale:
dat_medien$ÜBERREGIONALE_medien$bundesland <- "Überregional"

# digitale:
dat_medien$DIGITALE_medien$bundesland <- "Überregional"

# ÖRR:
dat_medien$ÖRR_medien$bundesland <- ""
dat_medien$ÖRR_medien <- dat_medien$ÖRR_medien %>%
  mutate(bundesland = case_when(user == "ARTEde" ~ "Überregional",
                                user == "BR_Presse" ~ "Bayern",
                                user == "BR24" ~ "Bayern",
                                user == "DeutscheWelle" ~ "Überregional",
                                user == "DLF" ~ "Überregional",
                                user == "dlfnova" ~ "Überregional",
                                user == "dlfkultur" ~ "Überregional",
                                user == "hessenschau" ~ "Hessen",
                                user == "hrPresse" ~ "Hessen",
                                user == "mdrde" ~ "Überregional",
                                user == "MDRAktuell" ~ "Überregional",
                                user == "MDRpresse" ~ "Überregional",
                                user == "MDR_SN" ~ "Sachsen",
                                user == "MDR_SAN" ~ "Sachsen-Anhalt",
                                user == "mdr_th" ~ "Thüringen",
                                user == "NDRinfo" ~ "Überregional",
                                user == "NDRnds" ~ "Niedersachsen",
                                user == "NDRsh" ~ "Schleswig-Holstein",
                                user == "ndr" ~ "Überregional",
                                user == "butenunbinnen" ~ "Bremen",
                                user == "rbbabendschau" ~ "Überregional",
                                user == "rbb24" ~ "Überregional",
                                user == "rbb24Inforadio" ~ "Überregional",
                                user == "SRKommunikation" ~ "Saarland",
                                user == "SRaktuell" ~ "Saarland",
                                user == "SWRAktuellBW" ~ "Baden-Württemberg",
                                user == "SWRpresse" ~ "Überregional",
                                user == "SWRAktuellRP" ~ "Rheinland-Pfalz",
                                user == "WDR" ~ "Nordrhein-Westfalen",
                                user == "WDRaktuell" ~ "Nordrhein-Westfalen"))

freq(dat_medien$ÖRR_medien$bundesland)
freq(dat_medien$DIGITALE_medien$bundesland)
freq(dat_medien$ÜBERREGIONALE_medien$bundesland)

## alle medien: neues reduce + tidytable, damit die bundesländer im gesamten df drin sind
alleMedien <- reduce(dat_medien, .f = full_join)
alleMedien <- as_tidytable(alleMedien)
alleMedien$dateTime <- as.Date(alleMedien$dateTime)

# kontrolle
freq(alleMedien$bundesland)
alleMedien
dim(alleMedien)


# ===============================================================================================================================================
# HERSTELLEN DER ANALYSEDATEN FÜR ALLE MEDIEN
# orientiert am vorbild der politiker

# ACHTUNG:
# SOLL EINE ANALYSE FÜR EINE BESTIMMTE DISCURSIVE POWER GRUPPE DURCHGEFÜHRT WERDEN?
# wenn das der fall ist, muss diese hier mit vorsortiert werden;
# ein einpflegen der discursive power variablen verfälscht unerklärlicherweise die ergebnisse;
# deswegen: hier vorher schon die akteure filtern, die man haben will und dann neu aggregieren!

## fall 1: aggregation per user, ohne einbezug des datums
medienAnalysedaten_userAggregiert <- alleMedien %>%
  as_tidytable(alleMedien) %>%
  # filter(geschäftsmodell == 1) %>%
  # filter(normenWerte == 1) %>%
  # filter(erreichbarkeit == 1) %>%
  # filter(RVisits > 25000) %>%
  # filter(RVerkauf > 100000) %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "ARTEde") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user, sum)

## fall 2: aggregation mit konzentration auf die themen, auf tagesbasis
medienAnalysedaten_tagesbasis <- alleMedien %>%
  as_tidytable(alleMedien) %>%
  # filter(geschäftsmodell == 1) %>%
  # filter(normenWerte == 1) %>%
  # filter(erreichbarkeit == 1) %>%
  # filter(RVisits > 25000) %>%
  # filter(RVerkauf > 100000) %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "ARTEde") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ dateTime, sum)

## fall 3: aggregation user auf tagesbasis (user + dateTime)
medienAnalysedaten_userTagesbasis <- alleMedien %>%
  as_tidytable(alleMedien) %>%
  # filter(geschäftsmodell == 1) %>%
  # filter(normenWerte == 1) %>%
  # filter(erreichbarkeit == 1) %>%
  # filter(RVisits > 25000) %>%
  # filter(RVerkauf > 100000) %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "ARTEde") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user + dateTime, sum)

## fall 4: aggregation user und herkunft
medienAnalysedaten_userBundesland <- alleMedien %>%
  as_tidytable(alleMedien) %>%
  # filter(geschäftsmodell == 1) %>%
  # filter(normenWerte == 1) %>%
  # filter(erreichbarkeit == 1) %>%
  # filter(RVisits > 25000) %>%
  # filter(RVerkauf > 100000) %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "ARTEde") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user + bundesland, sum)

## fall 5: aggregation user, herkunft und dateTime
medienAnalysedaten_maximus <- alleMedien %>%
  as_tidytable(alleMedien) %>%
  # filter(geschäftsmodell == 1) %>%
  # filter(normenWerte == 1) %>%
  # filter(erreichbarkeit == 1) %>%
  # filter(RVisits > 25000) %>%
  # filter(RVerkauf > 100000) %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "ARTEde") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user + dateTime + bundesland, sum)

## fall 6: rücksicht auf variablen discursive power
## ACHTUNG: es fehlen ca. 3000 tweets in dieser aggregation; discursive power variablen nehmen diese raus, grund unbekannt
medienAnalysedaten_discursivePower <- alleMedien %>%
  as_tidytable(alleMedien) %>%
  # filter(geschäftsmodell == 1) %>%
  # filter(normenWerte == 1) %>%
  # filter(erreichbarkeit == 1) %>%
  # filter(RVisits > 25000) %>%
  # filter(RVerkauf > 100000) %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "ARTEde") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user + dateTime +
              geschäftsmodell + normenWerte + erreichbarkeit, sum)

sum(medienAnalysedaten_tagesbasis$covid)
sum(medienAnalysedaten_userAggregiert$covid)
sum(medienAnalysedaten_maximus$covid)
sum(medienAnalysedaten_discursivePower$covid)

# !!! -> ALT; NUR HIER ALS REFERENZ, ERKLÄRUNG S. O.!
# medienAnalysedatenThemen <- alleMedien %>%
#   as_tidytable(alleMedien) %>%
#   # filter(geschäftsmodell == 1) %>%
#   # filter(normenWerte == 1) %>%
#   # filter(erreichbarkeit == 1) %>%
#   # filter(RVisits > 25000) %>%
#   # filter(RVerkauf > 100000) %>%
#   # filter(bundesland == "Baden-Württemberg") %>%
#   # filter(user == "ARTEde") %>%
#   aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ dateTime, sum)

# ===============================================================================================================================================
# ===============================================================================================================================================
# ===============================================================================================================================================

# UMBENENNUNGEN UND ZWISCHENSPEICHERUNGEN

## hier soll nur festgehalten werden, dass die datensätze mit ihren deklarativen namen für die analysen umbenannt wurden
## dies geschieht je nach dokument
## vgl. 005 -> umbenennung in datensätze, die mit "ts" für timeseries arbeiten
## kürzer, daher leichter verwendbar in der analyse




