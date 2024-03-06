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
library(pals)
library(Polychrome)
library(hrbrthemes)
library(ggpmisc)
library(ggfortify)
library(changepoint)
library(strucchange)
library(patchwork)

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

# umwandlung von "südschleswigscher wählerverband" ins kürzel
allePolitiker$partei[allePolitiker$partei == "Südschleswigscher Wählerverband"] <- "SSW"

# check?
freq(allePolitiker$partei)

# passen dimensionen?
dim(allePolitiker)

########################################################################

# BATCH LOADING: MEDIENDATEN
filelist_medien <- list.files(path = "./mediendaten",
                              pattern=".csv",
                              full.names = T)

# filelist_medien_regionale <- list.files(path = "./mediendaten_reg",
#                                            pattern = ".csv",
#                                            full.names = T)

filelistNames_medien <- mgsub(filelist_medien, c("./mediendaten/","Kategorisiert.csv"), c("",""))

# filelistNames_medien_regionale <- mgsub(filelist_medien, c("./mediendaten/","Kategorisiert.csv"), c("",""))

dat_medien <- list()

# dat_medien_regionale <- list()

for (i in unique(filelist_medien)){
  dat_medien[[i]] <- readr::read_csv(i)
}

# for (i in unique(filelist_medien_regionale)){
#   dat_medien_regionale[[i]] <- readr::read_csv(i)
# }

dat_medien <- dat_medien %>% purrr::set_names(filelistNames_medien)
# dat_medien_regionale <- dat_medien_regionale %>% purrr::set_names(filelistNames_medien_regionale)
gc()
# View(dat_medien)

# nurRegionaleMedien <- reduce(dat_medien_regionale, .f = full_join)
# nurRegionaleMedien <- as_tidytable(nurRegionaleMedien)
# nurRegionaleMedien$dateTime <- lubridate::date(nurRegionaleMedien$dateTime)
# dim(nurRegionaleMedien)
# dim(distinct(nurRegionaleMedien, text, .keep_all = T))

# überregionale <- lubridate::date(überregionale$dateTime)
# join1 <- full_join(nurRegionaleMedien, überregionale)
# join1$dateTime <- lubridate::date(join1$dateTime)
# ÖRR$dateTime <- lubridate::date(ÖRR$dateTime)
# digitale$dateTime <- lubridate::date(digitale$dateTime)
#
# überJoin1 <- full_join(überregionale, ÖRR)
# überJoin2 <- full_join(überJoin1, digitale)
#
# join2 <- full_join(join1, digitale)
# join3 <- full_join(join2, ÖRR)
#
# # zeigt sich, dass nach bereinigung + datumsebene "tag" immer 1.372.113 tweets vorliegen
# join3 <- join3 %>% distinct(text, .keep_all = T)

alleMedien <- reduce(dat_medien, .f = full_join)
alleMedien <- as_tidytable(alleMedien)
alleMedien$dateTime <- lubridate::date(alleMedien$dateTime)
dim(alleMedien)
dim(distinct(alleMedien, text, .keep_all = T))

# letzte bereinigung mediendaten (wahrscheinlich in überregionalen noch was drin)
alleMedien <- alleMedien %>% distinct(text, .keep_all = T)

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
dim(distinct(alleMedien, text, .keep_all = T))

# ersetzen: normen auf marktgetrieben statt 4/NA bei mucbook
alleMedien$normenWerte[alleMedien$user == "mucbook"] <- 3
# View(alleMedien %>% filter(user == "mucbook"))

# ersetzen: normen abendblatt auf objektiv statt parteiisch
alleMedien$normenWerte[alleMedien$user == "abendblatt"] <- 1
# View(alleMedien %>% filter(user == "abendblatt"))

# ersetzen: alle drei für stuttgarter zeitung
alleMedien$normenWerte[alleMedien$user == "StZ_NEWS"] <- 1
alleMedien$geschäftsmodell[alleMedien$user == "StZ_NEWS"] <- 2
alleMedien$erreichbarkeit[alleMedien$user == "StZ_NEWS"] <- 2
# View(alleMedien %>% filter(user == "StZ_NEWS"))

# ersetzen: alle drei für nowy casnik
alleMedien$normenWerte[alleMedien$user == "NCasnik"] <- 1
alleMedien$geschäftsmodell[alleMedien$user == "NCasnik"] <- 2
alleMedien$erreichbarkeit[alleMedien$user == "NCasnik"] <- 3
# View(alleMedien %>% filter(user == "NCasnik"))

# ersetzen: alle drei für compact magazin
alleMedien$normenWerte[alleMedien$user == "COMPACTMagazin"] <- 2
alleMedien$geschäftsmodell[alleMedien$user == "COMPACTMagazin"] <- 2
alleMedien$erreichbarkeit[alleMedien$user == "COMPACTMagazin"] <- 3
# View(alleMedien %>% filter(user == "COMPACTMagazin"))

# ersetzen: alle drei für rbb24
alleMedien$normenWerte[alleMedien$user == "rbb24"] <- 1
alleMedien$geschäftsmodell[alleMedien$user == "rbb24"] <- 1
alleMedien$erreichbarkeit[alleMedien$user == "rbb24"] <- 2
# View(alleMedien %>% filter(user == "rbb24"))

# ersetzen: alle drei für rbb24 inforadio
alleMedien$normenWerte[alleMedien$user == "rbb24Inforadio"] <- 1
alleMedien$geschäftsmodell[alleMedien$user == "rbb24Inforadio"] <- 1
alleMedien$erreichbarkeit[alleMedien$user == "rbb24Inforadio"] <- 2
# View(alleMedien %>% filter(user == "rbb24Inforadio"))

# ersetzen: alle drei für spiegel top news
alleMedien$normenWerte[alleMedien$user == "SPIEGEL_Top"] <- 1
alleMedien$geschäftsmodell[alleMedien$user == "SPIEGEL_Top"] <- 2
alleMedien$erreichbarkeit[alleMedien$user == "SPIEGEL_Top"] <- 1
# View(alleMedien %>% filter(user == "SPIEGEL_Top"))

# ersetzen: geschäftsmodell für jreichelt
alleMedien$geschäftsmodell[alleMedien$user == "jreichelt"] <- 3
# View(alleMedien %>% filter(user == "jreichelt"))

# ersetzen: geschäftsmodell für pressenza
alleMedien$geschäftsmodell[alleMedien$user == "pressenza_ger"] <- 3
# View(alleMedien %>% filter(user == "pressenza_ger"))

# ersetzen: erreichbarkeit für pressenza
alleMedien$erreichbarkeit[alleMedien$user == "boersenzeitung"] <- 1
# View(alleMedien %>% filter(user == "boersenzeitung"))

#### unten: das nicht machen, zerschießt alles!
# alleMedien <- alleMedien %>% mutate(normenWerte = case_when(user == "SPIEGEL_Top" ~ 1, .default = normenWerte))
# alleMedien <- alleMedien %>% mutate(erreichbarkeit = case_when(user == "SPIEGEL_Top" ~ 1, .default = normenWerte))
# alleMedien <- alleMedien %>% mutate(geschäftsmodell = case_when(user == "SPIEGEL_Top" ~ 2, .default = normenWerte))

# check
nonassigned <- (alleMedien[is.na(alleMedien$erreichbarkeit),]) # alle rows mit NA
freq(nonassigned$user)

alleMedien <- alleMedien %>% distinct(text, .keep_all = T)
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
(medienAnalysedaten_discursivePower <- alleMedien %>%
  as_tidytable(alleMedien) %>%
  # filter(geschäftsmodell == 1) %>%
  # filter(normenWerte == 1) %>%
  # filter(erreichbarkeit == 1) %>%
  # filter(RVisits > 25000) %>%
  # filter(RVerkauf > 100000) %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "ARTEde") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ user + bundesland + geschäftsmodell + normenWerte + erreichbarkeit, sum))

## fall 6: rücksicht auf variablen discursive power
(medienAnalysedaten_discursivePower_days <- alleMedien %>%
    as_tidytable(alleMedien) %>%
    # filter(geschäftsmodell == 1) %>%
    # filter(normenWerte == 1) %>%
    # filter(erreichbarkeit == 1) %>%
    # filter(RVisits > 25000) %>%
    # filter(RVerkauf > 100000) %>%
    # filter(bundesland == "Baden-Württemberg") %>%
    # filter(user == "ARTEde") %>%
    aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ dateTime + user + bundesland + geschäftsmodell + normenWerte + erreichbarkeit, sum))

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

# POLITIKER
# timePol <- politikerAnalysedaten
timePol_topics <- politikerAnalysedatenThemen
timePol_party <- politikerAnalysedaten_Parteien
timePol_userParty <- politikerAnalysedaten_userParteien
timePol_topicsParty <- politikerAnalysedaten_TopicsUndParteien
timePol_topicsUser <- politikerAnalysedaten_TopicsUndUser
timePol_maximus <- politikerAnalysedaten_maximus

# MEDIEN
timeMedia <- medienAnalysedaten_userAggregiert
timeMedia_topics <- medienAnalysedaten_tagesbasis
timeMedia_topicsUser <- medienAnalysedaten_userTagesbasis
timeMedia_userBundesland <- medienAnalysedaten_userBundesland
timeMedia_maximus <- medienAnalysedaten_maximus
timeMedia_discursivePower <- medienAnalysedaten_discursivePower
timeMedia_discPow_days <- medienAnalysedaten_discursivePower_days

# ===============================================================================================================================================
# ===============================================================================================================================================
# ===============================================================================================================================================

# ========================================================================================================================================================
# ANALYSEN ZUR REGIONALITÄT
# ========================================================================================================================================================

# richtiger datensatz mit ost und west teilung erstellen
# in diesem fall muss überlegt werden, was mit denen passiert, die überregional sind
# am schlauesten ist wahrscheinlich wegzulassen
# überregional sagt ja auch aus: das ist nicht gebunden
ostdeutschlandListe <- c("Thüringen", "Sachsen", "Sachsen-Anhalt", "Brandenburg", "Mecklenburg-Vorpommern")
westdeutschland <- c("Baden-Württemberg", "Bayern", "Hessen", "Rheinland-Pfalz", "Nordrhein-Westfalen",
                     "Niedersachsen", "Schleswig-Holstein", "Saarland", "Bremen", "Hamburg", "Berlin")

# politiker und mediendaten je nach zuordnung zu ost und west
politikOst <- timePol_maximus %>% filter(bundesland %in% ostdeutschlandListe)
medienOst <- timeMedia_discursivePower %>% filter(bundesland %in% ostdeutschlandListe)
#
politikWest <- timePol_maximus %>% filter(bundesland %in% westdeutschland)
medienWest <- timeMedia_discursivePower %>% filter(bundesland %in% westdeutschland)

# =========================================================================================================================================================
# themenüberblickPolitiker <- data.frame(apply(X = timePol_maximus[,c(5:19)], MARGIN = 2, FUN = sum)) # gleiches ergebnis, mehr text

# themenüberblick mittels apply und summierung tweets pro thema
themenüberblickPolitiker <- as_tidytable(apply(X = timePol_topics[,-c(1, 17:20)], MARGIN = 2, FUN = sum), .keep_rownames = T)
themenüberblickPolitiker <- themenüberblickPolitiker %>% rename(names = rn, anzahl = x)

# ansicht
themenüberblickPolitiker

# tweets politiker insgesamt
sum(themenüberblickPolitiker$anzahl)

# alternativ auch das möglich
# themenüberblickPolitiker <- data.frame(apply(X = politikerAnalysedatenThemen[,-1], MARGIN = 2, FUN = sum))

# =========================================================================================================================================================

# BILDUNG SUMME TWEETS PRO THEMA: mediendaten
# unterschiedliche zahlen bei discpow datensatz vs. tagesbasis; hier nutzung der tagessummierung
# themenüberblickMedien <- data.frame(apply(X = medienAnalysedaten_discursivePower[,c(6:20)], MARGIN = 2, FUN = sum)) ## NICHT NUTZEN

themenüberblickMedien <- as_tidytable(apply(X = medienAnalysedaten_tagesbasis[,-1], MARGIN = 2, FUN = sum), .keep_rownames = T)
themenüberblickMedien <- themenüberblickMedien %>% rename(names = rn, anzahl = x)

# tidytable + ansicht
themenüberblickMedien

# tweets medien insgesamt
sum(themenüberblickMedien$anzahl)

# =========================================================================================================================================================

# variablennamen anpassen
themenüberblickMedien$names2 <- c("Covid", "Ukraine", "Energie", "Soziales", "Verteidigungspolitik", "PolizistenmordKusel", "FlutAhrtal",
                                  "PolitikEuropa", "PolitikInternational", "Klima", "ProtesteIran", "Verkehr",
                                  "PluralismusMedien", "Zukunft", "Verfassungsfeindlich")
themenüberblickPolitiker$names2 <- c("Covid", "Ukraine", "Energie", "Soziales", "Verteidigungspolitik", "PolizistenmordKusel", "FlutAhrtal",
                                     "PolitikEuropa", "PolitikInternational", "Klima", "ProtesteIran", "Verkehr",
                                     "PluralismusMedien", "Zukunft", "Verfassungsfeindlich")

# factor reorder absteigend
themenüberblickMedien <- themenüberblickMedien %>% mutate(names2 = fct_reorder(names2, desc(-anzahl)))
# factor reorder absteigend
themenüberblickPolitiker <- themenüberblickPolitiker %>% mutate(names2 = fct_reorder(names2, desc(-anzahl)))

# =========================================================================================================================================================

überblickPolOst <- as_tidytable(apply(X=politikOst[,c(5:19)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickPolOst <- überblickPolOst %>% rename(names = rn, anzahl = x)

überblickMedienOst <- as_tidytable(apply(X = medienOst[,c(6:20)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickMedienOst <- überblickMedienOst %>% rename(names = rn, anzahl = x)

überblickPolWest <- as_tidytable(apply(X = politikWest[,c(5:19)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickPolWest <- überblickPolWest %>% rename(names = rn, anzahl = x)

überblickMedienWest <- as_tidytable(apply(X = medienWest[,c(6:20)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickMedienWest <- überblickMedienWest %>% rename(names = rn, anzahl = x)

###

sum(überblickMedienOst$anzahl)
sum(überblickPolOst$anzahl)
sum(überblickMedienWest$anzahl)
sum(überblickPolWest$anzahl)

#===============================================================================

# normalisierung auf anteilig 100%
proportionalOst_politiker <- as_tidytable(apply(überblickPolOst[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalOst_politiker$names <- überblickPolOst$names

proportionalWest_politiker <- as_tidytable(apply(überblickPolWest[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalWest_politiker$names <- überblickPolWest$names

proportionalOst_medien <- as_tidytable(apply(überblickMedienOst[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalOst_medien$names <- überblickMedienOst$names

proportionalWest_medien <- as_tidytable(apply(überblickMedienWest[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalWest_medien$names <- überblickMedienWest$names

#===============================================================================

# min max normalisierung
processPolOst <- preProcess(as_tidytable(überblickPolOst), method = "range")
überblickPolOst_minmax <- predict(processPolOst, as_tidytable(überblickPolOst))

processPolWest <- preProcess(as_tidytable(überblickPolWest), method = "range")
überblickPolWest_minmax <- predict(processPolWest, as_tidytable(überblickPolWest))

processMedienOst <- preProcess(as_tidytable(überblickMedienOst), method = "range")
überblickMedienOst_minmax <- predict(processMedienOst, as_tidytable(überblickMedienOst))

processMedienWest <- preProcess(as_tidytable(überblickMedienWest), method = "range")
überblickMedienWest_minmax <- predict(processMedienWest, as_tidytable(überblickMedienWest))

#===============================================================================

# datensätze für osten und westen, jeweils medien UND politiker
gemeinsamOst_absolut <- überblickPolOst %>% mutate(kennzeichnung = "Politiker Ost") %>%
  bind_rows(überblickMedienOst) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ost"))

gemeinsamOst_proportional <- proportionalOst_politiker %>% mutate(kennzeichnung = "Politiker Ost") %>%
  bind_rows(proportionalOst_medien) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ost"))

gemeinsamOst_minmax <- überblickPolOst_minmax %>% mutate(kennzeichnung = "Politiker Ost") %>%
  bind_rows(überblickMedienOst_minmax) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ost"))

ganzOsten <- as_tidytable(data.frame("names" = gemeinsamOst_absolut$names,
                                     "anzahlAbs" = gemeinsamOst_absolut$anzahl,
                                     "anzahlProporz" = gemeinsamOst_proportional$anzahl,
                                     "anzahlMinMax" = gemeinsamOst_minmax$anzahl,
                                     "kennzeichnung" = gemeinsamOst_absolut$kennzeichnung))

gemeinsamWest_absolut <- überblickPolWest %>% mutate(kennzeichnung = "Politiker West") %>%
  bind_rows(überblickMedienWest) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien West"))

gemeinsamWest_proportional <- proportionalWest_politiker %>% mutate(kennzeichnung = "Politiker West") %>%
  bind_rows(proportionalWest_medien) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien West"))

gemeinsamWest_minmax <- überblickPolWest_minmax %>% mutate(kennzeichnung = "Politiker West") %>%
  bind_rows(überblickMedienWest_minmax) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien West"))

ganzWesten <- as_tidytable(data.frame("names" = gemeinsamWest_absolut$names,
                                      "anzahlAbs" = gemeinsamWest_absolut$anzahl,
                                      "anzahlProporz" = gemeinsamWest_proportional$anzahl,
                                      "anzahlMinMax" = gemeinsamWest_minmax$anzahl,
                                      "kennzeichnung" = gemeinsamWest_absolut$kennzeichnung))

kompletto <- as_tidytable(data.frame("names" = ganzWesten$names,
                                     "absolutWesten" = ganzWesten$anzahlAbs,
                                     "proporzWesten" = ganzWesten$anzahlProporz,
                                     "minmaxWesten" = ganzWesten$anzahlMinMax,
                                     "absolutOsten" = ganzOsten$anzahlAbs,
                                     "proporzOsten" = ganzOsten$anzahlProporz,
                                     "minmaxOsten" = ganzOsten$anzahlMinMax,
                                     "kennung" = ganzWesten$kennzeichnung))

kompletto$kennzeichnung[kompletto$kennung == "Politiker West"] <- "Politiker"
kompletto$kennzeichnung[kompletto$kennung == "Medien West"] <- "Medien"

kompletto$names <- c("Covid", "Ukraine", "Energie", "Soziales", "Verteidigungspolitik", "PolizistenmordKusel", "FlutAhrtal", "PolitikEuropa", "PolitikInternational", "Klima", "ProtesteIran", "Verkehr", "PluralismusMedien", "Zukunft", "Verfassungsfeindlich", "Covid", "Ukraine", "Energie", "Soziales", "Verteidigungspolitik", "PolizistenmordKusel", "FlutAhrtal", "PolitikEuropa", "PolitikInternational", "Klima", "ProtesteIran", "Verkehr", "PluralismusMedien", "Zukunft", "Verfassungsfeindlich")

ganzOsten <- ganzOsten %>% mutate(names = fct_reorder(names, desc(-anzahlAbs)))
ganzWesten <- ganzWesten %>% mutate(names = fct_reorder(names, desc(-anzahlAbs)))
kompletto <- kompletto %>% mutate(names = fct_reorder(names, desc(absolutWesten)))

komplettoPolitiker <- kompletto %>% filter(kennzeichnung == "Politiker")
komplettoMedien <- kompletto %>% filter(kennzeichnung == "Medien")

komplettoOsten <- as_tidytable(data.frame("names"=komplettoPolitiker$names,
                                          "absolutPolitiker"=komplettoPolitiker$absolutOsten,
                                          "proporzPolitiker"=komplettoPolitiker$proporzOsten,
                                          "absolutMedien" = komplettoMedien$absolutOsten,
                                          "proporzMedien" = komplettoMedien$proporzOsten,
                                          "herkunft"="Ostdeutschland"))
komplettoWesten <- as_tidytable(data.frame("names"=komplettoPolitiker$names,
                                           "absolutPolitiker"=komplettoPolitiker$absolutWesten,
                                           "proporzPolitiker"=komplettoPolitiker$proporzWesten,
                                           "absolutMedien" = komplettoMedien$absolutWesten,
                                           "proporzMedien" = komplettoMedien$proporzWesten,
                                           "herkunft"="Westdeutschland"))

# ===============================================================================================================================================
# ===============================================================================================================================================
# ===============================================================================================================================================

# Farben
medienPolitikerFarben <- c("Medien"="#008080", "PolitikerInnen"="#fb4d46")

parteifarben <- c("AfD"="#0087c1", "B90/Die Grünen"="#19a329",
                  "CDU"="black", "CSU"="skyblue", "Die Linke"="#be3075",
                  "FDP"="#ffee00", "SPD"="#e40006", "SSW"="darkblue")

bundesländer <- c("Baden-Württemberg"="black", "Bayern"="skyblue", "Berlin"="#eb4c42",
                  "Brandenburg"="#69472D", "Bremen"="darkseagreen",
                  "Hamburg"="#246bce", "Hessen"="#873260", "Mecklenburg-Vorpommern"="#f5c942",
                  "Niedersachsen"="#cae00d", "Nordrhein-Westfalen"="#38761D",
                  "Rheinland-Pfalz"="#800020", "Saarland"="blue", "Sachsen"="#4a5d23",
                  "Sachsen-Anhalt"="#954535", "Schleswig-Holstein"="blue4",
                  "Thüringen"="#cdba96", "Überregional"="#a2add0")

sechzehnFarben <- c("black", "skyblue","#eb4c42", "#69472D", "darkseagreen", "#246bce",
                    "#873260", "#f5c942", "#cae00d", "#38761D", "#800020", "blue",
                    "#4a5d23", "#954535", "blue4", "#a1a3fd", "#b2beb5")

topicFarben <- c("ukraine"="#005bbb", "energie"="#ceff00", "soziales"="#e40006", "zukunft"="#cc397b",
                 "covid"="#b5a642", "klima"="#03c03c", "politikEuropa"="blue4", "politikInternational"="#8a496b",
                 "verkehr"="#4166f5", "verteidigungspolitik"="#4B5320",
                 "pluralismusMedien"="#00cdcd", "verfassungsfeindlich"="#654321", "protesteIran"="#229f40",
                 "flutAhrtal"="#873260", "polizistenmordKusel"="orange")

topicsDistinctColours <- c("Ukraine"="#0000cd", "Energie"="#00ff00", "Soziales"="#ff0000", "Zukunft"="#2f4f4f",
                          "Covid"="#6b8e23", "Klima"="#7f0000", "PolitikEuropa"="#191970", "PolitikInternational"="#48d1cc",
                          "Verkehr"="#ffff00", "Verteidigungspolitik"="#c71585",
                          "PluralismusMedien"="#00fa9a", "Verfassungsfeindlich"="#f4a460", "ProtesteIran"="#ff00ff",
                          "FlutAhrtal"="#d8bfd8", "PolizistenmordKusel"="#1e90ff")

theHeat <- heat.colors(14)
rainCol <- rainbow(14)
palettePolychrome1 <- Polychrome::createPalette(N = 15, seedcolors = c("#bb0040", "#10adf0", "#ec843e", "#356043"))
geographie <- c("Westen"="#10adf0", "Osten"="#b40040")
politikerMedienFarben <- c("Politiker"="#bb0040", "Medien"="#10adf0")
paletteCategorical3 <- c("Politiker Ost"="#bb0040", "Politiker Gesamt"="orange", "Politiker West" = "#10adf0",
                         "Medien Ost"="#bb0000", "Medien Gesamt"="orange", "Medien West"="#10adc0")

optimum <- c("Baden-Württemberg"="black", "Bayern"="#1e90ff", "Berlin"="red", "Brandenburg"="blue",
             "Bremen"="green", "Hamburg"="#814a19", "Hessen"="purple", "Mecklenburg-Vorpommern"="lightgrey",
             "Niedersachsen"="lightgreen", "Nordrhein-Westfalen"="lightblue", "Rheinland-Pfalz"="cyan", Saarland="orange",
             "Sachsen"="yellow", "Sachsen-Anhalt"="tan", "Schleswig-Holstein"="pink", "Thüringen"="#008080",
             "Überregional"="darkgrey")

coloursGeschäftsmodell <- c("Öffentlich"="#1e90ff", "Kommerziell"="limegreen", "Spenden"="red3")
coloursNormenwerte <- c("Objektiv-ausbalanciert"="darkgreen", "Parteiisch"="gold", "Marktgetrieben"="deeppink", "NA"="grey45")
coloursErreichbarkeit <- c("Etabliert (überregional)"="#6495ed", "Etabliert (regional)"="goldenrod", "Alternativ"="#bc8f8f", "Individuell"="turquoise3")

farbenPolitikerMedien <- c("PolitikerInnen"="#0000cd", "Medien"="#ffc125")


# ===============================================================================================================================================
# ===============================================================================================================================================
# ===============================================================================================================================================

## FORMELN

# NORMALISIERUNG MIN-MAX
minMaxNorm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

newNormalPol <- function(x) {
  # summe <- sum(df_col)
  x / 83658
}

newNormalMed <- function(x) {
  # summe <- sum(df_col)
  x / 607370
}

## PROZENTUALE THEMENANTEILE PRO FOKUSGRUPPE
formel_prozentualisierung <- function(obj) {
  obj / sum(obj)
}

# negation von %in%
"%ni%" <- Negate("%in%")

gc()
