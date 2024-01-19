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

########################################################################

# OPERATIONEN FÜR ALLE DATENSÄTZE, HIER EXEMPLARISCH
# ## berliner politiker; datum = tagesebene
# politikerBerlin <- dat_politiker$BERLIN_politiker
# politikerBerlin <- as_tidytable(politikerBerlin)
# politikerBerlin$dateTime <- date(politikerBerlin$dateTime)

# ## alle medien; datum = tagesebene
# alleMedien <- reduce(dat_medien, .f = full_join)
# alleMedien <- as_tidytable(alleMedien)
# alleMedien$dateTime <- lubridate::date(alleMedien$dateTime)

# save(allePolitiker, file = "zwischenspeicherung/allePolitiker.RData")
# save(alleMedien, file = "zwischenspeicherung/alleMedien.RData")

########################################################################

# # FARBEN/DEKORATIONEN
# parteifarben <- c("#0087c1", "#19a329", "black", "grey40", "#be3075", "#ffee00", "#e40006", "darkblue")
#
# colours <- c("Ukraine"="#10adf9", "Covid"="#894c0a", "Energie"="#ec843e", "Klima"="#356043",
#              "Soziales"="#bb0040", "Verkehr"="#e7c65d")

# politiker_alles_days <- allePolitiker %>%
#   reframe(.by = c(dateTime),
#           mentionsCovid = sum(covid),
#           mentionsUkraine = sum(ukraine),
#           mentionsEnergie = sum(energie),
#           mentionsSoziales = sum(soziales),
#           mentionsVerteidigungspolitik = sum(verteidigungspolitik),
#           mentionsPolitikNational = sum(politikNational),
#           mentionsPolitikInternational = sum(politikInternational),
#           mentionsPolitikEuropa = sum(politikEuropa),
#           mentionsKlima = sum(klima),mentionsProtesteIran = sum(protesteIran),
#           mentionsPolizistenmordKusel = sum(polizistenmordKusel),
#           mentionsVerkehr = sum(verkehr),mentionsPluralismusMedien = sum(pluralismusMedien),
#           mentionsZukunft = sum(zukunft),mentionsVerfassungsfeindlich = sum(verfassungsfeindlich),
#           #partei = partei,
#           #user = user,
#           follower = followerAmount,
#           einzug = einzug,
#           replies = replies,
#           retweets = retweets,
#           likes = likes)
#
# medien_alles_days <- alleMedien %>%
#   reframe(.by = c(dateTime),
#           mentionsCovid = sum(covid),
#           mentionsUkraine = sum(ukraine),
#           mentionsEnergie = sum(energie),
#           mentionsSoziales = sum(soziales),
#           mentionsVerteidigungspolitik = sum(verteidigungspolitik),
#           mentionsPolitikNational = sum(politikNational),
#           mentionsPolitikInternational = sum(politikInternational),
#           mentionsPolitikEuropa = sum(politikEuropa),
#           mentionsKlima = sum(klima),mentionsProtesteIran = sum(protesteIran),
#           mentionsPolizistenmordKusel = sum(polizistenmordKusel),
#           mentionsVerkehr = sum(verkehr),mentionsPluralismusMedien = sum(pluralismusMedien),
#           mentionsZukunft = sum(zukunft),mentionsVerfassungsfeindlich = sum(verfassungsfeindlich),
#           #partei = partei,
#           user = user,
#           follower = followerAmount,
#           #einzug = einzug,
#           replies = replies,
#           retweets = retweets,
#           likes = likes)


# # alternativen datensatz erstellen zur abbildung der themen im zeitverlauf
# alternativeMedien <- alleMedien %>% aggregate(covid ~ dateTime, sum) %>% as_tidytable(alternativeMedien)
# alternativePolitiker <- allePolitiker %>% aggregate(covid ~ dateTime, sum) %>% as_tidytable(alternativePolitiker)
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(ukraine ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(energie ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(soziales ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(verteidigungspolitik ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(polizistenmordKusel ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(politikEuropa ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(politikInternational ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(politikNational ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(klima ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(protesteIran ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(verkehr ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(pluralismusMedien ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(zukunft ~ dateTime, sum)))
# alternativeMedien <- full_join(x = alternativeMedien,
#                                y = (alleMedien %>% aggregate(verfassungsfeindlich ~ dateTime, sum)))
#
# alternativeMedien
#
# # alternativen datensatz erstellen zur abbildung der themen im zeitverlauf
# # afdPolitiker <- allePolitiker %>% filter(partei == "AfD") %>% as_tidytable(afdPolitiker)
#
# alternativePolitiker <- allePolitiker %>%
#   as_tidytable(allePolitiker) %>%
#   filter(partei == "AfD") %>%
#   filter(bundesland == "Baden-Württemberg") %>%
#   aggregate(covid ~ dateTime, sum)
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(ukraine ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(energie ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(soziales ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(verteidigungspolitik ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(polizistenmordKusel ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(flutAhrtal ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(politikEuropa ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(politikInternational ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(klima ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(protesteIran ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(verkehr ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(pluralismusMedien ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(zukunft ~ dateTime, sum)))
#
# alternativePolitiker <- full_join(x = alternativePolitiker,
#                                   y = (allePolitiker %>%
#                                          filter(partei == "AfD") %>%
#                                          filter(bundesland == "Baden-Württemberg") %>%
#                                          aggregate(verfassungsfeindlich ~ dateTime, sum)))
#
# # alternativePolitiker

# ===============================================================================================================================================

################################################################################
################################################################################
################################################################################
## POLITIKERDATEN (SUMME) BEREINIGEN
# SO MUSS ES AUSSEHEN
# es kann hier bereits vorgefiltert werden (vgl. partei, bundesland, user, ...)
# es können mehrere dimensionen hinzugefügt werden (hinterer abteil aggregate, nach tilde):
## so z. b. möglich, nicht nur die themen über datum mit immer einem tag referenz aufzusummieren, sondern auch gleich die parteien oder auch die bundesländer mit einzubeziehen
# UND TROTZDEM BLEIBEN DIE DIAGRAMME GLEICH; egal welche ebene man mit einbezieht
#  es kommt dann nur auf die summierung mit an
#  wichtig: wenn nach ~ noch weitere variablen mit "+" ergänzt werden, dann wird zugleich auch über diese aggregiert
#  hier ist vorsicht geboten weil das sonst keinen sinn macht!

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

#### ANFÜGEN VON WOCHENNUMMERN UND MONATSSORTIERUNG
#### ERMÖGLICHUNG: SPÄTERE ANALYSEN AUF ANDERER ZEITEBENE

## geht momentan noch nicht, weil die datensatz-namen noch alt sind;
## könnte aber angepasst werden!
# politikerAnalysedaten$weeks <- as.Date(cut(politikerAnalysedaten$dateTime,
#                               breaks = "week"))
# politikerAnalysedaten$months <- as.Date(cut(politikerAnalysedaten$dateTime,
#                                breaks = "month"))
# politikerAnalysedaten$weeksLubridate <- lubridate::week(politikerAnalysedaten$dateTime)
# politikerAnalysedaten$monthsLubridate <- lubridate::month(politikerAnalysedaten$dateTime)
#
# ####
#
# politikerAnalysedatenThemen$weeks <- as.Date(cut(politikerAnalysedatenThemen$dateTime,
#                                            breaks = "week"))
# politikerAnalysedatenThemen$months <- as.Date(cut(politikerAnalysedatenThemen$dateTime,
#                                             breaks = "month"))
# politikerAnalysedatenThemen$weeksLubridate <- lubridate::week(politikerAnalysedatenThemen$dateTime)
# politikerAnalysedatenThemen$monthsLubridate <- lubridate::month(politikerAnalysedatenThemen$dateTime)

#### OPTIONALE SPEICHERUNG ALS R OBJEKTE UND CSV DATEIEN

# save(politikerAnalysedaten, file = "zwischenspeicherung/politikerAnalysedaten.RData")
# write_csv(politikerAnalysedaten, file = "zwischenspeicherung/politikerAnalysedaten.csv")
# save(politikerAnalysedatenThemen, file = "zwischenspeicherung/politikerAnalysedatenThemen.RData")
# write_csv(politikerAnalysedatenThemen, file = "zwischenspeicherung/politikerAnalysedatenThemen.csv")

# ===============================================================================================================================================
################################################# MEDIENDATEN (SUMME) BEREINIGEN

# ===================================================
# datumseinstellung
# alleMedien$dateTime <- as.Date(alleMedien$dateTime)
# ===================================================

# arte <- alleMedien %>% filter(user == "ARTEde")
dat_medien$ÜBERREGIONALE_medien$bundesland <- "Überregional"
freq(dat_medien$ÜBERREGIONALE_medien$bundesland)
dat_medien$DIGITALE_medien$bundesland <- "Überregional"
freq(dat_medien$DIGITALE_medien$bundesland)
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
fre(dat_medien$ÖRR_medien$bundesland)

# hier momentan dann noch alle auf NA, die nicht bei den regionalen medien dabei waren
freq(alleMedien$bundesland)

## alle medien: neues reduce + tidytable, damit die bundesländer im gesamten df drin sind
alleMedien <- reduce(dat_medien, .f = full_join)
alleMedien <- as_tidytable(alleMedien)
alleMedien$dateTime <- as.Date(alleMedien$dateTime)
freq(alleMedien$bundesland)
alleMedien

# ===================================================
# herstellen der analysedaten für die medien nach vorbild dessen, was für politiker schon gemacht wurde

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

# ALT!!! NUR HIER ALS REFERENZ, EIGENTLICH UNNÜTZ
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

# medienAnalysedaten
# medienAnalysedatenThemen

####

#gleiches spiel wie bei den politikerdaten;
#kann ebenfalls reaktiviert werden!
# medienAnalysedaten$weeks <- as.Date(cut(medienAnalysedaten$dateTime,
#                                            breaks = "week"))
# medienAnalysedaten$months <- as.Date(cut(medienAnalysedaten$dateTime,
#                                             breaks = "month"))
# medienAnalysedaten$weeksLubridate <- lubridate::week(medienAnalysedaten$dateTime)
# medienAnalysedaten$monthsLubridate <- lubridate::month(medienAnalysedaten$dateTime)
#
# ####
#
# medienAnalysedatenThemen$weeks <- as.Date(cut(medienAnalysedatenThemen$dateTime,
#                                               breaks = "week"))
# medienAnalysedatenThemen$months <- as.Date(cut(medienAnalysedatenThemen$dateTime,
#                                          breaks = "month"))
# medienAnalysedatenThemen$weeksLubridate <- lubridate::week(medienAnalysedatenThemen$dateTime)
# medienAnalysedatenThemen$monthsLubridate <- lubridate::month(medienAnalysedatenThemen$dateTime)

####

# save(medienAnalysedaten, file = "zwischenspeicherung/medienAnalysedaten.RData")
# write_csv(medienAnalysedaten, file = "zwischenspeicherung/medienAnalysedaten.csv")
# save(medienAnalysedatenThemen, file = "zwischenspeicherung/medienAnalysedatenThemen.RData")
# write_csv(medienAnalysedatenThemen, file = "zwischenspeicherung/medienAnalysedatenThemen.csv")

################################################################################
################################################################################
################################################################################

# plotting neuer datensatz
politikerAnalysedaten %>% ggplot() +
  stat_summary(aes(dateTime, ukraine, colour = "Ukraine"), geom = "line", fun = sum) +
  stat_summary(aes(dateTime, covid, colour = "Covid"), geom = "line", fun = sum) +
  stat_summary(aes(dateTime, ukraine, colour = "Ukraine"), geom = "point", fun = sum) +
  stat_summary(aes(dateTime, covid, colour = "Covid"), geom = "point", fun = sum) +
  #facet_wrap_paginate(~ bundesland, page = 1) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%m", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(name="Thema", values = colours) +
  ggtitle(label = "Themenkonkurrenz, täglich", subtitle = "Coronavirus vs. Ukrainekrieg") +
  xlab("Tage") +
  ylab("Erwähnungen")

medienAnalysedaten %>% ggplot() +
  stat_summary(aes(dateTime, ukraine, colour = "Ukraine"), geom = "line", fun = sum) +
  stat_summary(aes(dateTime, covid, colour = "Covid"), geom = "line", fun = sum) +
  stat_summary(aes(dateTime, ukraine, colour = "Ukraine"), geom = "point", fun = sum) +
  stat_summary(aes(dateTime, covid, colour = "Covid"), geom = "point", fun = sum) +
  #facet_wrap_paginate(~ bundesland, page = 1) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%m", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(name="Thema", values = colours) +
  ggtitle(label = "Themenkonkurrenz, täglich", subtitle = "Coronavirus vs. Ukrainekrieg") +
  xlab("Tage") +
  ylab("Erwähnungen")

# # vars für wochen und monate schaffen
# alternativePolitiker$weeks <- as.Date(cut(alternativePolitiker$dateTime,
#                                           breaks = "week"))
# alternativePolitiker$months <- as.Date(cut(alternativePolitiker$dateTime,
#                                            breaks = "month"))


# plot wöchentlich
politikerAnalysedaten %>% ggplot() +
  stat_summary(aes(weeks, ukraine, colour = "Ukraine"), geom = "line", fun.y = sum) +
  stat_summary(aes(weeks, covid, colour = "Covid"), geom = "line", fun.y = sum) +
  stat_summary(aes(weeks, ukraine, colour = "Ukraine"), geom = "point", fun.y = sum) +
  stat_summary(aes(weeks, covid, colour = "Covid"), geom = "point", fun.y = sum) +
  scale_x_date(breaks = "1 week", labels = date_format(format = "KW %W", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(name="Thema", values = colours) +
  ggtitle(label = "Themenkonkurrenz, wöchentlich", subtitle = "Coronavirus vs. Ukrainekrieg") +
  xlab("Wochen") +
  ylab("Erwähnungen")

medienAnalysedaten %>% ggplot() +
  stat_summary(aes(weeks, ukraine, colour = "Ukraine"), geom = "line", fun = sum) +
  stat_summary(aes(weeks, covid, colour = "Covid"), geom = "line", fun = sum) +
  stat_summary(aes(weeks, klima, colour = "Klima"), geom = "line", fun = sum) +
  stat_summary(aes(weeks, energie, colour = "Energie"), geom = "line", fun = sum) +
  stat_summary(aes(weeks, soziales, colour = "Soziales"), geom = "line", fun = sum) +
  stat_summary(aes(weeks, verkehr, colour = "Verkehr"), geom = "line", fun = sum) +
  stat_summary(aes(weeks, ukraine, colour = "Ukraine"), geom = "point", fun = sum) +
  stat_summary(aes(weeks, covid, colour = "Covid"), geom = "point", fun = sum) +
  stat_summary(aes(weeks, klima, colour = "Klima"), geom = "point", fun = sum) +
  stat_summary(aes(weeks, energie, colour = "Energie"), geom = "point", fun = sum) +
  stat_summary(aes(weeks, soziales, colour = "Soziales"), geom = "point", fun = sum) +
  stat_summary(aes(weeks, verkehr, colour = "Verkehr"), geom = "point", fun = sum) +
  scale_x_date(breaks = "1 week", labels = date_format(format = "KW %W", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(name="Thema", values = colours) +
  ggtitle(label = "Themenkonkurrenz, wöchentlich", subtitle = "Themen: Ukrainekrieg, Corona, Klima, Energie, Soziales und Verkehr") +
  xlab("Wochen") +
  ylab("Erwähnungen")

# plot monatlich
politikerAnalysedaten %>% ggplot() +
  stat_summary(aes(months, ukraine, colour = "Ukraine"), geom = "line", fun.y = sum) +
  stat_summary(aes(months, covid, colour = "Covid"), geom = "line", fun.y = sum) +
  stat_summary(aes(months, ukraine, colour = "Ukraine"), geom = "point", fun.y = sum) +
  stat_summary(aes(months, covid, colour = "Covid"), geom = "point", fun.y = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b %Y", locale = "de")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  scale_colour_manual(name="Thema", values = colours) +
  ggtitle(label = "Themenkonkurrenz, monatlich", subtitle = "Coronavirus vs. Ukrainekrieg") +
  xlab("Monate") +
  ylab("Erwähnungen")

medienAnalysedaten %>% ggplot() +
  stat_summary(aes(months, ukraine, colour = "Ukraine"), geom = "line", fun.y = sum) +
  stat_summary(aes(months, covid, colour = "Covid"), geom = "line", fun.y = sum) +
  stat_summary(aes(months, ukraine, colour = "Ukraine"), geom = "point", fun.y = sum) +
  stat_summary(aes(months, covid, colour = "Covid"), geom = "point", fun.y = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b %Y", locale = "de")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  scale_colour_manual(name="Thema", values = colours) +
  ggtitle(label = "Themenkonkurrenz, monatlich", subtitle = "Coronavirus vs. Ukrainekrieg") +
  xlab("Monate") +
  ylab("Erwähnungen")

# politiker vs medien bei covid und energie über die zeit (sowas ist auch wöchentlich/monatlich denkbar)
politikergraph <- politiker_alles_days %>% ggplot() +
  geom_point(aes(x=dateTime, y=covid, colour="Covid")) +
  geom_line(aes(x=dateTime, y=covid, colour="Covid")) +
  geom_point(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  geom_line(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  theme_bw() +
  #scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, 50)) +
  ggtitle(label = "Alle Politiker, täglich gruppiert", subtitle = "Themen: Ukraine und Covid") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_colour_manual(name = "", values = colours)

mediengraph <- medien_alles_days %>% ggplot() +
  geom_point(aes(x=dateTime, y=covid, colour="Covid")) +
  geom_line(aes(x=dateTime, y=covid, colour="Covid")) +
  geom_point(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  geom_line(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  theme_bw() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 200)) +
  ggtitle(label = "Alle Medien, täglich gruppiert", subtitle = "Themen: Ukraine und Covid") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_colour_manual(values = colours)

kombigraph <- ggarrange(politikergraph, mediengraph)
kombigraph

################################################################################

# überblicksgraphik mit allen themen in beiden sphären

# BILDUNG SUMME TWEETS PRO THEMA
themenüberblickPolitiker <- data.frame(apply(X = politikerAnalysedaten[,c(8:22)], MARGIN = 2, FUN = sum))
themenüberblickPolitiker$variable <- rownames(themenüberblickPolitiker)
themenüberblickPolitiker <- themenüberblickPolitiker %>% rename(anzahl = apply.X...politikerAnalysedaten...c.8.22....MARGIN...2..FUN...sum.)

themenüberblickMedien <- data.frame(apply(X = medienAnalysedaten[,c(8:22)], MARGIN = 2, FUN = sum))
themenüberblickMedien$variable <- rownames(themenüberblickMedien)
themenüberblickMedien <- themenüberblickMedien %>% rename(anzahl = apply.X...medienAnalysedaten...c.8.22....MARGIN...2..FUN...sum.)

sum(themenüberblickPolitiker$anzahl)
sum(themenüberblickMedien$anzahl)

# STANDARDISIERUNG MIN-MAX
processPol <- preProcess(as_tidytable(themenüberblickPolitiker), method = "range")
themenüberblickPolitiker <- predict(processPol, as_tidytable(themenüberblickPolitiker))

processMedien <- preProcess(as_tidytable(themenüberblickMedien), method = "range")
themenüberblickMedien <- predict(processMedien, as_tidytable(themenüberblickMedien))

## dabei ersichtlich: grösstes thema politiker ist ukraine, grösstes thema medien ist covid

# GRAPHISCH
überblPol <- ggplot(mapping = aes(variable, anzahl)) +
  stat_summary(data = themenüberblickPolitiker, geom = "bar", fun = sum, fill = "#b04044") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
überblPol

ggplot(data= themenüberblickPolitiker) +
  geom_bar(aes(y=anzahl, fill=variable), position = "fill")

überblMedia <- ggplot(mapping = aes(variable, anzahl)) +
  stat_summary(data = themenüberblickMedien, geom = "bar", fun = sum, fill = "orange") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggarrange(überblMedia, überblPol, common.legend = F)

# ERSTELLEN EINES GEMEINSAMEN DATENSATZES DES THEMENÜBERBLICKS MIT BEZEICHNUNGEN VON WELCHER GRUNDLAGE ETWAS STAMMT
gemeinsam <- themenüberblickPolitiker %>% mutate(kennzeichnung = "Politiker") %>%
  bind_rows(themenüberblickMedien) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien"))

## farbschemata: schwierigkeit der abbildung von 14 themen
theHeat <- heat.colors(14)
rainCol <- rainbow(14)

# GEGENÜBERSTELLUNG
ggplot(gemeinsam, aes(kennzeichnung, anzahl, fill = variable)) +
  stat_summary(geom="bar", fun=sum, position = "fill") +
  scale_fill_discrete(type = rainCol) +
  theme_dark()



