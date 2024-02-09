# ################################################################################ libraries
# library(tidytext)
# library(readr)
# library(summarytools)
# library(rvest)
# library(expss)
# # library(hunspell)
# library(lubridate)
# # library(quanteda)
# # library(tm)
# # library(topicmodels)
# # library(stopwords)
# library(stringi)
# #library(rvisidata)
# library(qdap)
# library(mgsub)
# library(dirichletprocess)
# library(igraph)
# library(ggraph)
# library(esquisse)
# library(ggforce)
# library(ggh4x)
# library(plotly)
# library(data.table)
# library(tidytable)
# library(gridExtra)
# library(ggpubr)
# library(scales)
# library(TSstudio)
# library(caret)
# library(ggsci)
# library(tidyverse)
# library(pals)
# library(Polychrome)
# library(hrbrthemes)
# library(ggpmisc)
# library(ggfortify)
# library(changepoint)
# library(strucchange)
# library(rsthemes)
#
# ################################################################################
#
# library(knitr)
# library(kableExtra)
# library(DT)
# # library(tm)
# # library(topicmodels)
# # library(reshape2)
# # library(ggplot2)
# # library(wordcloud)
# # library(pals)
# # library(SnowballC)
# # library(lda)
# # library(ldatuning)
# library(flextable)
# # activate klippy for copy-to-clipboard button
# # klippy::klippy()
#
# # emoji clipping
# # library(emoji)
# # library(textclean)

# ==============================================================================================================================================# ==============================================================================================================================================# ==============================================================================================================================================

# setwd(dir = "~/Documents/uni/masterarbeit/scraping/polResp-css/auswertungRfiles/")
#
# # load für relevante datensets
#
# load("zwischenspeicherung/environment3_komplett.RData")

# # colours
# medienPolitikerFarben <- c("Medien"="#008080", "PolitikerInnen"="#fb4d46")
#
# parteifarben <- c("AfD"="#0087c1", "B90/Die Grünen"="#19a329",
#                   "CDU"="black", "CSU"="skyblue", "Die Linke"="#be3075",
#                   "FDP"="#ffee00", "SPD"="#e40006", "Südschleswigscher Wählerverband"="darkblue")
#
# bundesländer <- c("Baden-Württemberg"="black", "Bayern"="skyblue", "Berlin"="#eb4c42",
#                   "Brandenburg"="#69472D", "Bremen"="darkseagreen",
#                   "Hamburg"="#246bce", "Hessen"="#873260", "Mecklenburg-Vorpommern"="#f5c942",
#                   "Niedersachsen"="#cae00d", "Nordrhein-Westfalen"="#38761D",
#                   "Rheinland-Pfalz"="#800020", "Saarland"="blue", "Sachsen"="#4a5d23",
#                   "Sachsen-Anhalt"="#954535", "Schleswig-Holstein"="blue4",
#                   "Thüringen"="#cdba96", "Überregional"="#a2add0")
#
# sechzehnFarben <- c("black", "skyblue","#eb4c42", "#69472D", "darkseagreen", "#246bce",
#                     "#873260", "#f5c942", "#cae00d", "#38761D", "#800020", "blue",
#                     "#4a5d23", "#954535", "blue4", "#a1a3fd", "#b2beb5")
#
# topicFarben <- c("ukraine"="#005bbb", "energie"="#ceff00", "soziales"="#e40006", "zukunft"="#cc397b",
#                  "covid"="#b5a642", "klima"="#03c03c", "politikEuropa"="blue4", "politikInternational"="#8a496b",
#                  "verkehr"="#4166f5", "verteidigungspolitik"="#4B5320",
#                  "pluralismusMedien"="#00cdcd", "verfassungsfeindlich"="#654321", "protesteIran"="#229f40",
#                  "flutAhrtal"="#873260", "polizistenmordKusel"="orange")
#
# theHeat <- heat.colors(14)
# rainCol <- rainbow(14)
# palettePolychrome1 <- Polychrome::createPalette(N = 15, seedcolors = c("#bb0040", "#10adf0", "#ec843e", "#356043"))
# geographie <- c("Westen"="#10adf0", "Osten"="#b40040")
# paletteCategorical2 <- c("Politiker"="#bb0040", "Medien"="#10adf0")
# paletteCategorical3 <- c("Politiker Ost"="#bb0040", "Politiker Gesamt"="orange", "Politiker West" = "#10adf0",
#                          "Medien Ost"="#bb0000", "Medien Gesamt"="orange", "Medien West"="#10adc0")

# ===============================================================================================================================================
# ######################################## FUNKTIONALE NEUBENENNUNGEN AUS DATEI 04
#
# # POLITIKER
# # timePol <- politikerAnalysedaten
# timePol_topics <- politikerAnalysedatenThemen
# timePol_party <- politikerAnalysedaten_Parteien
# timePol_userParty <- politikerAnalysedaten_userParteien
# timePol_topicsParty <- politikerAnalysedaten_TopicsUndParteien
# timePol_topicsUser <- politikerAnalysedaten_TopicsUndUser
# timePol_maximus <- politikerAnalysedaten_maximus
#
# # MEDIEN
# timeMedia <- medienAnalysedaten_userAggregiert
# timeMedia_topics <- medienAnalysedaten_tagesbasis
# timeMedia_topicsUser <- medienAnalysedaten_userTagesbasis
# timeMedia_userBundesland <- medienAnalysedaten_userBundesland
# timeMedia_maximus <- medienAnalysedaten_maximus
# # timeMedia_discursivePower <- medienAnalysedaten_discursivePower ## vorsicht, unterschiedliche anzahlen, lieber nicht benutzen

# ==============================================================================================================================================

# ÜBERBLICKSGRAPHIK: BEIDE SPHÄREN UND DIE JEWEILIGE THEMENBESPIELUNG IM DIREKTEN VERGLEICH
# dabei keine einschränkungen nach akteuren, sondern einfach alle genommen

# BILDUNG SUMME TWEETS PRO THEMA: politikerdaten
# themenüberblickPolitiker <- data.frame(apply(X = timePol_maximus[,c(5:19)], MARGIN = 2, FUN = sum)) # gleiches ergebnis, mehr text
themenüberblickPolitiker <- as_tidytable(apply(X = timePol_topics[,-1], MARGIN = 2, FUN = sum), .keep_rownames = T)
themenüberblickPolitiker <- themenüberblickPolitiker %>% rename(names = rn, anzahl = x)

## bei altem code noch genutzt; nur dann, wenn data.frame statt as_tidytable für apply genutzt;
## nur hier als spickzettel für zukunft
# themenüberblickPolitiker$names <- rownames(themenüberblickPolitiker)
# themenüberblickPolitiker <- themenüberblickPolitiker %>%
#   as_tidytable() %>%
#   rename(anzahl = apply.X...timePol_topics....1...MARGIN...2..FUN...sum.)

# ansicht
themenüberblickPolitiker

# factor reorder absteigend
themenüberblickPolitiker <- themenüberblickPolitiker %>% mutate(names = fct_reorder(names, desc(-anzahl)))

# tweets politiker insgesamt
sum(themenüberblickPolitiker$anzahl)

# alternativ auch das möglich
# themenüberblickPolitiker <- data.frame(apply(X = politikerAnalysedatenThemen[,-1], MARGIN = 2, FUN = sum))

# ===============================================================================================================================================

# BILDUNG SUMME TWEETS PRO THEMA: mediendaten
# unterschiedliche zahlen bei discpow datensatz vs. tagesbasis; hier nutzung der tagessummierung
# themenüberblickMedien <- data.frame(apply(X = medienAnalysedaten_discursivePower[,c(6:20)], MARGIN = 2, FUN = sum)) ## NICHT NUTZEN

themenüberblickMedien <- as_tidytable(apply(X = medienAnalysedaten_tagesbasis[,-1], MARGIN = 2, FUN = sum), .keep_rownames = T)
themenüberblickMedien <- themenüberblickMedien %>% rename(names = rn, anzahl = x)

# tidytable + ansicht
themenüberblickMedien

# factor reorder absteigend
themenüberblickMedien <- themenüberblickMedien %>% mutate(names = fct_reorder(names, desc(-anzahl)))

# tweets medien insgesamt
sum(themenüberblickMedien$anzahl)

# ===============================================================================================================================================

# NORMALISIERUNG MIN-MAX
minMaxNorm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

newNormalPol <- function(x) {
  # summe <- sum(df_col)
  x / 83658 # vorher: 98803
}

newNormalMed <- function(x) {
  # summe <- sum(df_col)
  x / 607871 # vorher: 611506
}

## PROZENTUALE THEMENANTEILE PRO FOKUSGRUPPE
formel_prozentualisierung <- function(obj) {
  obj / sum(obj)
}

###########

## NORMALISIERUNG AUF DEN PROZENTUALEN ANTEIL VON POSTS PRO THEMA
# normalisierung politiker- und medienthemen mentions auf gesamtzahl aller themen-mentions
proportionalPolitiker <- as_tidytable(apply(themenüberblickPolitiker[,2], MARGIN = 2, FUN = newNormalPol))
proportionalPolitiker$names <- themenüberblickPolitiker$names

proportionalMedia <- as_tidytable(apply(themenüberblickMedien[,2], MARGIN = 2, FUN = newNormalMed))
proportionalMedia$names <- themenüberblickPolitiker$names

proportionalPolitiker <- proportionalPolitiker %>% mutate(names = fct_reorder(names, desc(-anzahl)))
# proportionalMedia <- proportionalMedia %>% mutate(names = fct_reorder(names, desc(-anzahl)))

# MIN-MAX-NORMALISIERUNG
processPol <- preProcess(as_tidytable(themenüberblickPolitiker), method = "range")
minmaxPolitiker <- predict(processPol, as_tidytable(themenüberblickPolitiker))

processMedien <- preProcess(as_tidytable(themenüberblickMedien), method = "range")
minmaxMedia <- predict(processMedien, as_tidytable(themenüberblickMedien))

# ==============================================================================================================================================
# => SPEICHERUNG, DA HILFREICH FÜR THEMENFRAGE 1 ZUM EINSTIEG IN AUSWERTUNGSKAPITEL

# gruppe hinzufügen zu proportionalen datensätzen
proportionalPolitiker$GRUPPE <- "Politiker"
proportionalMedia$GRUPPE <- "Medien"

# GRAPHISCH
überblPol <- ggplot(data = proportionalPolitiker, mapping = aes(y=names, x=anzahl, fill=names)) +
  stat_summary(geom = "bar", fun = sum) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10, face = "bold")) +
  scale_x_percent(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) +
  scale_fill_manual(name="", values = topicsDistinctColours) +
  xlab("Anteil %") +
  ylab ("") +
  ggtitle("PolitikerInnen: Themenaufmerksamkeit", subtitle = "Prozentualer Anteil am Gesamtkorpus")

# ggplot(data= themenüberblickPolitiker) +
#    geom_bar(aes(y=anzahl, fill=names), position = "fill")

überblMedia <- ggplot(data = proportionalMedia, mapping = aes(y=names, x=anzahl, fill=names)) +
  stat_summary(geom = "bar", fun = sum) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10, face = "bold")) +
  scale_x_percent(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) +
  scale_fill_manual(name="", values = topicsDistinctColours) +
  xlab("Anteil %") +
  ylab ("") +
  ggtitle("Medien: Themenaufmerksamkeit", subtitle = "Prozentualer Anteil am Gesamtkorpus")

(themen_gegenüber_basic <- ggarrange(überblPol, überblMedia, legend = "none", labels = c("1)", "2)")))

## es gibt hier die möglichkeit, die themen in der gleichen farbe zu halten (topicFarben nutzen)
## oder alternativ die graphik nach den jeweiligen gruppen zu färben (rot politiker, blau medien z. B.)

# ggsave(themen_gegenüber_basic, filename = "themen_gegenüber_basic",
#        plot = themen_gegenüber_basic, units = "px",
#        device = "png",path = "graphiken/",
#        height = 800, width = 1400, dpi = "retina")

# ===============================================================================================================================================

# # OBACHT: nicht Ostdeutschland vergleichen mit Gesamtdeutschland; sonst ist Ost da auch mit eingerechnet
# # ERSTELLEN EINES GEMEINSAMEN DATENSATZES DES THEMENÜBERBLICKS MIT BEZEICHNUNGEN VON WELCHER GRUNDLAGE ETWAS STAMMT
# gemeinsam <- themenüberblickPolitiker %>% mutate(kennzeichnung = "Politiker Gesamt") %>%
#   bind_rows(themenüberblickMedien) %>%
#   mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Gesamt")) %>%
#   bind_rows(überblickPolOst) %>%
#   mutate(kennzeichnung = replace_na(kennzeichnung, "Politiker Ostdeutschland")) %>%
#   bind_rows(überblickMedienOst) %>%
#   mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ostdeutschland"))

# processGemeinsam <- preProcess(as_tidytable(gemeinsam), method = "range")
# gemeinsamNorm <- predict(processGemeinsam, as_tidytable(gemeinsam))

# # factor order
# themenüberblickPolitikerNorm <- themenüberblickPolitikerNorm %>% mutate(names = fct_reorder(names, desc(anzahl)))
# themenüberblickMedieNorm <- themenüberblickMedienNorm %>% mutate(names = fct_reorder(names, desc(anzahl)))
# gemeinsamNorm <- gemeinsamNorm %>% mutate(names = fct_reorder(names, desc(anzahl)))
#

#
# # GEGENÜBERSTELLUNG
# ggplot(gemeinsamNorm, aes(kennzeichnung, anzahl, fill = names)) +
#   stat_summary(geom="bar", fun=sum, position = "fill", colour="white") +
#   scale_fill_manual(values=unname(palettePolychrome1)) +
#   theme_dark()

# # piechart
# ggplot(data = themenüberblickPolitiker, mapping = aes(x="", y=anzahl, fill=names)) +
#   geom_bar(stat = "identity", width = 1, colour = "white") +
#   coord_polar("y", start = 0) +
#   scale_fill_manual(values=unname(topicFarben)) +
#   theme_void(base_family = "TeX Gyre Heros")

# # lollipop chart (OBACHT OSTDEUTSCHLAND VS. GESAMT)
# ggplot(data=gemeinsamNorm) +
#   geom_point(aes(x=names, y=anzahl, colour=kennzeichnung, size = 2.5)) +
#   geom_segment(aes(x=names, xend=names, y=0, yend=anzahl, colour=kennzeichnung)) +
#   coord_flip() +
#   scale_colour_manual(values=unname(palettePolychrome1)) +
#   hrbrthemes::theme_ft_rc()

# # barchart nebeneinander dodge
# ggplot(data=gemeinsamNorm, aes(x=names, y=anzahl, fill=kennzeichnung)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip() +
#   scale_fill_manual(values=unname(palettePolychrome1)) +
#   hrbrthemes::theme_ft_rc()

# # cleveland dot plot
# gemeinsamCleveland <- data.frame(names = themenüberblickPolitiker$names,
#                                  anzahlPolitiker = themenüberblickPolitiker$anzahl,
#                                  anzahlMedien = themenüberblickMedien$anzahl,
#                                  anzahlPolitikerNorm = head(gemeinsamNorm$anzahl, 15),
#                                  anzahlMedienNorm = tail(gemeinsamNorm$anzahl, 15))
# gemeinsamCleveland <- as_tidytable(gemeinsamCleveland)

# # sortierweise 1: nach größe des durchschnittswertes
# gemeinsamCleveland <- gemeinsamCleveland %>%
#   rowwise() %>%
#   mutate(durchschnittswerte = mean(c(anzahlPolitiker, anzahlMedien))) %>%
#   mutate(names = fct_reorder(names, desc(durchschnittswerte)))
#
# gemeinsamCleveland
# gemeinsamCleveland <- gemeinsamCleveland %>%
#   arrange(desc(durchschnittswerte)) %>%
#   mutate(names = factor(names, levels = names))
#
# ggplot(data = gemeinsamCleveland) +
#   geom_segment(aes(x=names, xend=names, y=anzahlPolitikerNorm, yend=anzahlMedienNorm), colour="white") +
#   geom_point(aes(x=names, y=anzahlPolitikerNorm, colour = "Politiker")) +
#   geom_point(aes(x=names, y=anzahlMedienNorm, colour = "Medien")) +
#   theme_ft_rc() +
#   scale_colour_manual(name="Gruppe", values = paletteCategorical2) +
#   coord_flip() +
#   ggtitle(label = "Themen", subtitle = "") +
#   xlab("Themen") +
#   ylab("%")

# gemeinsam <- themenüberblickPolitiker %>% mutate(kennzeichnung = "Politiker Gesamt") %>%
#   bind_rows(themenüberblickMedien) %>%
#   mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Gesamt")) %>%
#   bind_rows(überblickPolOst) %>%
#   mutate(kennzeichnung = replace_na(kennzeichnung, "Politiker Ostdeutschland")) %>%
#   bind_rows(überblickMedienOst) %>%
#   mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ostdeutschland"))

# ===============================================================================================================================================

# richtiger datensatz mit ost und west teilung erstellen
# in diesem fall muss überlegt werden, was mit denen passiert, die überregional sind
# am schlauesten ist wahrscheinlich wegzulassen
# überregional sagt ja auch aus: das ist nicht gebunden
ostdeutschlandListe <- c("Thüringen", "Sachsen", "Sachsen-Anhalt", "Brandenburg", "Mecklenburg-Vorpommern")
westdeutschland <- c("Baden-Württemberg", "Bayern", "Hessen", "Rheinland-Pfalz", "Nordrhein-Westfalen",
                     "Niedersachsen", "Schleswig-Holstein", "Saarland", "Bremen", "Hamburg", "Berlin")

# politiker und mediendaten je nach zuordnung zu ost und west
politikOst <- politikerAnalysedaten_maximus %>% filter(bundesland %in% ostdeutschlandListe)
medienOst <- medienAnalysedaten_maximus %>% filter(bundesland %in% ostdeutschlandListe)
#
politikWest <- politikerAnalysedaten_maximus %>% filter(bundesland %in% westdeutschland)
medienWest <- medienAnalysedaten_maximus %>% filter(bundesland %in% westdeutschland)

#===============================================================================

überblickPolOst <- as_tidytable(apply(X=politikOst[,c(5:19)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickPolOst <- überblickPolOst %>% rename(names = rn, anzahl = x)

überblickMedienOst <- as_tidytable(apply(X = medienOst[,c(4:18)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickMedienOst <- überblickMedienOst %>% rename(names = rn, anzahl = x)

überblickPolWest <- as_tidytable(apply(X = politikWest[,c(5:19)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickPolWest <- überblickPolWest %>% rename(names = rn, anzahl = x)

überblickMedienWest <- as_tidytable(apply(X = medienWest[,c(4:18)], MARGIN = 2, FUN = sum), .keep_rownames = T)
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
überblickPolOstNorm <- predict(processPolOst, as_tidytable(überblickPolOst))

processPolWest <- preProcess(as_tidytable(überblickPolWest), method = "range")
überblickPolWestNorm <- predict(processPolWest, as_tidytable(überblickPolWest))

processMedienOst <- preProcess(as_tidytable(überblickMedienOst), method = "range")
überblickMedienOstNorm <- predict(processMedienOst, as_tidytable(überblickMedienOst))

processMedienWest <- preProcess(as_tidytable(überblickMedienWest), method = "range")
überblickMedienWestNorm <- predict(processMedienWest, as_tidytable(überblickMedienWest))

#===============================================================================

# datensätze für osten und westen, jeweils medien UND politiker
gemeinsamOst <- überblickPolOst %>% mutate(kennzeichnung = "Politiker Ost") %>%
  bind_rows(überblickMedienOst) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ost"))

gemeinsamWest <- überblickPolWest %>% mutate(kennzeichnung = "Politiker West") %>%
  bind_rows(überblickMedienWest) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien West"))

#===============================================================================

# medien und politiker auf 100% normalisiert, macht an sich gar nicht sehr viel sinn, nur nice to have
proportionalWest_ganz<- as_tidytable(apply(gemeinsamWest[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalWest_ganz$names <- gemeinsamWest$names

proportionalOst_ganz<- as_tidytable(apply(gemeinsamOst[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalOst_ganz$names <- gemeinsamOst$names

#===============================================================================

# # gemeinsame min max; überarbeitung oder halt einfach rauslassen, weil sinnlos
# processOstNorm <- preProcess(as_tidytable(gemeinsamOst), method = "range")
# gemeinsamOstNorm <- predict(processOstNorm, as_tidytable(gemeinsamOst))
#
# processWestNorm <- preProcess(as_tidytable(gemeinsamWest), method = "range")
# gemeinsamWestNorm <- predict(processWestNorm, as_tidytable(gemeinsamWest))
#
# # aus ungleichem topf
# gemeinsamOstNormUngleichAbsolut <- data.frame(names = themenüberblickPolitiker$names,
#                                        anzahlPolitikerOstNorm = überblickPolOst$anzahl,
#                                        anzahlMedienOstNorm = überblickMedienOst$anzahl,
#                                        anzahlPolitikerNorm = themenüberblickPolitiker$anzahl,
#                                        anzahlMedienNorm = themenüberblickMedien$anzahl)
# #
# gemeinsamOstNormUngleich <- data.frame(names = themenüberblickPolitikerNorm$names,
#                                        anzahlPolitikerOstNorm = überblickPolOstNorm$anzahl,
#                                        anzahlMedienOstNorm = überblickMedienOstNorm$anzahl,
#                                        anzahlPolitikerNorm = themenüberblickPolitikerNorm$anzahl,
#                                        anzahlMedienNorm = themenüberblickMedienNorm$anzahl)

# gleich genormt
# gemeinsamOstNormGleich <- data.frame(names = themenüberblickPolitikerNorm$names,
#                                      anzahlPolitikerOstNorm = head(überblickPolOstNorm$anzahl, 15),
#                                      anzahlMedienOstNorm = tail(gemeinsamOstNorm$anzahl, 15),
#                                      anzahlPolitikerNorm = head(gemeinsamNorm$anzahl, 15),
#                                      anzahlMedienNorm = tail(gemeinsamNorm$anzahl, 15))

# norm ost vs. norm west: alle vier gruppen mit 2x pol und 2x medien sind jeweils in sich selbst genormt
# ANGLEICHUNG AN NEUE NAMENSGEBUNG NOTWENDIG
gnormt <- data.frame(names = themenüberblickPolitikerNorm$names,
                     politikerOst = proportionalOst_politiker$anzahl,
                     politikerWest = proportionalWest_politiker$anzahl,
                     medOst = proportionalOst_medien$anzahl,
                     medWest = proportionalWest_medien$anzahl)

# factor order GNORMT
SORT_gnormt_politiker <- gnormt %>% mutate(names = fct_reorder(names, desc(-politikerOst)))
SORT_gnormt_medien <- gnormt %>% mutate(names = fct_reorder(names, desc(-medOst)))
SORT_gnormt_alleGleich <- gnormt %>% mutate(names = fct_reorder(names, desc(-politikerOst)))

# vorsicht: hier wird gesamtdeutschland mit osten verglichen; gesamtdeutschland enthält nochmal den osten
# es werden potentiell doppelte nennungen gemacht und damit die statistiken verzerrt
# ggplot(data = gemeinsamOstNormUngleichAbsolut) +
#   geom_segment(aes(x=names, xend=names, y=anzahlPolitikerOstNorm, yend=anzahlPolitikerNorm), colour="white") +
#   geom_point(aes(x=names, y=anzahlPolitikerNorm, colour = "Politiker Gesamt")) +
#   geom_point(aes(x=names, y=anzahlPolitikerOstNorm, colour = "Politiker Ost")) +
#   theme_ft_rc() +
#   scale_colour_manual(name="Gruppe", values = paletteCategorical3) +
#   coord_flip() +
#   ggtitle(label = "Themen", subtitle = "") +
#   xlab("Themen") +
#   ylab("Anteil %")

#===============================================================================

# politiker ostdeutschland und westdeutschland im vergleich
politiker_ostwest <- ggplot(data = SORT_gnormt_alleGleich) +
  geom_segment(aes(x=names, xend=names, y=politikerOst, yend=politikerWest), colour="white") +
  geom_point(aes(x=names, y=politikerWest, colour = "Politiker West")) +
  geom_point(aes(x=names, y=politikerOst, colour = "Politiker Ost")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10, face = "bold")) +
  scale_y_percent() +
  scale_colour_manual(name="Gruppe", values = paletteCategorical3) +
  coord_flip() +
  ggtitle(label = "Themen der POLITIKERINNEN im Vergleich", subtitle = "Vergleich von Ost- und Westdeutschland") +
  xlab("Themen") +
  ylab("Anteil %")

# medien ostdeutschland und westdeutschland im vergleich
medien_ostwest <- ggplot(data = SORT_gnormt_alleGleich) +
  geom_segment(aes(x=names, xend=names, y=medOst, yend=medWest), colour="white") +
  geom_point(aes(x=names, y=medWest, colour = "Medien West")) +
  geom_point(aes(x=names, y=medOst, colour = "Medien Ost")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10, face = "bold")) +
  scale_y_percent() +
  sc <- le_colour_m <- nual(name="Gruppe", values = paletteCategorical3) +
  coord_flip() +
  ggtitle(label = "Themen der MEDIEN im Vergleich", subtitle = "Vergleich von Ost- und Westdeutschland") +
  xlab("Themen") +
  ylab("Anteil %")

lollipop_vergleich_ostwest <- ggarrange(politiker_ostwest, medien_ostwest, common.legend = T)
lollipop_vergleich_ostwest

#===============================================================================

SORT_überblickPolitiker <- themenüberblickPolitiker %>% mutate(names = fct_reorder(names, desc(anzahl)))
SORT_überblickMedien <- themenüberblickMedien %>% mutate(names = fct_reorder(names, desc(anzahl)))

SORT_überblickPolitiker <- cbind(SORT_überblickPolitiker, "Zuweisung"="Politiker")
SORT_überblickMedien <- cbind(SORT_überblickMedien, "Zuweisung"="Medien")

#===============================================================================

# # geht auch für prozentualisierung und bindung an df
# plyr::ddply(.data = SORT_überblickMedien, .variables = "Zuweisung", .fun = transform, prozent=formel_prozentualisierung(SORT_überblickMedien$anzahl))

# prozentuale werte an df anschließen
SORT_überblickMedien <- cbind(SORT_überblickMedien, "prozent"=formel_prozentualisierung(SORT_überblickMedien$anzahl))

# min max normalisierung und werte anhängen
processÜberblickMedien <- preProcess(as_tidytable(SORT_überblickMedien), method = "range")
SORT_überblickMedien_NORM <- predict(processÜberblickMedien, as_tidytable(SORT_überblickMedien))
SORT_überblickMedien_NORM <- cbind(SORT_überblickMedien_NORM, "anzahlAbs"=SORT_überblickMedien$anzahl)
medienAggregiert <- SORT_überblickMedien_NORM
medienAggregiert

# gleiches für politikerdaten
SORT_überblickPolitiker <- cbind(SORT_überblickPolitiker, "prozent"=formel_prozentualisierung(SORT_überblickPolitiker$anzahl))

processÜberblickPolitiker <- preProcess(as_tidytable(SORT_überblickPolitiker), method = "range")
SORT_überblickPolitiker_NORM <- predict(processÜberblickPolitiker, as_tidytable(SORT_überblickPolitiker))
SORT_überblickPolitiker_NORM <- cbind(SORT_überblickPolitiker_NORM, "anzahlAbs"=SORT_überblickPolitiker$anzahl)
politikerAggregiert <- SORT_überblickPolitiker_NORM
politikerAggregiert

# processPolOst <- preProcess(as_tidytable(überblickPolOst), method = "range")
# überblickPolOstNorm <- predict(processPolOst, as_tidytable(überblickPolOst))

ggplot(data = politikerAggregiert, aes(x=Zuweisung, y=prozent, fill=names)) +
  geom_bar(stat="identity", position = "dodge", width = 1.1) +
  geom_label(aes(label = anzahlAbs), vjust=-0.2, position = position_dodge(1.1)) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 9)) +
  scale_fill_manual(name="Themen", values = topicFarben) +
  ylab("Anteil Posts %") +
  xlab("") +
  ggtitle("Themen-Aufteilung in der Gruppe der PolitikerInnen", subtitle = "Prozentuale Anteile. Annotiert mit absoluten Posts/Thema")

# ggplotly(stackedbar_politiker)

ggplot(data = medienAggregiert, aes(x=Zuweisung, y=prozent, fill=names)) +
  geom_bar(stat="identity", position = "dodge", width = 1.1) +
  geom_label(aes(label = anzahlAbs), vjust=-0.2, position = position_dodge(1.1)) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 9)) +
  scale_fill_manual(name="Themen", values = topicFarben) +
  ylab("Anteil Posts %") +
  xlab("") +
  ggtitle("Themen-Aufteilung in den Medien", subtitle = "Prozentuale Anteile. Annotiert mit absoluten Posts/Thema")

# ggplotly(stackedbar_medien)

# ==============================================================================================================================================
# ==============================================================================================================================================
# ==============================================================================================================================================

########################################
# Zeitreihendaten zur Analyse ##########
########################################

# datensätze
# gesamte daten
timeseriesPolitikerWest <- timePol_maximus %>% filter(bundesland %in% westdeutschland)
timeseriesPolitikerOst <- timePol_maximus %>% filter(bundesland %in% ostdeutschlandListe)

# afd west und ost
TS_westAfd <- timePol_maximus %>%
  filter(bundesland %in% westdeutschland) %>%
  filter(partei == "AfD")

TS_ostAfd <- timePol_maximus %>%
  filter(bundesland %in% ostdeutschlandListe) %>%
  filter(partei == "AfD")

timePol_maximus_afd <- timePol_maximus %>% filter(partei == "AfD")
timePol_maximus_spd <- timePol_maximus %>% filter(partei == "SPD")
timePol_maximus_fdp <- timePol_maximus %>% filter(partei == "FDP")
unique(timePol_maximus_afd$user)

dim(TS_ostAfd)
dim(TS_westAfd)

# scale(timeseriesPolitikerOst$ukraine, center = T, scale = T) # z standardisierung

# plot zum thema ukrainekrieg im zeitverlauf, ost und west
ukraine_TS_allg <- ggplot() +
  stat_summary(data = timeseriesPolitikerWest, aes(dateTime, ukraine, colour = "Westen"), geom = "line", fun = sum) +
  stat_summary(data = timeseriesPolitikerOst, aes(dateTime, ukraine, colour = "Osten"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  scale_colour_manual(name="", values = geographie) +
  ggtitle(label = "Themenbehandlung Ukrainekrieg", subtitle = "Vergleich Ost- und Westdeutschland") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# plot zum thema ukrainekrieg im zeitverlauf, ost und west mit afd fokus
ukraine_TS_Afd <- ggplot() +
  stat_summary(data = TS_westAfd, aes(dateTime, ukraine, colour = "Westen"), geom = "line", fun = sum) +
  stat_summary(data = TS_ostAfd, aes(dateTime, ukraine, colour = "Osten"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  scale_colour_manual(name="", values = geographie) +
  ggtitle(label = "Themenbehandlung Ukrainekrieg durch\nPolitiker der AfD", subtitle = "Vergleich Ost- und Westdeutschland") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

ggarrange(ukraine_TS_allg, ukraine_TS_Afd,
          common.legend = T, legend = "top",
          ncol = 1, nrow = 2)

# ===============================================================================================================================================

# aktivste user zum thema covid in den parteien vergleichen
# erstellen einer informativen, dashboard-artigen visualisierung

# timePol_maximus_afd <- timePol_maximus %>% filter(partei == "AfD")
# timePol_maximus_spd <- timePol_maximus %>% filter(partei == "SPD")
# timePol_maximus_fdp <- timePol_maximus %>% filter(partei == "FDP")
# unique(timePol_maximus_afd$user)

activeCovidUsers <- timePol_maximus %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_afd <- timePol_maximus_afd %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_spd <- timePol_maximus_spd %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_fdp <- timePol_maximus_fdp %>% group_by(user) %>% filter(sum(covid) >= 100)

unique(activeCovidUsers$user)
unique(activeCovidUsers_afd$user)
unique(activeCovidUsers_spd$user)

a <- ggplot() +
  stat_summary(data = activeCovidUsers, aes(dateTime, covid, fill=partei), color="grey25", position = "fill", geom = "area", fun = sum, na.rm = T) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Das Thema COVID-19", subtitle = "Behandlung des Themas in Abhängigkeit von der Partei") +
  scale_fill_manual(name="Parteien", values = parteifarben) +
  xlab("") +
  ylab("Erwähnungen (absolut)")

b <- ggplot() +
  stat_summary(data = activeCovidUsers_afd, aes(dateTime, covid), color="#0087c1", geom = "line", fun = sum) +
  stat_summary(data = activeCovidUsers_spd, aes(dateTime, covid), color="#e40006", geom = "line", fun = sum) + #effektiv karl lauterbach alleine
  stat_summary(data = activeCovidUsers_fdp, aes(dateTime, covid), color="#ffee00", geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Behandlung des Themas COVID", subtitle = "Zeitverlauf für PolitikerInnen der AfD und der SPD im Vergleich") +
  scale_fill_manual(name="PolitikerInnen", values = sechzehnFarben) +
  xlab("") +
  ylab("Erwähnungen (absolut)")


# graphik für politiker mit mind. 80 tweets zum thema COVID über untersuchungszeitraum
c <- timePol_userParty %>%
  filter(covid >= 100) %>%
  mutate(user = fct_reorder(user, desc(-covid))) %>%
  ggplot(aes(y=user, x=covid, fill=partei)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name="Parteizugehörigkeit", values = parteifarben) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("Posts zum Thema 'covid'") +
  ylab("Usernamen") +
  ggtitle(label = "POLITIKERINNEN zum Thema COVID", subtitle = "Graphik für jene mit mind. 100 Tweets/Thema")

arrangement_politiker_covid <- ggarrange(a, ggarrange(b, c, ncol = 2), nrow = 2)
annotate_figure(arrangement_politiker_covid,
                bottom=text_grob("Hinweis: Gezählte Beiträge der SPD in diesen Graphiken stammen allein von Gesundheitsminister K. Lauterbach.",
                                 face = "italic",
                                 size = 8,
                                 color = "black"))

# ==============================================================================
# gruppe der politiker + wichtigste themen in einer graphik
# versuch der erkennung eines musters von interner themendynamik a la Rauchfleisch et al. (2021)

# funktioniert: 1 spalte = 100% für ein topic, jeder tag wird in prozentualem anteil am gesamtvolumen eines topics gemessen
# ist nur schlecht darstellbar, da extrem kleine werte
proportional_timePol_topics <- as_tidytable(apply(timePol_topics[,-1], MARGIN = 2, FUN = newNormalPol))
proportional_timePol_topics$dateTime <- timePol_topics$dateTime

# beobachtung: mehr als 3 themen pro graphik wird enorm unübersichtlich
testgraphik <- ggplot(data = timePol_topics) +
  geom_line(aes(x=dateTime, y=ukraine, color="ukraine")) +
  geom_line(aes(x=dateTime, y=verteidigungspolitik, color="verteidigungspolitik")) +
  scale_color_manual(name="Thema", values = topicFarben) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
testgraphik
ggplotly(testgraphik)

# themenüberblickPolitiker %>% mutate(names = fct_reorder(names, desc(anzahl)))

# graphik für politiker mit mind. 80 tweets zum thema COVID über untersuchungszeitraum
timePol_userParty %>%
  filter(covid >= 100) %>%
  mutate(user = fct_reorder(user, desc(-covid))) %>%
  ggplot(aes(y=user, x=covid, fill=partei)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name="Parteizugehörigkeit", values = parteifarben) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("Posts zum Thema 'covid'") +
  ylab("Usernamen") +
  ggtitle(label = "POLITIKERINNEN zum Thema COVID", subtitle = "Graphik für jene mit mind. 80 Tweets/Thema")

# graphik für politiker mit mind. 100 tweets zum thema UKRAINE über untersuchungszeitraum
timePol_userParty %>%
  filter(ukraine >= 100) %>%
  mutate(user = fct_reorder(user, desc(-ukraine))) %>%
  ggplot(aes(y=user, x=ukraine, fill=partei)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name="Parteizugehörigkeit", values = parteifarben) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("Posts zum Thema 'covid'") +
  ylab("Usernamen") +
  ggtitle(label = "POLITIKERINNEN zum Thema UKRAINE", subtitle = "Graphik für jene mit mind. 100 Tweets/Thema")

# ==============================================================================================================================================

# vorarbeiten für die graphik zum vergleich medien vs. politik und wer welche themen zuerst anspricht

# gleiches für medien mit dem thema COVID
medienCovid1200 <- timeMedia_userBundesland %>%
  filter(covid >= 1200) %>%
  mutate(user = fct_reorder(user, desc(-covid))) %>%
  ggplot(aes(y=user, x=covid, fill=bundesland)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = bundesland), hjust=-.05) +
  scale_fill_manual(values = bundesländer) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("Posts zum Thema 'covid'") +
  ylab("Usernamen") +
  ggtitle(label = "MEDIEN zum Thema COVID", subtitle = "Graphik für jene mit mind. 1200 Tweets/Thema")

# UKRAINE TOPIC
medienUkraine1200 <- timeMedia_userBundesland %>%
  filter(ukraine >= 1200) %>%
  mutate(user = fct_reorder(user, desc(-ukraine))) %>%
  ggplot(aes(y=user, x=ukraine, fill=bundesland)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = bundesland), hjust=-.05) +
  scale_fill_manual(values = bundesländer) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("Posts zum Thema 'covid'") +
  ylab("Usernamen") +
  ggtitle(label = "MEDIEN zum Thema UKRAINE", subtitle = "Graphik für jene mit mind. 1200 Tweets/Thema")

ggarrange(legend = "bottom", common.legend = F, medienUkraine1200, medienCovid1200)

normalize <- function(v, na.rm = FALSE) (v - min(v, na.rm = na.rm))/diff(range(v, na.rm = na.rm)) ## stackoverflow, macht aber das gleiche wie meine funktion, aber bezieht dabei die NAs mit ein, wobei sich keine darin befinden dürften -- an sich also obsolet

obj <- lapply(timePol_topics[,-1], FUN = minMaxNorm)
obj <- as_tidytable(obj)
obj$dateTime <- timePol_topics$dateTime

objMed <- lapply(timeMedia_topics[,-1], FUN = minMaxNorm)
objMed <- as_tidytable(objMed)
objMed$dateTime <- timeMedia_topics$dateTime

# ==============================================================================================================================================

# wichtige graphik zum vergleich: wer bespricht ein thema zuerst

TSukrainePolitiker <- ggplot() +
  stat_summary(data = obj, aes(dateTime, ukraine, colour = "ukraine"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de"), date_breaks = "2 weeks") +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "PolitikerInnen: Themenbehandlung Ukraine") +
  xlab("") +
  ylab("Erwähnungen (normalisiert)")

TSenergiePolitiker <- ggplot() +
  stat_summary(data = obj, aes(dateTime, energie, colour = "energie"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de"), date_breaks = "2 weeks") +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "PolitikerInnen: Themenbehandlung Energie") +
  xlab("") +
  ylab("Erwähnungen (normalisiert)")

ts_energieCovidPol_überschneidend <- ggplot() +
  stat_summary(data = obj, aes(dateTime, ukraine, colour = "ukraine"), geom = "line", fun = sum) +
  stat_summary(data = obj, aes(dateTime, energie, colour = "energie"), geom = "line", fun = sum) +
  stat_smooth(data = obj, aes(dateTime, ukraine), colour="blue3") +
  stat_smooth(data = obj, aes(dateTime, energie), colour="yellow3") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de"), date_breaks = "2 weeks") +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "PolitikerInnen: Vergleichsgraphik Ukraine und Energie", subtitle = "Vergleich des zeitlichen Verlaufs der Themen über den Beobachtungszeitraum inkl. geglätteter Trendlinie") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# ==============================================================================================================================================

TSsozialesPolitiker <- ggplot() +
  stat_summary(data = obj, aes(dateTime, soziales, colour = "soziales"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "PolitikerInnen: Themenbehandlung Soziales") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

TScovidPolitiker <- ggplot() +
  stat_summary(data = obj, aes(dateTime, covid, colour = "covid"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "PolitikerInnen: Themenbehandlung Covid") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# ==============================================================================================================================================

TSklimaPolitiker <- ggplot() +
  stat_summary(data = obj, aes(dateTime, klima, colour = "klima"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "PolitikerInnen: Themenbehandlung Klima") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

TSverkehrPolitiker <- ggplot() +
  stat_summary(data = obj, aes(dateTime, verkehr, colour = "verkehr"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "PolitikerInnen: Themenbehandlung Verkehr") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# ==============================================================================================================================================

TSzukunftPolitiker <- ggplot() +
  stat_summary(data = obj, aes(dateTime, zukunft, colour = "zukunft"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "POLITIKERINNEN: Themenbehandlung Zukunft") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

TSverteidigungPolitiker <- ggplot() +
  stat_summary(data = obj, aes(dateTime, verteidigungspolitik, colour = "verteidigungspolitik"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "POLITIKERINNEN: Themenbehandlung Verteidigungspolitik") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# ==============================================================================================================================================# ==============================================================================================================================================# ==============================================================================================================================================

# time series ukraine, energie, soziales, zukunft, covid medien
TSukraineMedien <- ggplot() +
  stat_summary(data = objMed, aes(dateTime, ukraine, colour = "ukraine"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "Medien: Themenbehandlung Ukraine") +
  xlab("") +
  ylab("Erwähnungen (normalisiert)")

TSenergieMedien <- ggplot() +
  stat_summary(data = objMed, aes(dateTime, energie, colour = "energie"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "Medien: Themenbehandlung Energie") +
  xlab("") +
  ylab("Erwähnungen (normalisiert")

ggplot() +
  stat_summary(data = objMed, aes(dateTime, ukraine, colour = "ukraine"), geom = "line", fun = sum) +
  stat_summary(data = objMed, aes(dateTime, energie, colour = "energie"), geom = "line", fun = sum) +
  stat_smooth(data = objMed, aes(dateTime, ukraine), colour = "blue") +
  stat_smooth(data = objMed, aes(dateTime, energie), colour = "yellow") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "Medien: Themenbehandlung Ukraine und Energie im Vergleich", subtitle = "Inklusive integrierter geglättetem Gesamttrend") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# ==============================================================================================================================================

TSsozialesMedien <- ggplot() +
  stat_summary(data = objMed, aes(dateTime, soziales, colour = "soziales"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "Medien: Themenbehandlung Soziales") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

TScovidMedien <- ggplot() +
  stat_summary(data = objMed, aes(dateTime, covid, colour = "covid"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "Medien: Themenbehandlung Covid") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# ==============================================================================================================================================

TSklimaMedien <- ggplot() +
  stat_summary(data = objMed, aes(dateTime, klima, colour = "klima"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "Medien: Themenbehandlung Klima") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

TSverkehrMedien <- ggplot() +
  stat_summary(data = objMed, aes(dateTime, verkehr, colour = "verkehr"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "Medien: Themenbehandlung Verkehr") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# ==============================================================================================================================================

TSzukunftMedien <- ggplot() +
  stat_summary(data = objMed, aes(dateTime, zukunft, colour = "zukunft"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "MEDIEN: Themenbehandlung Zukunft") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

TSverteidigungMedien <- ggplot() +
  stat_summary(data = objMed, aes(dateTime, verteidigungspolitik, colour = "verteidigungspolitik"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "POLITIKERINNEN: Themenbehandlung Verteidigungspolitik") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# DIE GRAPHIK UNBEDINGT AUSARBEITEN UND AUFNEHMEN!!!!!
ggarrange(nrow=2, ncol=2,
          TSukraineMedien, TSukrainePolitiker,
          TSenergieMedien, TSenergiePolitiker,
          # TSsozialesMedien, TSsozialesPolitiker,
          # TSzukunftMedien, TSzukunftPolitiker,
          # TScovidMedien, TScovidPolitiker,
          # TSklimaMedien, TSklimaPolitiker,
          legend="none")

ggarrange(nrow=2, ncol=2,
          # TSukraineMedien, TSukrainePolitiker,
          # TSenergieMedien, TSenergiePolitiker,
          TScovidMedien, TScovidPolitiker,
          TSsozialesMedien, TSsozialesPolitiker,
          # TSklimaMedien, TSklimaPolitiker,
          # TSverkehrMedien, TSverkehrPolitiker,
          legend="none")

ggarrange(nrow=2, ncol=2,
          # TSukraineMedien, TSukrainePolitiker,
          # TSenergieMedien, TSenergiePolitiker,
          # TScovidMedien, TScovidPolitiker,
          # TSsozialesMedien, TSsozialesPolitiker,
          TSklimaMedien, TSklimaPolitiker,
          TSverkehrMedien, TSverkehrPolitiker,
          legend="none")


ggplotly(TSukrainePolitiker)
ggplotly(TSenergiePolitiker)

ggplotly(TScovidPolitiker)
ggplotly(TSsozialesPolitiker)

ggplotly(TSklimaPolitiker +
           geom_hline(yintercept = 0.50, colour="red") +
           geom_hline(yintercept = 0.65, colour="yellow"))
ggplotly(TSverkehrPolitiker +
           geom_hline(yintercept = 0.40, colour="red") +
           geom_hline(yintercept = 0.55, colour="yellow"))

ggplotly(TSverteidigungPolitiker +
           geom_hline(yintercept = 0.25, colour="red"))


# ==============================================================================================================================================
# ==============================================================================================================================================# ==============================================================================================================================================

# medien betrachten:
# ähnlich der graphik zu aktivsten politikerinnen zu covid
# versuch des kopierens für den fall der medien

arrange(timeMedia_userBundesland, desc(covid))

activeCovidUsers_medien <- timeMedia_maximus %>% group_by(user) %>% filter(sum(covid) >= 2000)
activeUkraineUsers_medien <- timeMedia_maximus %>% group_by(user) %>% filter(sum(ukraine) >= 2000)

timeMedia_maximus %>% filter(user=="badischezeitung") %>% summarise()

covid_area_medien <- ggplot() +
  stat_summary(data = activeCovidUsers_medien, aes(dateTime, covid, fill=user), color="grey25", position = "fill", geom = "area", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  scale_fill_manual(name = "Medien per Usernamen", values = sechzehnFarben) +
  ggtitle(label = "Das Thema COVID-19 in den Medien", subtitle = "Behandlung des Themas über Zeitraum für Akteure mit mehr als 2000 Tweets zum Thema") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

covid_linechart_medien <- ggplot() +
  stat_summary(data = activeCovidUsers_medien, aes(dateTime, covid, color=user), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Zeitverlauf zum Thema COVID-19\nin der medialen Berichterstattung", subtitle = "Verlauf auf Tagesebene für Akteure mit 2000 oder mehr Tweets zum Thema") +
  scale_fill_manual(name="Medien per Usernamen", values = sechzehnFarben) +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# graphik für politiker mit mind. 80 tweets zum thema COVID über untersuchungszeitraum
# geht nicht, liegt an länge und sortierung
covid_akteure_medien <- timeMedia_maximus %>%
  group_by(user) %>%
  filter(sum(covid) <= 3500) %>%
  ungroup() %>%
  mutate(user = fct_reorder(user, desc(-sum(covid)))) %>%
  ggplot(aes(y=user, x=covid, fill=bundesland)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name="Bundesland", values = bundesländer) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("Posts zum Thema 'covid'") +
  ylab("Usernamen") +
  ggtitle(label = "MEDIALE AKTEURE, Thema COVID-19", subtitle = "Direkter Vergleich für Akteure mit 2000 oder mehr Tweets zum Thema")

ggarrange(covid_area_medien,
          ggarrange(covid_linechart_medien, covid_akteure_medien, ncol=2),
          nrow=2, labels = "AUTO")

# ==============================================================================================================================================

# graphen medien politiker in einem zum direkten kurvenvergleich der themen

ts_ukraine <- ggplot() +
  stat_summary(data = obj, aes(dateTime, ukraine, colour="PolitikerInnen"), geom = "line", fun = sum) +
  stat_summary(data = objMed, aes(dateTime, ukraine, colour="Medien"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="Gruppenzugehörigkeit", values = medienPolitikerFarben) +
  scale_y_percent() +
  ggtitle(label = "Thema Ukraine: Gegenüberstellung der Akteursgruppen") +
  xlab("") +
  ylab("Erwähnungen (absolut)")
ts_ukraine

ts_covid <- ggplot() +
  stat_summary(data = obj, aes(dateTime, covid, colour="PolitikerInnen"), geom = "line", fun = sum) +
  stat_summary(data = objMed, aes(dateTime, covid, colour="Medien"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="Gruppenzugehörigkeit", values = medienPolitikerFarben) +
  scale_y_percent() +
  ggtitle(label = "Thema Covid: Gegenüberstellung der Akteursgruppen") +
  xlab("") +
  ylab("Erwähnungen (absolut)")
ts_covid

ts_energie <- ggplot() +
  stat_summary(data = obj, aes(dateTime, energie, colour="PolitikerInnen"), geom = "line", fun = sum) +
  stat_summary(data = objMed, aes(dateTime, energie, colour="Medien"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="Gruppenzugehörigkeit", values = medienPolitikerFarben) +
  scale_y_percent() +
  ggtitle(label = "Thema Energie: Gegenüberstellung der Akteursgruppen") +
  xlab("") +
  ylab("Erwähnungen (absolut)")
ts_energie

ts_soziales <- ggplot() +
  stat_summary(data = obj, aes(dateTime, soziales, colour="PolitikerInnen"), geom = "line", fun = sum) +
  stat_summary(data = objMed, aes(dateTime, soziales, colour="Medien"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="Gruppenzugehörigkeit", values = medienPolitikerFarben) +
  scale_y_percent() +
  ggtitle(label = "Thema Soziales: Gegenüberstellung der Akteursgruppen") +
  xlab("") +
  ylab("Erwähnungen (absolut)")
ts_soziales

ggplot() +
  stat_summary(data = obj, aes(dateTime, covid, colour="PolitikerInnen"), geom = "line", fun = sum) +
  stat_summary(data = objMed, aes(dateTime, covid, colour="Medien"), geom = "line", fun = sum) +
  stat_smooth(data = obj, aes(dateTime, covid), colour="#fb4d46", method = "loess") +
  stat_smooth(data = objMed, aes(dateTime, covid), colour="#008080", method = "loess") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de"), date_breaks = "2 weeks") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="Gruppenzugehörigkeit", values = medienPolitikerFarben) +
  scale_y_percent() +
  ggtitle(label = "Thema Covid: Gegenüberstellung der Akteursgruppen") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

ggplot() +
  stat_summary(data = obj, aes(dateTime, ukraine, colour="PolitikerInnen"), geom = "line", fun = sum) +
  stat_summary(data = objMed, aes(dateTime, ukraine, colour="Medien"), geom = "line", fun = sum) +
  stat_smooth(data = obj, aes(dateTime, ukraine), colour="#fb4d46", method = "loess") +
  stat_smooth(data = objMed, aes(dateTime, ukraine), colour="#008080", method = "loess") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de"), date_breaks = "2 weeks") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="Gruppenzugehörigkeit", values = medienPolitikerFarben) +
  scale_y_percent() +
  ggtitle(label = "Thema Ukraine: Gegenüberstellung der Akteursgruppen") +
  xlab("") +
  ylab("Erwähnungen (absolut)")

# zeitlich zugeschnittene analysen möglich
objMed %>%
  filter(dateTime >= "2022-01-01") %>%
  filter(dateTime <= "2022-03-31") %>%
  ggplot() +
  stat_summary(aes(dateTime, ukraine, colour = "ukraine"), geom = "line", fun = sum) +
  stat_summary(aes(dateTime, covid, colour = "covid"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de"), date_breaks = "1 week") +
  theme_ft_rc(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name="", values = topicFarben) +
  scale_y_percent() +
  ggtitle(label = "Medien: Themenbehandlung Ukraine") +
  xlab("") +
  ylab("Erwähnungen (normalisiert)")

