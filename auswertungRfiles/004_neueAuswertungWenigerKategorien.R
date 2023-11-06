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

########################################################################

# OPERATIONEN FÜR ALLE DATENSÄTZE, HIER EXEMPLARISCH
## politiker brd; datum = tagesebene
allePolitiker <- reduce(dat_politiker, .f = full_join)
allePolitiker <- as_tidytable(allePolitiker)
allePolitikerDay <- allePolitiker
allePolitikerDay$dateTime <- lubridate::date(allePolitiker$dateTime)

# ## berliner politiker; datum = tagesebene
# politikerBerlin <- dat_politiker$BERLIN_politiker
# politikerBerlin <- as_tidytable(politikerBerlin)
# politikerBerlin$dateTime <- date(politikerBerlin$dateTime)

## alle medien; datum = tagesebene
alleMedien <- reduce(dat_medien, .f = full_join)
alleMedien <- as_tidytable(alleMedien)
alleMedienDay <- alleMedien
alleMedienDay$dateTime <- lubridate::date(alleMedien$dateTime)

########################################################################

# FARBEN/DEKORATIONEN
parteifarben <- c("#0087c1", "#19a329", "black", "grey40", "#be3075", "#ffee00", "#e40006", "darkblue")

colours <- c("Ukraine"="#10adf9", "Covid"="#894c0a", "Energie"="#ec843e", "Klima"="#356043",
             "Soziales"="#bb0040", "Verkehr"="#e7c65d")

# politiker_alles_days <- allePolitikerDay %>%
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
# medien_alles_days <- alleMedienDay %>%
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


# alternativen datensatz erstellen zur abbildung der themen im zeitverlauf
alternativeMedien <- alleMedienDay %>% aggregate(covid ~ dateTime, sum) %>% as_tidytable(alternativeMedien)
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(ukraine ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(energie ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(soziales ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(verteidigungspolitik ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(polizistenmordKusel ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(politikEuropa ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(politikInternational ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(politikNational ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(klima ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(protesteIran ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(verkehr ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(pluralismusMedien ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(zukunft ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien,
                               y = (alleMedienDay %>% aggregate(verfassungsfeindlich ~ dateTime, sum)))

alternativeMedien

# alternativen datensatz erstellen zur abbildung der themen im zeitverlauf
# afdPolitiker <- allePolitikerDay %>% filter(partei == "AfD") %>% as_tidytable(afdPolitiker)

alternativePolitiker <- allePolitikerDay %>%
  as_tidytable(allePolitikerDay) %>%
  filter(partei == "AfD") %>%
  filter(bundesland == "Baden-Württemberg") %>%
  aggregate(covid ~ dateTime, sum)

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(ukraine ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(energie ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(soziales ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(verteidigungspolitik ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(polizistenmordKusel ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(flutAhrtal ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(politikEuropa ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(politikInternational ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(klima ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(protesteIran ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(verkehr ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(pluralismusMedien ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(zukunft ~ dateTime, sum)))

alternativePolitiker <- full_join(x = alternativePolitiker,
                                  y = (allePolitikerDay %>%
                                         filter(partei == "AfD") %>%
                                         filter(bundesland == "Baden-Württemberg") %>%
                                         aggregate(verfassungsfeindlich ~ dateTime, sum)))

# alternativePolitiker

################################################################################
################################################################################
################################################################################
## POLITIKERDATEN (SUMME) BEREINIGEN
# SO MUSS ES AUSSEHEN
# es kann hier bereits vorgefiltert werden
# es können mehrere dimensionen hinzugefügt werden (hinterer abteil aggregate, nach tilde):
## so z. b. möglich, nicht nur die themen über datum mit immer einem tag referenz aufzusummieren, sondern auch gleich die parteien oder auch die bundesländer mit einzubeziehen
# UND TROTZDEM BLEIBEN DIE DIAGRAMME GLEICH; egal welche ebene man mit einbezieht
#  es kommt dann nur auf die summierung mit an
politikerAnalysedaten <- allePolitikerDay %>%
  as_tidytable(allePolitikerDay) %>%
  # filter(partei == "AfD") %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "Alice_Weidel") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ dateTime +
              partei + user + bundesland + einzug + geschlecht + followerAmount,
            sum, na.rm = T, na.action = NULL)

politikerAnalysedaten$weeks <- as.Date(cut(politikerAnalysedaten$dateTime,
                              breaks = "week"))
politikerAnalysedaten$months <- as.Date(cut(politikerAnalysedaten$dateTime,
                               breaks = "month"))
politikerAnalysedaten$weeksLubridate <- lubridate::week(politikerAnalysedaten$dateTime)
politikerAnalysedaten$monthsLubridate <- lubridate::month(politikerAnalysedaten$dateTime)

save(politikerAnalysedaten, file = "zwischenspeicherung/politikerAnalysedaten.RData")
write_csv(politikerAnalysedaten, file = "zwischenspeicherung/politikerAnalysedaten.csv")

################################################################################
## MEDIENDATEN (SUMME) BEREINIGEN
##NACHBEARBEITUNGEN
arte <- alleMedienDay %>% filter(user == "ARTEde")
dat_medien$ÜBERREGIONALE_medien$bundesland <- "Überregional"
fre(dat_medien$ÜBERREGIONALE_medien$bundesland)
dat_medien$DIGITALE_medien$bundesland <- "überregional"
fre(dat_medien$DIGITALE_medien$bundesland)
dat_medien$ÖRR_medien$bundesland <- ""
dat_medien$ÖRR_medien <- dat_medien$ÖRR_medien %>% mutate(bundesland = case_when(user == "ARTEde" ~ "Überregional",
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
fre(dat_medien$DIGITALE_medien$bundesland)

freq(alleMedienDay$bundesland)

## alle medien; datum = tagesebene
alleMedien <- reduce(dat_medien, .f = full_join)
alleMedien <- as_tidytable(alleMedien)

alleMedienDay <- alleMedien
alleMedienDay$dateTime <- lubridate::date(alleMedien$dateTime)

medienAnalysedaten <- alleMedienDay %>%
  as_tidytable(alleMedienDay) %>%
  # filter(geschäftsmodell == 1) %>%
  # filter(normenWerte == 1) %>%
  # filter(erreichbarkeit == 1) %>%
  # filter(RVisits > 25000) %>%
  # filter(RVerkauf > 100000) %>%
  # filter(bundesland == "Baden-Württemberg") %>%
  # filter(user == "ARTEde") %>%
  aggregate(cbind(covid, ukraine, energie, soziales, verteidigungspolitik, polizistenmordKusel, flutAhrtal, politikEuropa, politikInternational, klima, protesteIran, verkehr, pluralismusMedien, zukunft, verfassungsfeindlich) ~ dateTime +
              user + bundesland + erreichbarkeit + normenWerte + geschäftsmodell + followerAmount,
            sum)

medienAnalysedaten

medienAnalysedaten$weeks <- as.Date(cut(medienAnalysedaten$dateTime,
                                           breaks = "week"))
medienAnalysedaten$months <- as.Date(cut(medienAnalysedaten$dateTime,
                                            breaks = "month"))
medienAnalysedaten$weeksLubridate <- lubridate::week(medienAnalysedaten$dateTime)
medienAnalysedaten$monthsLubridate <- lubridate::month(medienAnalysedaten$dateTime)

save(medienAnalysedaten, file = "zwischenspeicherung/medienAnalysedaten.RData")
write_csv(medienAnalysedaten, file = "zwischenspeicherung/medienAnalysedaten.csv")

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
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, 50)) +
  ggtitle(label = "Alle Politiker, täglich gruppiert", subtitle = "Themen: Ukraine und Covid") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_colour_manual(name = "", values = colours)

mediengraph <- medien_alles_days %>% ggplot() +
  geom_point(aes(x=dateTime, y=covid, colour="Covid")) +
  geom_line(aes(x=dateTime, y=covid, colour="Covid")) +
  geom_point(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  geom_line(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 200)) +
  ggtitle(label = "Alle Medien, täglich gruppiert", subtitle = "Themen: Ukraine und Covid") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_colour_manual(values = colours)

kombigraph <- ggarrange(politikergraph, mediengraph)
kombigraph

################################################################################

# überblicksgraphik mit allen themen in beiden sphären

themenüberblickPolitiker <- data.frame(apply(X = alternativePolitiker[,c(-1, -10)], MARGIN = 2, FUN = sum))
themenüberblickPolitiker$variable <- rownames(themenüberblickPolitiker)
themenüberblickPolitiker <- themenüberblickPolitiker %>% rename(anzahl = apply.X...alternativePolitiker...c..1...10....MARGIN...2..FUN...sum.)

themenüberblickMedien <- data.frame(apply(X = alternativeMedien[,c(-1, -10)], MARGIN = 2, FUN = sum))
themenüberblickMedien$variable <- rownames(themenüberblickMedien)
themenüberblickMedien <- themenüberblickMedien %>% rename(anzahl = apply.X...alternativeMedien...c..1...10....MARGIN...2..FUN...sum.)

sum(themenüberblickPolitiker$anzahl)

processPol <- preProcess(as_tidytable(themenüberblickPolitiker), method = "range")
themenüberblickPolitiker <- predict(processPol, as_tidytable(themenüberblickPolitiker))

processMedien <- preProcess(as_tidytable(themenüberblickMedien), method = "range")
themenüberblickMedien <- predict(processMedien, as_tidytable(themenüberblickMedien))


überblPol <- ggplot(mapping = aes(variable, anzahl)) +
  stat_summary(data = themenüberblickPolitiker, geom = "bar", fun = sum, position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
überblPol

ggplot(data= themenüberblickPolitiker) +
  geom_bar(aes(y=anzahl, fill=variable), position = "fill")

überblMedia <- ggplot(mapping = aes(variable, anzahl)) +
  stat_summary(data = themenüberblickMedien, geom = "bar", fun = sum, fill = "orange") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggarrange(überblMedia, überblPol, common.legend = F)


gemeinsam <- themenüberblickPolitiker %>% mutate(kennzeichnung = "Politiker") %>%
  bind_rows(themenüberblickMedien) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien"))

processGemeinsam

theHeat <- heat.colors(14)
rainCol <- rainbow(14)

ggplot(gemeinsam, aes(kennzeichnung, anzahl, fill = variable)) +
  stat_summary(geom="bar", fun=sum, position = "fill") +
  scale_fill_discrete(type = rainCol) +
  theme_dark()



