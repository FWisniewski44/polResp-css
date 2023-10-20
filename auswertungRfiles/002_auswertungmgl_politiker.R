# ==============================================================================
# ============================= AUSWERTUNG =====================================
# ==============================================================================

# ==============================================================================
# ======================== Konzentration Politiker =============================
# ==============================================================================


################################################################################ libraries
library(tidyverse)
library(tidytext)
library(readr)
library(summarytools)
library(rvest)
library(expss)
library(hunspell)
library(lubridate)
library(quanteda)
# library(tm)
library(topicmodels)
library(stopwords)
library(stringi)
#library(rvisidata)
library(qdap)
library(dirichletprocess)
library(igraph)
library(ggraph)
library(esquisse)
library(ggforce)
library(ggh4x)
library(plotly)

################################################################################

library(knitr) 
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(flextable)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# emoji clipping
library(emoji)
library(textclean)

################################################################################

gc()

setwd(dir = "~/Documents/uni/masterarbeit/scraping/polResp-css/auswertungRfiles/")

################################################################################

# funktion batch loading politikerdaten
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
View(dat_politiker)

################################################################################

# # funktion batch loading politikerdaten
# filelist_medien <- list.files(path = "./mediendaten",
#                               pattern=".csv",
#                               full.names = T)
# 
# filelistNames_medien <- mgsub(filelist_medien, c("./mediendaten/","Kategorisiert.csv"), c("",""))
# 
# dat_medien <- list()
# 
# for (i in unique(filelist_medien)){
#   dat_medien[[i]] <- readr::read_csv(i)
# }
# 
# dat_medien <- dat_medien %>% purrr::set_names(filelistNames_medien)
# gc()
# View(dat_medien)

################################################################################

## datensatz für alle politiker erstellen, bei dem datum richtig gemacht wurde
allePolitiker <- reduce(dat_politiker, .f = full_join)
allePolitiker$dateTime <- as_datetime(allePolitiker$dateTime)

parteifarben <- c("#0087c1", "#19a329", "black", "#be3075", "#ffee00", "#e40006")

################################################################################

politikerBerlin <- dat_politiker$BERLIN_politiker
politikerBerlin$dateTime <- as_datetime(politikerBerlin$dateTime)

reframingPolitiker_berlin <- politikerBerlin %>% mutate(date = date(dateTime)) %>% 
  group_by(date, partei, user) %>%
  reframe(mentionsCovidVirus = sum(covidVirus),
          mentionsCovidMaßnahmen = sum(covidMaßnahmen), 
          mentionsCovidInfektionsgeschehen = sum(covidInfektionsgeschehen),
          mentionsCovidImpfung = sum(covidImpfung),
          mentionsUkraineAllgemein = sum(ukraineAllgemein),
          mentionsUkraineBewaffnung = sum(ukraineBewaffnung),
          mentionsUkraineReaktion = sum(ukraineReaktion),
          mentionsEnergieAllgemein = sum(energieAllgemein),
          mentionsEnergieKrise = sum(energieKrise),
          mentionsEnergieAtomkraftdiskurs = sum(energieAtomdiskurs),
          mentionsEnergieNordstream = sum(energieNordstream),
          mentionsSozialesWohnen = sum(sozialesWohnen),
          mentionsSozialesEntlastungen = sum(sozialesEntlastungen),
          mentionsSozialesKinder = sum(sozialesKinder),
          mentionsSozialesAllgemein = sum(sozialesAllgemein),
          mentionsVerteidigungspolitik = sum(verteidigungspolitik),
          mentionsPolitikNational = sum(politikNational),
          mentionsPolitikInternational = sum(politikInternational),
          mentionsPolitikEuropa = sum(politikEuropa),
          mentionsKlimaAktivismus = sum(klimaAktivismus),
          mentionsKlimaKlimawandel = sum(klimaKlimawandel),
          mentionsKlimaPolitisch = sum(klimaPolitisch),
          mentionsKlimaTechnologien = sum(klimaTechnologien),
          mentionsProtesteIran = sum(protesteIran),
          mentionsPolizistenmordKusel = sum(polizistenmordKusel),
          mentionsVerkehrAutomobil = sum(verkehrAutomobil),
          mentionsVerkehrFahrrad = sum(verkehrFahrrad),
          mentionsVerkehrPolitik = sum(verkehrPolitik),
          mentionsVerkehrÖpnv = sum(verkehrÖpnv),
          mentionsPluralismusMedien = sum(pluralismusMedien),
          mentionsZukunftEntwicklung = sum(zukunftEntwicklung),
          mentionsZukunftVersprechen = sum(zukunftVersprechen),
          mentionsVerfassungsfeindlich = sum(verfassungsfeindlich),
          partei = partei,
          user = user,
          follower = followerAmount,
          einzug = einzug,
          replies = replies,
          retweets = retweets,
          likes = likes)

reframingPolitiker_berlin %>% ggplot(aes(date, mentionsUkraineAllgemein)) +
  geom_col() +
  facet_wrap2(~partei, scales = "fixed", axes = T) +
  theme_bw()

reframingPolitiker_alle <- allePolitiker %>% mutate(date = date(dateTime)) %>% 
  group_by(date, partei, user) %>%
  reframe(mentionsCovidVirus = sum(covidVirus),
          mentionsCovidMaßnahmen = sum(covidMaßnahmen), 
          mentionsCovidInfektionsgeschehen = sum(covidInfektionsgeschehen),
          mentionsCovidImpfung = sum(covidImpfung),
          mentionsUkraineAllgemein = sum(ukraineAllgemein),
          mentionsUkraineBewaffnung = sum(ukraineBewaffnung),
          mentionsUkraineReaktion = sum(ukraineReaktion),
          mentionsEnergieAllgemein = sum(energieAllgemein),
          mentionsEnergieKrise = sum(energieKrise),
          mentionsEnergieAtomkraftdiskurs = sum(energieAtomdiskurs),
          mentionsEnergieNordstream = sum(energieNordstream),
          mentionsSozialesWohnen = sum(sozialesWohnen),
          mentionsSozialesEntlastungen = sum(sozialesEntlastungen),
          mentionsSozialesKinder = sum(sozialesKinder),
          mentionsSozialesAllgemein = sum(sozialesAllgemein),
          mentionsVerteidigungspolitik = sum(verteidigungspolitik),
          mentionsPolitikNational = sum(politikNational),
          mentionsPolitikInternational = sum(politikInternational),
          mentionsPolitikEuropa = sum(politikEuropa),
          mentionsKlimaAktivismus = sum(klimaAktivismus),
          mentionsKlimaKlimawandel = sum(klimaKlimawandel),
          mentionsKlimaPolitisch = sum(klimaPolitisch),
          mentionsKlimaTechnologien = sum(klimaTechnologien),
          mentionsProtesteIran = sum(protesteIran),
          mentionsPolizistenmordKusel = sum(polizistenmordKusel),
          mentionsVerkehrAutomobil = sum(verkehrAutomobil),
          mentionsVerkehrFahrrad = sum(verkehrFahrrad),
          mentionsVerkehrPolitik = sum(verkehrPolitik),
          mentionsVerkehrÖpnv = sum(verkehrÖpnv),
          mentionsPluralismusMedien = sum(pluralismusMedien),
          mentionsZukunftEntwicklung = sum(zukunftEntwicklung),
          mentionsZukunftVersprechen = sum(zukunftVersprechen),
          mentionsVerfassungsfeindlich = sum(verfassungsfeindlich),
          partei = partei,
          user = user,
          follower = followerAmount,
          einzug = einzug,
          replies = replies,
          retweets = retweets,
          likes = likes)

reframingPolitiker_alle %>% ggplot(aes(date, mentionsCovidMaßnahmen)) +
  geom_col() +
  facet_wrap2(~partei, axes=T, scales="fixed") +
  theme_bw()

################################################################################

# test1 => am beispiel der politiker berlins für erwähnungen des atomkraftdiskurses zur energiesicherheit
## erstellung eines arealgraphen für kompletten zeitstrang mit einfärbung nach partei
## ziel: ablesbarkeit des "ringens" der parteien um ein thema

test <- reframingPolitiker_berlin %>% ggplot(aes(date, mentionsEnergieAtomkraftdiskurs)) +
  geom_area(mapping = aes(fill=partei), position = "fill", na.rm = T) +
  scale_fill_discrete(type = parteifarben, guide = guide_legend(title = "Partei", title.theme = element_text(face = "bold"))) +
  xlab("") +
  ylab("Erwähnungen ukraineAllgemein") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_classic()

test
# ggplotly(test)

################################################################################

# test2 => am beispiel der politiker berlins für erwähnungen des ukrainekriegs und der bewaffnung der ukraine
## erstellung eines liniengraphen, abhängig je nach partei, mit abhebung der jeweiligen nutzer

janitor::tabyl(reframingPolitiker_berlin, partei, mentionsUkraineBewaffnung)
janitor::tabyl(politikerBerlin, partei, ukraineBewaffnung)

test2 <- reframingPolitiker_berlin %>% ggplot(aes(date, mentionsUkraineBewaffnung)) +
  geom_col(aes(fill=user, colour=partei), position="identity", width = 1) +
  facet_wrap2(~partei, axes = T, scales="fixed") +
  scale_colour_discrete(type = parteifarben, guide = guide_legend(title = "NutzerIn", title.theme = element_text(face = "bold"))) +
  xlab("") +
  ylab("Erwähnungen ukraineAllgemein") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_classic()

test2
ggplotly(test2)

################################################################################

# test3 => berliner politiker, erwähnungen des coronavirus im allgemeinen
## erstellung eines liniengraphen
## facetten nach parteien gliedern
test3 <- reframingPolitiker_berlin %>% ggplot(aes(date, mentionsCovidVirus)) +
  geom_line(mapping = aes(colour=partei)) +
  facet_wrap2(~partei, axes=T, scale="fixed") +
  scale_colour_discrete(type = parteifarben, guide = guide_legend(title = "Partei", title.theme = element_text(face = "bold"))) +
  xlab("") +
  ylab("Erwähnungen ukraineAllgemein") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_classic()

test3
ggplotly(test3)

################################################################################

# test4 => eine der obigen darstellungen mit allen politikern, idealerweise 

test4 <- reframingPolitiker_alle


