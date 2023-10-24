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
klippy::klippy()

# emoji clipping
# library(emoji)
# library(textclean)

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

## datensatz für alle politiker erstellen, bei dem datum auf tagesebene ist
allePolitiker <- reduce(dat_politiker, .f = full_join)
allePolitiker <- as_tidytable(allePolitiker)
allePolitiker$dateTime <- date(allePolitiker$dateTime)

## identisch für berliner politiker
politikerBerlin <- dat_politiker$BERLIN_politiker
politikerBerlin <- as_tidytable(politikerBerlin)
politikerBerlin$dateTime <- date(politikerBerlin$dateTime)

parteifarben <- c("#0087c1", "#19a329", "black", "#be3075", "#ffee00", "#e40006")
colourrange <- c("Impfung"="#ffb900", "Infektion"="#10add6", "Maßnahmen"="#bb0040", "Virus"="#03d802")
colours <- c("ukraine"="blue", "covid"="red")

datumsrange <- seq(ymd("2022-10-01"), ymd("2022-10-31"), "1 day")
datumsrange_alternativ <- ymd("2022-10-01") %m+% days(x = seq.int(from = 0, to = 30, by = 1))

# str(datumsrange)
# str(datumsrange_alternativ)

################################################################################

# alternativen datensatz erstellen zur abbildung der themen im zeitverlauf
alternative <- politikerBerlin %>% aggregate(covidImpfung ~ dateTime, sum) %>% as_tidytable(alternative)
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(covidInfektionsgeschehen ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(covidMaßnahmen ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(covidVirus ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(ukraineAllgemein ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(ukraineBewaffnung ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(ukraineReaktion ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(energieAllgemein ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(energieAtomdiskurs ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(energieKrise ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(energieNordstream ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(sozialesAllgemein ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(sozialesEntlastungen ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(sozialesKinder ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(sozialesWohnen ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(verteidigungspolitik ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(polizistenmordKusel ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(politikEuropa ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(politikInternational ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(politikNational ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(klimaAktivismus ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(klimaKlimawandel ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(klimaPolitisch ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(klimaTechnologien ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(protesteIran ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(verkehrAutomobil ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(verkehrFahrrad ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(verkehrPolitik ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(verkehrÖpnv ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(pluralismusMedien ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(zukunftEntwicklung ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(zukunftVersprechen ~ dateTime, sum)))
alternative <- full_join(x = alternative, y = (politikerBerlin %>% aggregate(verfassungsfeindlich ~ dateTime, sum)))

alternative

plot1 <- alternative %>% as.data.table(alternative) %>% ggplot() +
  geom_point(aes(dateTime, covidImpfung, colour = "Impfung")) +
  geom_step(aes(dateTime, covidImpfung, colour = "Impfung")) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 25), limits = c(0, 25)) +
  scale_colour_manual(name = "", values = colourrange) +
  ylab("Erwähnungen covidImpfung") +
  xlab("") +
  ggtitle(label = "CORONA-IMPFUNG", subtitle = "Häufigkeit der Erwähnung über Zeit – Referenzgruppe: PolitikerInnen") +
  theme_bw()

plot2 <- alternative %>% as.data.table(alternative) %>% ggplot() +
  geom_point(aes(dateTime, covidInfektionsgeschehen, colour = "Infektion")) +
  geom_step(aes(dateTime, covidInfektionsgeschehen, colour = "Infektion")) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 25), limits = c(0, 25)) +
  scale_colour_manual(name = "", values = colourrange) +
  ylab("Erwähnungen covidInfektionsgeschehen") +
  xlab("") +
  ggtitle(label = "CORONA-INFEKTIONSGESCHEHEN", subtitle = "Häufigkeit der Erwähnung über Zeit – Referenzgruppe: PolitikerInnen") +
  theme_bw()

plot3 <- alternative %>% as.data.table(alternative) %>% ggplot() +
  geom_point(aes(dateTime, covidMaßnahmen, colour = "Maßnahmen")) +
  geom_step(aes(dateTime, covidMaßnahmen, colour = "Maßnahmen")) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 25), limits = c(0, 25)) +
  scale_colour_manual(name = "", values = colourrange) +
  ylab("Erwähnungen covidMaßnahmen") +
  xlab("") +
  ggtitle(label = "CORONA-MAẞNAHMEN", subtitle = "Häufigkeit der Erwähnung über Zeit – Referenzgruppe: PolitikerInnen") +
  theme_bw()

plot4 <- alternative %>% as.data.table(alternative) %>% ggplot() +
  geom_point(aes(dateTime, covidVirus, colour = "Virus")) +
  geom_step(aes(dateTime, covidVirus, colour = "Virus")) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 25)) +
  scale_colour_manual(name = "", values = colourrange) +
  ylab("Erwähnungen covidVirus") +
  xlab("") +
  ggtitle(label = "CORONAVIRUS ALLGEMEIN", subtitle = "Häufigkeit der Erwähnung über Zeit – Referenzgruppe: PolitikerInnen") +
  theme_bw()

ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2, common.legend=T, legend="none")
# ggplotly(plot1)

reframingPolitiker_berlin <- politikerBerlin %>%
  group_by(dateTime, partei) %>%
  reframe(.by = c(dateTime, partei), mentionsCovidVirus = sum(covidVirus),
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
          #partei = partei,
          user = user,
          follower = followerAmount,
          einzug = einzug,
          replies = replies,
          retweets = retweets,
          likes = likes,
          bundesland = bundesland)
reframingPolitiker_berlin

politikerBerlin %>% freq(ukraineAllgemein)

testing <- reframingPolitiker_berlin %>% ggplot() +
  geom_step(aes(dateTime, mentionsUkraineAllgemein, colour = "ukraine")) +
  geom_point(aes(dateTime, mentionsUkraineAllgemein, colour = "ukraine")) +
  geom_step(aes(dateTime, mentionsCovidVirus, colour = "covid")) +
  geom_point(aes(dateTime, mentionsCovidVirus, colour = "covid")) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  facet_wrap2(~partei, scales = "fixed", axes = T) +
  scale_colour_manual(name="Themen", values = colours) +
  ggtitle(label = "Beobachtungen zu den allgemeinen Themen Covid und Ukraine über die Zeit",
          subtitle = "Nach Parteien gruppiert und für jeweilige Themen eingefärbt") +
  xlab("") +
  ylab("Nachgewiesene Erwähnungen pro Topic") +
  theme_bw()
testing
ggplotly(testing)

reframingPolitiker_alle <- allePolitiker %>% group_by(dateTime, partei) %>%
  reframe(.by = c(dateTime, partei), mentionsCovidVirus = sum(covidVirus),
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
          #partei = partei,
          user = user,
          follower = followerAmount,
          einzug = einzug,
          replies = replies,
          retweets = retweets,
          likes = likes)

reframingPolitiker_alle %>% ggplot(aes(dateTime, mentionsCovidMaßnahmen)) +
  geom_step() +
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


