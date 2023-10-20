# ==============================================================================
# ============================= AUSWERTUNG =====================================
# ==============================================================================

# ==============================================================================
# ======================== Mögliche Visualisierungen ===========================
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

# funktion batch loading politikerdaten
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
View(dat_medien)

################################################################################

# test: erstellen eines datensatzes für baden württemberger medien um set runterzureduzieren zu testzwecken
bawü_medien <- dat_medien$BAWÜ_medien
bawü_medien$dateTime <- as_datetime(bawü_medien$dateTime)

## datensatz für alle politiker erstellen, bei dem datum richtig gemacht wurde
allePolitiker <- reduce(dat_politiker, .f = full_join)
allePolitiker$dateTime <- as_datetime(allePolitiker$dateTime)

# datensatz für alle medien erstellen
alleMedien <- reduce(dat_medien, .f = full_join)
alleMedien$dateTime <- as_datetime(alleMedien$dateTime)

################################################################################

# linechart mit aufsummierten tweets pro Tag im gesamten zeitraum zu covidImpfung mit facets nach usern

### GANZ WICHTIG. DAS IST DAS RICHTIGE. DOUBLE GROUP_BY IST MÖGLICH
sum(bawü_medien$covidImpfung)
summarisedCovidImpfung <- bawü_medien %>%
  mutate(date = date(dateTime)) %>%
  group_by(date, user) %>%
  summarise(total = sum(covidImpfung))
head(summarisedCovidImpfung)

p <- summarisedCovidImpfung %>%
  ggplot(aes(date, total)) +
  geom_line() +
  facet_wrap2(~user, scales = "fixed", axes = T) +
  theme_bw()

## wenn interaktiver graph gewünscht
# ggplotly(p)

## wenn interaktivität egal
p


# linechart mit aufsummierten tweets pro Tag im gesamten zeitraum zum ukrainekrieg allgemein, ohne fokus auf user

sum(bawü_medien$ukraineAllgemein)
summarisedUkraineAllg <- bawü_medien %>%
  mutate(date = date(dateTime)) %>%
  group_by(date) %>%
  summarise(total = sum(ukraineAllgemein))
head(summarisedUkraineAllg)

# bawü_medien %>%
#   mutate(date = date(dateTime)) %>%
#   group_by(date) %>%
#   summarise()

summarisedUkraineAllg %>%
  ggplot(aes(date, total)) +
  geom_line() +
  theme_bw()

################################################################################

# trial and error area

## reframe für covidVirus in BaWü
testreframeCovid <- bawü_medien %>%
  mutate(date = date(dateTime)) %>% 
  group_by(date) %>% 
  summarise(mentionsPerDay = sum(covidVirus))

## reframe statt summarise benutzen: user mit einbinden
testreframeUA <- bawü_medien %>%
  mutate(date = date(dateTime)) %>%
  group_by(date) %>%
  summarise(mentionsPerDay = sum(ukraineAllgemein))

# testreframeUA %>% ggplot(aes(date, mentionsPerDay)) +
#   geom_line() +
#   facet_wrap(~user) +
#   theme_bw()

# testreframe_covidImpfung <- bawü_medien %>%
#   mutate(date = date(dateTime)) %>%
#   group_by(date, user) %>%
#   summarise(total = sum(covidImpfung))


# funktioniert und sieht sehr gut aus
cols <- c("ukraine"="blue", "corona"="red")

covidVSukraine <- ggplot() +
  geom_line(data = testreframeUA, mapping = aes(date, mentionsPerDay, colour="ukraine")) +
  geom_line(data = testreframeCovid, mapping = aes(date, mentionsPerDay, colour="corona")) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  labs(x = "Datumsverlauf", y = "Anzahl Erwähnungen pro Tag, summiert", title = "Coronavirus vs. Ukraine-Krieg: Themenkonkurrenz, Medien Baden-Württemberg") +
  scale_color_manual(name = "Themen", values = cols) +
  theme(legend.position = "right")

covidVSukraine

ggplotly(covidVSukraine)

##################

reframingALLEPol_covidImpfung <- allePolitiker %>% 
  mutate(date = date(as_datetime(dateTime))) %>% group_by(date, user, partei) %>%
  reframe(total = sum(covidImpfung), username = user, partei = partei)

reframingALLEPol_covidImpfung %>% 
  ggplot(aes(date, total)) +
  geom_col() +
  facet_wrap2(~username + partei, scales = "fixed", axes = T) +
  theme_bw()

###################

reframingALLEMed_verfassungsfeindlich <- alleMedien %>% 
  mutate(date = date(dateTime)) %>% group_by(date, user, erreichbarkeit, normenWerte, geschäftsmodell) %>% 
  reframe(mentionsPerDay = sum(verfassungsfeindlich))

reframingALLEMed_verfassungsfeindlich %>% 
  ggplot(aes(date, mentionsPerDay)) +
  geom_line() +
  facet_wrap2(~user, scales = "fixed", axes = T)
  theme_bw()

###################

politikerBAYERN <- dat_politiker$BAYERN_politiker
politikerBAYERN$dateTime <- as_datetime(politikerBAYERN$dateTime)

reframingBAYERN_covidImpfung <- politikerBAYERN %>% 
  mutate(date = date(dateTime)) %>% group_by(date, user, partei) %>%
  summarise(total = sum(covidImpfung))

# filter(politikerBAYERN, user == "AlexanderRadwan") %>% freq(covidImpfung)

bayernPol_covidImpfung_PLOT <- reframingBAYERN_covidImpfung %>% filter(user == "AlexanderRadwan") %>% 
  ggplot(aes(date, total)) +
  geom_area() +
  facet_wrap2(~user+partei, scales="fixed", axes = T) +
  theme_bw()

bayernPol_covidImpfung_PLOT
ggplotly(bayernPol_covidImpfung_PLOT)

###################

library(janitor)
janitor::tabyl(politikerBAYERN, user, covidImpfung)
janitor::tabyl(politikerBerlin, partei, ukraineAllgemein)

# library(ggplot2)
# 
# typeof(bawü_medien$covidMaßnahmen)
# 
# bawü_medien %>% group_by(user) %>% summarise(values = sum(covidMaßnahmen), .groups = "drop") %>%
#   ggplot() + 
#   geom_point(mapping = aes(x=dateTime, y=values), colour = "#112446") +
#   theme_minimal()
# 
# ggplot(exdataframe, aes(x = cut(date, breaks = "month"), y = values, fill=Salesman)) + 
#   geom_bar(stat = "identity",position=position_dodge()) +
#   geom_text(aes(label=values), position=position_dodge(width=0.9), vjust=-0.25) + 
#   xlab("Month")
# 
# ggplot(bawü_medien) +
#   geom_bar(aes(x = dateTime, y = as.numeric(energieKrise)), stat = "identity") +
#   xlab("Datum") +
#   theme_bw()













