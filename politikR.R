# install.packages("tidyverse", verbose = T)
library(readr)
library(readODS)
library(tidyverse)
library(summarytools)

## TEST mit R:
## CSV mit Pandas in Python zu laden macht Probleme, da Datentypen nicht erkannt werden (dtypes problem).
## Mit R jedoch funktioniert alles reibungslos

## M??glichkeit: Datensatz aus Python speichern --- in R laden mit korrekter Datentyp-Angabe --- 
## dann wieder in Python reinladen und sehen, ob das funktioniert.

politiker1 <- read_csv("Documents/uni/masterarbeit/scraping/polResp-css/politiker1.csv",
                       col_types = cols(tweetID = col_character(), replies = col_integer(),
                                        retweets = col_integer(),
                                        likes = col_integer(), quotes = col_integer(),
                                        isRetweeted = col_character()))

write_csv(politiker1, file = "politikR.csv", na = "NA", progress = T)

### Medien auf regionaler Ebene in den Bundesländern
medientest_regio <- read_ods(path = "~/Documents/uni/masterarbeit/scraping/datensätze/sammlung_medien.ods",
                       sheet = 3)
summarytools::freq(medientest_regio$Gebiet)

### Medien auf nationaler/überregionaler Ebene
medientest_national <- read_ods(path = "~/Documents/uni/masterarbeit/scraping/datensätze/sammlung_medien.ods",
                                sheet = 2)
summarytools::freq(medientest_national$Gebiet)

### Medien des ÖRR Deutschland
medientest_örr <- read_ods(path = "~/Documents/uni/masterarbeit/scraping/datensätze/sammlung_medien.ods",
                                sheet = 4)
summarytools::freq(medientest_örr$Gebiet)

### Medien im digitalen Bereich
medientest_digi <- as_tibble(read_ods(path = "~/Documents/uni/masterarbeit/scraping/datensätze/sammlung_medien.ods",
                           sheet = 5))
summarytools::freq(medientest_digi$Klassifizierung)
unique(medientest_digi$Klassifizierung)
summarytools::freq(medientest_digi$InhaltThema)
summarytools::freq(medientest_digi$Gebiet)

blogs <- medientest_digi %>% filter(Klassifizierung == "Blog")
portale <- medientest_digi %>% filter(Klassifizierung == "Nachrichtenportal")

freq(blogs$Erreichbarkeit)
mean(blogs$RVisits)

freq(portale$Erreichbarkeit)








