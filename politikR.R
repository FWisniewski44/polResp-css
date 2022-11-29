install.packages("tidyverse", verbose = T)
library(readr)
library(tidyverse)

## TEST mit R:
## CSV mit Pandas in Python zu laden macht Probleme, da Datentypen nicht erkannt werden (dtypes problem).
## Mit R jedoch funktioniert alles reibungslos

## Möglichkeit: Datensatz aus Python speichern --- in R laden mit korrekter Datentyp-Angabe --- 
## dann wieder in Python reinladen und sehen, ob das funktioniert.

politiker1 <- read_csv("Documents/uni/masterarbeit/scraping/polResp-css/politiker1.csv",
                       col_types = cols(tweetID = col_character(), replies = col_integer(),
                                        retweets = col_integer(),
                                        likes = col_integer(), quotes = col_integer(),
                                        isRetweeted = col_character()))

write_csv(politiker1, file = "politikR.csv", na = "NA", progress = T)
