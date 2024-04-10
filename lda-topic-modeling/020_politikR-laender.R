# ==============================================================================
# =============================== ORDNUNG ======================================
# ==============================================================================

# Aufspalten des Politiker-Datensatzes nach Bundesländern für spätere Analyse
# Erörterung idealer k-Werte pro Bundesland

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

# if needed, set working directory
setwd(dir="~/Documents/uni/masterarbeit/methods/representative-speech/")

# datensatz aller politiker
politikR <- read_csv("regionalisierung/demojized/politikR_demojized.csv")

# wichtige variablen und listen
# eigene liste von stopwords
myStopwordsDE <- stopwords::stopwords(language = "de", source = "stopwords-iso")
myStopwordsDE <- append(myStopwordsDE, values = stopwords::stopwords(language = "de", source = "marimo"))
myStopwordsDE <- append(myStopwordsDE, values = stopwords::stopwords(language = "en", source = "stopwords-iso"))
myStopwordsDE <- append(myStopwordsDE, values = c("&lt;3", "amp", "lt3", "„",
                                                  "…", "“", "’", "\n"))
allstopwords <- unique(myStopwordsDE)
length(myStopwordsDE)
length(allstopwords)
length(stopwords::stopwords(language = "de", source = "stopwords-iso"))

# minFreq für wörter im corpus, damit sie erfasst werden
minimumFrequency <- 5

################################################################################

polBawü <- politikR %>% filter(bundesland == "Baden-Württemberg")
unique(polBawü$user)

polBay <- politikR %>% filter(bundesland == "Bayern")
unique(polBay$user)

polBrandenburg <- politikR %>% filter(bundesland == "Brandenburg")
unique(polBrandenburg$user)

polBerlin <- politikR %>% filter(bundesland == "Berlin")
unique(polBerlin$user)

polBremen <- politikR %>% filter(bundesland == "Bremen")
unique(polBremen$user)

polHamburg <- politikR %>% filter(bundesland == "Hamburg")
unique(polHamburg$user)

polHessen <- politikR %>% filter(bundesland == "Hessen")
unique(polHessen$user)

polMeckPomm <- politikR %>% filter(bundesland == "Mecklenburg-Vorpommern")
unique(polMeckPomm$user)

polNds <- politikR %>% filter(bundesland == "Niedersachsen")
unique(polNds$user)

polNrw <- politikR %>% filter(bundesland == "Nordrhein-Westfalen")
unique(polNrw$user)

polRlp <- politikR %>% filter(bundesland == "Rheinland-Pfalz")
unique(polRlp$user)

polSaar <- politikR %>% filter(bundesland == "Saarland")
unique(polSaar$user)

polSachsen <- politikR %>% filter(bundesland == "Sachsen")
unique(polSachsen$user)

polSachsenAnhalt <- politikR %>% filter(bundesland == "Sachsen-Anhalt")
unique(polSachsenAnhalt$user)

polSchleswig <- politikR %>% filter(bundesland == "Schleswig-Holstein")
unique(polSchleswig$user)

polThüringen <- politikR %>% filter(bundesland == "Thüringen")
unique(polThüringen$user)

################################################################################

write_csv(polBawü, file = "regionalisierung/politiker-regionalisiert/politiker_baden-württemberg.csv")
write_csv(polBay, file = "regionalisierung/politiker-regionalisiert/politiker_bayern.csv")
write_csv(polBerlin, file = "regionalisierung/politiker-regionalisiert/politiker_berlin.csv")
write_csv(polBrandenburg, file = "regionalisierung/politiker-regionalisiert/politiker_brandenburg.csv")
write_csv(polBremen, file = "regionalisierung/politiker-regionalisiert/politiker_bremen.csv")
write_csv(polHamburg, file = "regionalisierung/politiker-regionalisiert/politiker_hamburg.csv")
write_csv(polHessen, file = "regionalisierung/politiker-regionalisiert/politiker_hessen.csv")
write_csv(polMeckPomm, file = "regionalisierung/politiker-regionalisiert/politiker_meckPomm.csv")
write_csv(polNds, file = "regionalisierung/politiker-regionalisiert/politiker_niedersachsen.csv")
write_csv(polNrw, file = "regionalisierung/politiker-regionalisiert/politiker_nrw.csv")
write_csv(polRlp, file = "regionalisierung/politiker-regionalisiert/politiker_rlp.csv")
write_csv(polSaar, file = "regionalisierung/politiker-regionalisiert/politiker_saarland.csv")
write_csv(polSachsen, file = "regionalisierung/politiker-regionalisiert/politiker_sachsen.csv")
write_csv(polSachsenAnhalt, file = "regionalisierung/politiker-regionalisiert/politiker_sachsen-anhalt.csv")
write_csv(polSchleswig, file = "regionalisierung/politiker-regionalisiert/politiker_schleswig-holstein.csv")
write_csv(polThüringen, file = "regionalisierung/politiker-regionalisiert/politiker_thüringen.csv")

################################################################################

polBawü <- read_csv("regionalisierung/politiker-regionalisiert/politiker_baden-württemberg.csv")
polBay <- read_csv("regionalisierung/politiker-regionalisiert/politiker_bayern.csv")
polBerlin <- read_csv("regionalisierung/politiker-regionalisiert/politiker_berlin.csv")
polBrandenburg <- read_csv("regionalisierung/politiker-regionalisiert/politiker_brandenburg.csv")
polBremen <- read_csv("regionalisierung/politiker-regionalisiert/politiker_bremen.csv")
polHamburg <- read_csv("regionalisierung/politiker-regionalisiert/politiker_hamburg.csv")
polHessen <- read_csv("regionalisierung/politiker-regionalisiert/politiker_hessen.csv")
polMeckPomm <- read_csv("regionalisierung/politiker-regionalisiert/politiker_meckPomm.csv")
polNds <- read_csv("regionalisierung/politiker-regionalisiert/politiker_niedersachsen.csv")
polNrw <- read_csv("regionalisierung/politiker-regionalisiert/politiker_nrw.csv")
polRlp <- read_csv("regionalisierung/politiker-regionalisiert/politiker_rlp.csv")
polSaar <- read_csv("regionalisierung/politiker-regionalisiert/politiker_saarland.csv")
polSachsen <- read_csv("regionalisierung/politiker-regionalisiert/politiker_sachsen.csv")
polSachsenAnhalt<- read_csv("regionalisierung/politiker-regionalisiert/politiker_sachsen-anhalt.csv")
polSchleswig <- read_csv("regionalisierung/politiker-regionalisiert/politiker_schleswig-holstein.csv")
polThüringen <- read_csv("regionalisierung/politiker-regionalisiert/politiker_thüringen.csv")

################################################################################

# AB HIER FORSETZEN WENN DATEI NEU GELADEN UND WORKSPACE CLEAN IST!

# funktion für batch load von allen dateien auf einmal im ordner
# filelist <- list.files(path = "regionalisierung/politiker-regionalisiert/", pattern=".csv", full.names = T)
# for (i in filelist){
#   load(paste(i))
# }

################################################################################ BADEN-WÜRTTEMBERG

# erste row muss von 1 bis max durchzählen für späteres modell
polBawü <- polBawü[,-1]
polBawü <- cbind(doc_id = 1:nrow(polBawü), polBawü)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polBawü$text <- gsub("http*(.*)", " ", polBawü$text, perl = TRUE)
polBawü$text <- gsub("https*(.*)", " ", polBawü$text, perl = TRUE)
polBawü$text <- gsub("@\\w+", " ", polBawü$text, perl=TRUE)
polBawü$text <- gsub("\\n", " ", polBawü$text, perl = TRUE)
polBawü$text <- gsub("lt3", " ", polBawü$text, perl = TRUE)
polBawü$text <- gsub("…", " ", polBawü$text, perl = TRUE)
polBawü$text <- gsub("’", " ", polBawü$text, perl = TRUE)
polBawü$text <- gsub("‘", " ", polBawü$text, perl = TRUE)
#
corpus_polBawü <- Corpus(DataframeSource(polBawü))
processedCorpus_polBawü <- tm_map(corpus_polBawü, removeWords, allstopwords)
processedCorpus_polBawü <- tm_map(processedCorpus_polBawü, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polBawü <- tm_map(processedCorpus_polBawü, stripWhitespace)

DTM_polBawü <- DocumentTermMatrix(processedCorpus_polBawü, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polBawü)
DTM_polBawü

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polBawü) > 0
DTM_polBawü <- DTM_polBawü[sel_idx, ]
polBawü <- polBawü[sel_idx, ]

# find topic number
optimal_polBawü <- ldatuning::FindTopicsNumber(
  DTM_polBawü,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polBawü, file = "CaoJuan-Deveaud_results/politikerBundesländer/BAWÜ_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polBawü)

optimal_polBawüII <- ldatuning::FindTopicsNumber(
  DTM_polBawü,
  topics = seq(from = 6, to = 18, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polBawüII)

################################################################################ BAYERN

# erste row muss von 1 bis max durchzählen für späteres modell
polBay <- polBay[,-1]
polBay <- cbind(doc_id = 1:nrow(polBay), polBay)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polBay$text <- gsub("http*(.*)", " ", polBay$text, perl = TRUE)
polBay$text <- gsub("https*(.*)", " ", polBay$text, perl = TRUE)
polBay$text <- gsub("@\\w+", " ", polBay$text, perl=TRUE)
polBay$text <- gsub("\\n", " ", polBay$text, perl = TRUE)
polBay$text <- gsub("lt3", " ", polBay$text, perl = TRUE)
polBay$text <- gsub("…", " ", polBay$text, perl = TRUE)
polBay$text <- gsub("’", " ", polBay$text, perl = TRUE)
polBay$text <- gsub("‘", " ", polBay$text, perl = TRUE)
#
corpus_polBay <- Corpus(DataframeSource(polBay))
processedCorpus_polBay <- tm_map(corpus_polBay, removeWords, allstopwords)
processedCorpus_polBay <- tm_map(processedCorpus_polBay, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polBay <- tm_map(processedCorpus_polBay, stripWhitespace)

DTM_polBay <- DocumentTermMatrix(processedCorpus_polBay, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polBay)
DTM_polBay

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polBay) > 0
DTM_polBay <- DTM_polBay[sel_idx, ]
polBay <- polBay[sel_idx, ]

# find topic number
optimal_polBay <- ldatuning::FindTopicsNumber(
  DTM_polBay,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polBay, file = "CaoJuan-Deveaud_results/politikerBundesländer/BAYERN_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polBay)

optimal_polBayII <- ldatuning::FindTopicsNumber(
  DTM_polBay,
  topics = seq(from = 6, to = 18, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polBayII)

################################################################################ BERLIN

# erste row muss von 1 bis max durchzählen für späteres modell
polBerlin <- polBerlin[,-1]
polBerlin <- cbind(doc_id = 1:nrow(polBerlin), polBerlin)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polBerlin$text <- gsub("http*(.*)", " ", polBerlin$text, perl = TRUE)
polBerlin$text <- gsub("https*(.*)", " ", polBerlin$text, perl = TRUE)
polBerlin$text <- gsub("@\\w+", " ", polBerlin$text, perl=TRUE)
polBerlin$text <- gsub("\\n", " ", polBerlin$text, perl = TRUE)
polBerlin$text <- gsub("lt3", " ", polBerlin$text, perl = TRUE)
polBerlin$text <- gsub("…", " ", polBerlin$text, perl = TRUE)
polBerlin$text <- gsub("’", " ", polBerlin$text, perl = TRUE)
polBerlin$text <- gsub("‘", " ", polBerlin$text, perl = TRUE)
#
corpus_polBerlin <- Corpus(DataframeSource(polBerlin))
processedCorpus_polBerlin <- tm_map(corpus_polBerlin, removeWords, allstopwords)
processedCorpus_polBerlin <- tm_map(processedCorpus_polBerlin, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polBerlin <- tm_map(processedCorpus_polBerlin, stripWhitespace)

DTM_polBerlin <- DocumentTermMatrix(processedCorpus_polBerlin, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polBerlin)
DTM_polBerlin

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polBerlin) > 0
DTM_polBerlin <- DTM_polBerlin[sel_idx, ]
polBerlin <- polBerlin[sel_idx, ]

# find topic number
optimal_polBerlin <- ldatuning::FindTopicsNumber(
  DTM_polBerlin,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polBerlin, file = "CaoJuan-Deveaud_results/politikerBundesländer/BERLIN_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polBerlin)

optimal_polBerlinII <- ldatuning::FindTopicsNumber(
  DTM_polBerlin,
  topics = seq(from = 6, to = 14, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polBerlinII)

################################################################################ BRANDENBURG

# erste row muss von 1 bis max durchzählen für späteres modell
polBrandenburg <- polBrandenburg[,-1]
polBrandenburg <- cbind(doc_id = 1:nrow(polBrandenburg), polBrandenburg)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polBrandenburg$text <- gsub("http*(.*)", " ", polBrandenburg$text, perl = TRUE)
polBrandenburg$text <- gsub("https*(.*)", " ", polBrandenburg$text, perl = TRUE)
polBrandenburg$text <- gsub("@\\w+", " ", polBrandenburg$text, perl=TRUE)
polBrandenburg$text <- gsub("\\n", " ", polBrandenburg$text, perl = TRUE)
polBrandenburg$text <- gsub("lt3", " ", polBrandenburg$text, perl = TRUE)
polBrandenburg$text <- gsub("…", " ", polBrandenburg$text, perl = TRUE)
polBrandenburg$text <- gsub("’", " ", polBrandenburg$text, perl = TRUE)
polBrandenburg$text <- gsub("‘", " ", polBrandenburg$text, perl = TRUE)
#
corpus_polBrandenburg <- Corpus(DataframeSource(polBrandenburg))
processedCorpus_polBrandenburg <- tm_map(corpus_polBrandenburg, removeWords, allstopwords)
processedCorpus_polBrandenburg <- tm_map(processedCorpus_polBrandenburg, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polBrandenburg <- tm_map(processedCorpus_polBrandenburg, stripWhitespace)

DTM_polBrandenburg <- DocumentTermMatrix(processedCorpus_polBrandenburg, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polBrandenburg)
DTM_polBrandenburg

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polBrandenburg) > 0
DTM_polBrandenburg <- DTM_polBrandenburg[sel_idx, ]
polBrandenburg <- polBrandenburg[sel_idx, ]

# find topic number
optimal_polBrandenburg <- ldatuning::FindTopicsNumber(
  DTM_polBrandenburg,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polBrandenburg, file = "CaoJuan-Deveaud_results/politikerBundesländer/BRANDENBURG_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polBrandenburg)

optimal_polBrandenburgII <- ldatuning::FindTopicsNumber(
  DTM_polBrandenburg,
  topics = seq(from = 6, to = 14, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polBrandenburgII)

################################################################################ BREMEN

# erste row muss von 1 bis max durchzählen für späteres modell
polBremen <- polBremen[,-1]
polBremen <- cbind(doc_id = 1:nrow(polBremen), polBremen)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polBremen$text <- gsub("http*(.*)", " ", polBremen$text, perl = TRUE)
polBremen$text <- gsub("https*(.*)", " ", polBremen$text, perl = TRUE)
polBremen$text <- gsub("@\\w+", " ", polBremen$text, perl=TRUE)
polBremen$text <- gsub("\\n", " ", polBremen$text, perl = TRUE)
polBremen$text <- gsub("lt3", " ", polBremen$text, perl = TRUE)
polBremen$text <- gsub("…", " ", polBremen$text, perl = TRUE)
polBremen$text <- gsub("’", " ", polBremen$text, perl = TRUE)
polBremen$text <- gsub("‘", " ", polBremen$text, perl = TRUE)
#
corpus_polBremen <- Corpus(DataframeSource(polBremen))
processedCorpus_polBremen <- tm_map(corpus_polBremen, removeWords, allstopwords)
processedCorpus_polBremen <- tm_map(processedCorpus_polBremen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polBremen <- tm_map(processedCorpus_polBremen, stripWhitespace)

DTM_polBremen <- DocumentTermMatrix(processedCorpus_polBremen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polBremen)
DTM_polBremen

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polBremen) > 0
DTM_polBremen <- DTM_polBremen[sel_idx, ]
polBremen <- polBremen[sel_idx, ]

# find topic number
optimal_polBremen <- ldatuning::FindTopicsNumber(
  DTM_polBremen,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polBremen, file = "CaoJuan-Deveaud_results/politikerBundesländer/BREMEN_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polBremen)

optimal_polBremenII <- ldatuning::FindTopicsNumber(
  DTM_polBremen,
  topics = seq(from = 2, to = 6, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polBremenII)

################################################################################ HAMBURG

# erste row muss von 1 bis max durchzählen für späteres modell
polHamburg <- polHamburg[,-1]
polHamburg <- cbind(doc_id = 1:nrow(polHamburg), polHamburg)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polHamburg$text <- gsub("http*(.*)", " ", polHamburg$text, perl = TRUE)
polHamburg$text <- gsub("https*(.*)", " ", polHamburg$text, perl = TRUE)
polHamburg$text <- gsub("@\\w+", " ", polHamburg$text, perl=TRUE)
polHamburg$text <- gsub("\\n", " ", polHamburg$text, perl = TRUE)
polHamburg$text <- gsub("lt3", " ", polHamburg$text, perl = TRUE)
polHamburg$text <- gsub("…", " ", polHamburg$text, perl = TRUE)
polHamburg$text <- gsub("’", " ", polHamburg$text, perl = TRUE)
polHamburg$text <- gsub("‘", " ", polHamburg$text, perl = TRUE)
#
corpus_polHamburg <- Corpus(DataframeSource(polHamburg))
processedCorpus_polHamburg <- tm_map(corpus_polHamburg, removeWords, allstopwords)
processedCorpus_polHamburg <- tm_map(processedCorpus_polHamburg, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polHamburg <- tm_map(processedCorpus_polHamburg, stripWhitespace)

DTM_polHamburg <- DocumentTermMatrix(processedCorpus_polHamburg, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polHamburg)
DTM_polHamburg

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polHamburg) > 0
DTM_polHamburg <- DTM_polHamburg[sel_idx, ]
polHamburg <- polHamburg[sel_idx, ]

# find topic number
optimal_polHamburg <- ldatuning::FindTopicsNumber(
  DTM_polHamburg,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polHamburg, file = "CaoJuan-Deveaud_results/politikerBundesländer/HAMBURG_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polHamburg)

optimal_polHamburgII <- ldatuning::FindTopicsNumber(
  DTM_polHamburg,
  topics = seq(from = 6, to = 10, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polHamburgII)

################################################################################ HESSEN

# erste row muss von 1 bis max durchzählen für späteres modell
polHessen <- polHessen[,-1]
polHessen <- cbind(doc_id = 1:nrow(polHessen), polHessen)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polHessen$text <- gsub("http*(.*)", " ", polHessen$text, perl = TRUE)
polHessen$text <- gsub("https*(.*)", " ", polHessen$text, perl = TRUE)
polHessen$text <- gsub("@\\w+", " ", polHessen$text, perl=TRUE)
polHessen$text <- gsub("\\n", " ", polHessen$text, perl = TRUE)
polHessen$text <- gsub("lt3", " ", polHessen$text, perl = TRUE)
polHessen$text <- gsub("…", " ", polHessen$text, perl = TRUE)
polHessen$text <- gsub("’", " ", polHessen$text, perl = TRUE)
polHessen$text <- gsub("‘", " ", polHessen$text, perl = TRUE)
#
corpus_polHessen <- Corpus(DataframeSource(polHessen))
processedCorpus_polHessen <- tm_map(corpus_polHessen, removeWords, allstopwords)
processedCorpus_polHessen <- tm_map(processedCorpus_polHessen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polHessen <- tm_map(processedCorpus_polHessen, stripWhitespace)

DTM_polHessen <- DocumentTermMatrix(processedCorpus_polHessen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polHessen)
DTM_polHessen

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polHessen) > 0
DTM_polHessen <- DTM_polHessen[sel_idx, ]
polHessen <- polHessen[sel_idx, ]

# find topic number
optimal_polHessen <- ldatuning::FindTopicsNumber(
  DTM_polHessen,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polHessen, file = "CaoJuan-Deveaud_results/politikerBundesländer/HESSEN_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polHessen)

optimal_polHessenII <- ldatuning::FindTopicsNumber(
  DTM_polHessen,
  topics = seq(from = 6, to = 14, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polHessenII)

################################################################################ MECKLENBURG-VORPOMMERN

# erste row muss von 1 bis max durchzählen für späteres modell
polMeckPomm <- polMeckPomm[,-1]
polMeckPomm <- cbind(doc_id = 1:nrow(polMeckPomm), polMeckPomm)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polMeckPomm$text <- gsub("http*(.*)", " ", polMeckPomm$text, perl = TRUE)
polMeckPomm$text <- gsub("https*(.*)", " ", polMeckPomm$text, perl = TRUE)
polMeckPomm$text <- gsub("@\\w+", " ", polMeckPomm$text, perl=TRUE)
polMeckPomm$text <- gsub("\\n", " ", polMeckPomm$text, perl = TRUE)
polMeckPomm$text <- gsub("lt3", " ", polMeckPomm$text, perl = TRUE)
polMeckPomm$text <- gsub("…", " ", polMeckPomm$text, perl = TRUE)
polMeckPomm$text <- gsub("’", " ", polMeckPomm$text, perl = TRUE)
polMeckPomm$text <- gsub("‘", " ", polMeckPomm$text, perl = TRUE)
#
corpus_polMeckPomm <- Corpus(DataframeSource(polMeckPomm))
processedCorpus_polMeckPomm <- tm_map(corpus_polMeckPomm, removeWords, allstopwords)
processedCorpus_polMeckPomm <- tm_map(processedCorpus_polMeckPomm, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polMeckPomm <- tm_map(processedCorpus_polMeckPomm, stripWhitespace)

DTM_polMeckPomm <- DocumentTermMatrix(processedCorpus_polMeckPomm, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polMeckPomm)
DTM_polMeckPomm

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polMeckPomm) > 0
DTM_polMeckPomm <- DTM_polMeckPomm[sel_idx, ]
polMeckPomm <- polMeckPomm[sel_idx, ]

# find topic number
optimal_polMeckPomm <- ldatuning::FindTopicsNumber(
  DTM_polMeckPomm,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polMeckPomm, file = "CaoJuan-Deveaud_results/politikerBundesländer/MECKPOMM_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polMeckPomm)

optimal_polMeckPommII <- ldatuning::FindTopicsNumber(
  DTM_polMeckPomm,
  topics = seq(from = 2, to = 14, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polMeckPommII)

################################################################################ NIEDERSACHSEN

# erste row muss von 1 bis max durchzählen für späteres modell
polNds <- polNds[,-1]
polNds <- cbind(doc_id = 1:nrow(polNds), polNds)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polNds$text <- gsub("http*(.*)", " ", polNds$text, perl = TRUE)
polNds$text <- gsub("https*(.*)", " ", polNds$text, perl = TRUE)
polNds$text <- gsub("@\\w+", " ", polNds$text, perl=TRUE)
polNds$text <- gsub("\\n", " ", polNds$text, perl = TRUE)
polNds$text <- gsub("lt3", " ", polNds$text, perl = TRUE)
polNds$text <- gsub("…", " ", polNds$text, perl = TRUE)
polNds$text <- gsub("’", " ", polNds$text, perl = TRUE)
polNds$text <- gsub("‘", " ", polNds$text, perl = TRUE)
#
corpus_polNds <- Corpus(DataframeSource(polNds))
processedCorpus_polNds <- tm_map(corpus_polNds, removeWords, allstopwords)
processedCorpus_polNds <- tm_map(processedCorpus_polNds, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polNds <- tm_map(processedCorpus_polNds, stripWhitespace)

DTM_polNds <- DocumentTermMatrix(processedCorpus_polNds, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polNds)
DTM_polNds

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polNds) > 0
DTM_polNds <- DTM_polNds[sel_idx, ]
polNds <- polNds[sel_idx, ]

# find topic number
optimal_polNds <- ldatuning::FindTopicsNumber(
  DTM_polNds,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polNds, file = "CaoJuan-Deveaud_results/politikerBundesländer/NIEDERSACHSEN_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polNds)

optimal_polNdsII <- ldatuning::FindTopicsNumber(
  DTM_polNds,
  topics = seq(from = 6, to = 20, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polNdsII)

################################################################################ NORDRHEIN-WESTFALEN

# erste row muss von 1 bis max durchzählen für späteres modell
polNrw <- polNrw[,-1]
polNrw <- cbind(doc_id = 1:nrow(polNrw), polNrw)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polNrw$text <- gsub("http*(.*)", " ", polNrw$text, perl = TRUE)
polNrw$text <- gsub("https*(.*)", " ", polNrw$text, perl = TRUE)
polNrw$text <- gsub("@\\w+", " ", polNrw$text, perl=TRUE)
polNrw$text <- gsub("\\n", " ", polNrw$text, perl = TRUE)
polNrw$text <- gsub("lt3", " ", polNrw$text, perl = TRUE)
polNrw$text <- gsub("…", " ", polNrw$text, perl = TRUE)
polNrw$text <- gsub("’", " ", polNrw$text, perl = TRUE)
polNrw$text <- gsub("‘", " ", polNrw$text, perl = TRUE)
#
corpus_polNrw <- Corpus(DataframeSource(polNrw))
processedCorpus_polNrw <- tm_map(corpus_polNrw, removeWords, allstopwords)
processedCorpus_polNrw <- tm_map(processedCorpus_polNrw, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polNrw <- tm_map(processedCorpus_polNrw, stripWhitespace)

DTM_polNrw <- DocumentTermMatrix(processedCorpus_polNrw, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polNrw)
DTM_polNrw

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polNrw) > 0
DTM_polNrw <- DTM_polNrw[sel_idx, ]
polNrw <- polNrw[sel_idx, ]

# find topic number
optimal_polNrw <- ldatuning::FindTopicsNumber(
  DTM_polNrw,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polNrw, file = "CaoJuan-Deveaud_results/politikerBundesländer/NRW_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polNrw)

optimal_polNrwII <- ldatuning::FindTopicsNumber(
  DTM_polNrw,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polNrwII)

################################################################################ RHEINLAND-PFALZ

# erste row muss von 1 bis max durchzählen für späteres modell
polRlp <- polRlp[,-1]
polRlp <- cbind(doc_id = 1:nrow(polRlp), polRlp)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polRlp$text <- gsub("http*(.*)", " ", polRlp$text, perl = TRUE)
polRlp$text <- gsub("https*(.*)", " ", polRlp$text, perl = TRUE)
polRlp$text <- gsub("@\\w+", " ", polRlp$text, perl=TRUE)
polRlp$text <- gsub("\\n", " ", polRlp$text, perl = TRUE)
polRlp$text <- gsub("lt3", " ", polRlp$text, perl = TRUE)
polRlp$text <- gsub("…", " ", polRlp$text, perl = TRUE)
polRlp$text <- gsub("’", " ", polRlp$text, perl = TRUE)
polRlp$text <- gsub("‘", " ", polRlp$text, perl = TRUE)
#
corpus_polRlp <- Corpus(DataframeSource(polRlp))
processedCorpus_polRlp <- tm_map(corpus_polRlp, removeWords, allstopwords)
processedCorpus_polRlp <- tm_map(processedCorpus_polRlp, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polRlp <- tm_map(processedCorpus_polRlp, stripWhitespace)

DTM_polRlp <- DocumentTermMatrix(processedCorpus_polRlp, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polRlp)
DTM_polRlp

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polRlp) > 0
DTM_polRlp <- DTM_polRlp[sel_idx, ]
polRlp <- polRlp[sel_idx, ]

# find topic number
optimal_polRlp <- ldatuning::FindTopicsNumber(
  DTM_polRlp,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polRlp, file = "CaoJuan-Deveaud_results/politikerBundesländer/RHEINLAND_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polRlp)

optimal_polRlpII <- ldatuning::FindTopicsNumber(
  DTM_polRlp,
  topics = seq(from = 2, to = 14, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polRlpII)

################################################################################ SAARLAND

# erste row muss von 1 bis max durchzählen für späteres modell
polSaar <- polSaar[,-1]
polSaar <- cbind(doc_id = 1:nrow(polSaar), polSaar)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polSaar$text <- gsub("http*(.*)", " ", polSaar$text, perl = TRUE)
polSaar$text <- gsub("https*(.*)", " ", polSaar$text, perl = TRUE)
polSaar$text <- gsub("@\\w+", " ", polSaar$text, perl=TRUE)
polSaar$text <- gsub("\\n", " ", polSaar$text, perl = TRUE)
polSaar$text <- gsub("lt3", " ", polSaar$text, perl = TRUE)
polSaar$text <- gsub("…", " ", polSaar$text, perl = TRUE)
polSaar$text <- gsub("’", " ", polSaar$text, perl = TRUE)
polSaar$text <- gsub("‘", " ", polSaar$text, perl = TRUE)
#
corpus_polSaar <- Corpus(DataframeSource(polSaar))
processedCorpus_polSaar <- tm_map(corpus_polSaar, removeWords, allstopwords)
processedCorpus_polSaar <- tm_map(processedCorpus_polSaar, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polSaar <- tm_map(processedCorpus_polSaar, stripWhitespace)

DTM_polSaar <- DocumentTermMatrix(processedCorpus_polSaar, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polSaar)
DTM_polSaar

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polSaar) > 0
DTM_polSaar <- DTM_polSaar[sel_idx, ]
polSaar <- polSaar[sel_idx, ]

# find topic number
optimal_polSaar <- ldatuning::FindTopicsNumber(
  DTM_polSaar,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polSaar, file = "CaoJuan-Deveaud_results/politikerBundesländer/SAARLAND_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polSaar)

optimal_polSaarII <- ldatuning::FindTopicsNumber(
  DTM_polSaar,
  topics = seq(from = 2, to = 18, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polSaarII)

################################################################################ SACHSEN

# erste row muss von 1 bis max durchzählen für späteres modell
polSachsen <- polSachsen[,-1]
polSachsen <- cbind(doc_id = 1:nrow(polSachsen), polSachsen)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polSachsen$text <- gsub("http*(.*)", " ", polSachsen$text, perl = TRUE)
polSachsen$text <- gsub("https*(.*)", " ", polSachsen$text, perl = TRUE)
polSachsen$text <- gsub("@\\w+", " ", polSachsen$text, perl=TRUE)
polSachsen$text <- gsub("\\n", " ", polSachsen$text, perl = TRUE)
polSachsen$text <- gsub("lt3", " ", polSachsen$text, perl = TRUE)
polSachsen$text <- gsub("…", " ", polSachsen$text, perl = TRUE)
polSachsen$text <- gsub("’", " ", polSachsen$text, perl = TRUE)
polSachsen$text <- gsub("‘", " ", polSachsen$text, perl = TRUE)
#
corpus_polSachsen <- Corpus(DataframeSource(polSachsen))
processedCorpus_polSachsen <- tm_map(corpus_polSachsen, removeWords, allstopwords)
processedCorpus_polSachsen <- tm_map(processedCorpus_polSachsen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polSachsen <- tm_map(processedCorpus_polSachsen, stripWhitespace)

DTM_polSachsen <- DocumentTermMatrix(processedCorpus_polSachsen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polSachsen)
DTM_polSachsen

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polSachsen) > 0
DTM_polSachsen <- DTM_polSachsen[sel_idx, ]
polSachsen <- polSachsen[sel_idx, ]

# find topic number
optimal_polSachsen <- ldatuning::FindTopicsNumber(
  DTM_polSachsen,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polSachsen, file = "CaoJuan-Deveaud_results/politikerBundesländer/SACHSEN_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polSachsen)

optimal_polSachsenII <- ldatuning::FindTopicsNumber(
  DTM_polSachsen,
  topics = seq(from = 6, to = 18, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polSachsenII)

################################################################################ SACHSEN-ANHALT

# erste row muss von 1 bis max durchzählen für späteres modell
polSachsenAnhalt <- polSachsenAnhalt[,-1]
polSachsenAnhalt <- cbind(doc_id = 1:nrow(polSachsenAnhalt), polSachsenAnhalt)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polSachsenAnhalt$text <- gsub("http*(.*)", " ", polSachsenAnhalt$text, perl = TRUE)
polSachsenAnhalt$text <- gsub("https*(.*)", " ", polSachsenAnhalt$text, perl = TRUE)
polSachsenAnhalt$text <- gsub("@\\w+", " ", polSachsenAnhalt$text, perl=TRUE)
polSachsenAnhalt$text <- gsub("\\n", " ", polSachsenAnhalt$text, perl = TRUE)
polSachsenAnhalt$text <- gsub("lt3", " ", polSachsenAnhalt$text, perl = TRUE)
polSachsenAnhalt$text <- gsub("…", " ", polSachsenAnhalt$text, perl = TRUE)
polSachsenAnhalt$text <- gsub("’", " ", polSachsenAnhalt$text, perl = TRUE)
polSachsenAnhalt$text <- gsub("‘", " ", polSachsenAnhalt$text, perl = TRUE)
#
corpus_polSachsenAnhalt <- Corpus(DataframeSource(polSachsenAnhalt))
processedCorpus_polSachsenAnhalt <- tm_map(corpus_polSachsenAnhalt, removeWords, allstopwords)
processedCorpus_polSachsenAnhalt <- tm_map(processedCorpus_polSachsenAnhalt, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polSachsenAnhalt <- tm_map(processedCorpus_polSachsenAnhalt, stripWhitespace)

DTM_polSachsenAnhalt <- DocumentTermMatrix(processedCorpus_polSachsenAnhalt, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polSachsenAnhalt)
DTM_polSachsenAnhalt

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polSachsenAnhalt) > 0
DTM_polSachsenAnhalt <- DTM_polSachsenAnhalt[sel_idx, ]
polSachsenAnhalt <- polSachsenAnhalt[sel_idx, ]

# find topic number
optimal_polSachsenAnhalt <- ldatuning::FindTopicsNumber(
  DTM_polSachsenAnhalt,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polSachsenAnhalt, file = "CaoJuan-Deveaud_results/politikerBundesländer/SACHSENANHALT_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polSachsenAnhalt)

optimal_polSachsenAnhaltII <- ldatuning::FindTopicsNumber(
  DTM_polSachsenAnhalt,
  topics = seq(from = 2, to = 14, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polSachsenAnhaltII)

################################################################################ SCHLESWIG-HOLSTEIN

# erste row muss von 1 bis max durchzählen für späteres modell
polSchleswig <- polSchleswig[,-1]
polSchleswig <- cbind(doc_id = 1:nrow(polSchleswig), polSchleswig)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polSchleswig$text <- gsub("http*(.*)", " ", polSchleswig$text, perl = TRUE)
polSchleswig$text <- gsub("https*(.*)", " ", polSchleswig$text, perl = TRUE)
polSchleswig$text <- gsub("@\\w+", " ", polSchleswig$text, perl=TRUE)
polSchleswig$text <- gsub("\\n", " ", polSchleswig$text, perl = TRUE)
polSchleswig$text <- gsub("lt3", " ", polSchleswig$text, perl = TRUE)
polSchleswig$text <- gsub("…", " ", polSchleswig$text, perl = TRUE)
polSchleswig$text <- gsub("’", " ", polSchleswig$text, perl = TRUE)
polSchleswig$text <- gsub("‘", " ", polSchleswig$text, perl = TRUE)
#
corpus_polSchleswig <- Corpus(DataframeSource(polSchleswig))
processedCorpus_polSchleswig <- tm_map(corpus_polSchleswig, removeWords, allstopwords)
processedCorpus_polSchleswig <- tm_map(processedCorpus_polSchleswig, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polSchleswig <- tm_map(processedCorpus_polSchleswig, stripWhitespace)

DTM_polSchleswig <- DocumentTermMatrix(processedCorpus_polSchleswig, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polSchleswig)
DTM_polSchleswig

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polSchleswig) > 0
DTM_polSchleswig <- DTM_polSchleswig[sel_idx, ]
polSchleswig <- polSchleswig[sel_idx, ]

# find topic number
optimal_polSchleswig <- ldatuning::FindTopicsNumber(
  DTM_polSchleswig,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polSchleswig, file = "CaoJuan-Deveaud_results/politikerBundesländer/SCHLESWIG_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polSchleswig)

optimal_polSchleswigII <- ldatuning::FindTopicsNumber(
  DTM_polSchleswig,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polSchleswigII)

################################################################################ THÜRINGEN

# erste row muss von 1 bis max durchzählen für späteres modell
polThüringen <- polThüringen[,-1]
polThüringen <- cbind(doc_id = 1:nrow(polThüringen), polThüringen)

# bearbeitung mittels regex: unnötige und unpassende pattern entfernen
polThüringen$text <- gsub("http*(.*)", " ", polThüringen$text, perl = TRUE)
polThüringen$text <- gsub("https*(.*)", " ", polThüringen$text, perl = TRUE)
polThüringen$text <- gsub("@\\w+", " ", polThüringen$text, perl=TRUE)
polThüringen$text <- gsub("\\n", " ", polThüringen$text, perl = TRUE)
polThüringen$text <- gsub("lt3", " ", polThüringen$text, perl = TRUE)
polThüringen$text <- gsub("…", " ", polThüringen$text, perl = TRUE)
polThüringen$text <- gsub("’", " ", polThüringen$text, perl = TRUE)
polThüringen$text <- gsub("‘", " ", polThüringen$text, perl = TRUE)
#
corpus_polThüringen <- Corpus(DataframeSource(polThüringen))
processedCorpus_polThüringen <- tm_map(corpus_polThüringen, removeWords, allstopwords)
processedCorpus_polThüringen <- tm_map(processedCorpus_polThüringen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_polThüringen <- tm_map(processedCorpus_polThüringen, stripWhitespace)

DTM_polThüringen <- DocumentTermMatrix(processedCorpus_polThüringen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# betrachtung der dtm
dim(DTM_polThüringen)
DTM_polThüringen

## entfernen inhaltsloser rows
sel_idx <- slam::row_sums(DTM_polThüringen) > 0
DTM_polThüringen <- DTM_polThüringen[sel_idx, ]
polThüringen <- polThüringen[sel_idx, ]

# find topic number
optimal_polThüringen <- ldatuning::FindTopicsNumber(
  DTM_polThüringen,
  topics = seq(from = 2, to = 45, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimal_polThüringen, file = "CaoJuan-Deveaud_results/politikerBundesländer/THÜRINGEN_politiker.RData")

x11()
FindTopicsNumber_plot(optimal_polThüringen)

optimal_polThüringenII <- ldatuning::FindTopicsNumber(
  DTM_polThüringen,
  topics = seq(from = 2, to = 14, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimal_polThüringenII)








