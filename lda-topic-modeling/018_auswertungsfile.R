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

################################################################################

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

################################################################################

# funktion für batch load von allen dateien auf einmal im ordner
filelist <- list.files(path = "CaoJuan-Deveaud_results/", pattern=".RData", full.names = T)
for (i in filelist){
  load(paste(i))
}

################################################################################

# optimalMecklenburgRef <- ldatuning::FindTopicsNumber(
#   DTMmecklenburg,
#   topics = seq(from = 12, to = 42, by = 1),
#   metrics = c("CaoJuan2009", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 5445, verbose=10, iter=500),
#   mc.cores = 3L,
#   verbose = T
# )
# 
# x11()
# FindTopicsNumber_plot(optimalMecklenburgRef)

################################################################################

### baden-württemberg

x11()
FindTopicsNumber_plot(optimalBaden)

optimalBadenRef <- ldatuning::FindTopicsNumber(
  DTMbaden,
  topics = seq(from = 12, to = 42, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 102030, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalBadenRef)

### bayern

x11()
FindTopicsNumber_plot(resultBay_all)

optimalBayRnRef <- ldatuning::FindTopicsNumber(
  DTMbayRn,
  topics = seq(from = 12, to = 62, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 55555, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalBayRnRef)

### berlin

x11()
FindTopicsNumber_plot(optimalBerl)

optimalBerlRef <- ldatuning::FindTopicsNumber(
  DTMberl,
  topics = seq(from = 2, to = 22, by = 2),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1221, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalBerlRef)

### brandenburg

x11()
FindTopicsNumber_plot(optimalBran)

optimalBranRef <- ldatuning::FindTopicsNumber(
  DTMbran,
  topics = seq(from = 2, to = 42, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1221, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalBranRef)

### bremen

x11()
FindTopicsNumber_plot(optimalBremen)

optimalBremenRef <- ldatuning::FindTopicsNumber(
  DTMbremen,
  topics = seq(from = 2, to = 12, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1303, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalBremenRef)

### hamburg

x11()
FindTopicsNumber_plot(optimalHamburg)

optimalHamburgRef <- ldatuning::FindTopicsNumber(
  DTMhamburg,
  topics = seq(from = 12, to = 42, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1404, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalHamburgRef)

### hessen

x11()
FindTopicsNumber_plot(optimalHessen)

optimalHessenRef <- ldatuning::FindTopicsNumber(
  DTMhessen,
  topics = seq(from = 12, to = 42, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1505, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalHessenRef)

### mecklenburg-vorpommern

x11()
FindTopicsNumber_plot(optimalMecklenburg)

optimalMecklenburgRef <- ldatuning::FindTopicsNumber(
  DTMmecklenburg,
  topics = seq(from = 12, to = 36, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1606, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalMecklenburgRef)

### niedersachsen

x11()
FindTopicsNumber_plot(optimalNiedersachsen)

optimalNiedersachsenRef <- ldatuning::FindTopicsNumber(
  DTMniedersachsen,
  topics = seq(from = 12, to = 52, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1707, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalNiedersachsenRef)

### nordrhein-westfalen

x11()
FindTopicsNumber_plot(optimalNordrhein)

optimalNordrheinRef <- ldatuning::FindTopicsNumber(
  DTMnordrhein,
  topics = seq(from = 40, to = 65, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1808, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalNordrheinRef)

### Rheinland-Pfalz

x11()
FindTopicsNumber_plot(optimalRheinland)

optimalRheinlandRef <- ldatuning::FindTopicsNumber(
  DTMrheinland,
  topics = seq(from = 12, to = 32, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1909, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalRheinlandRef)

### saarland

x11()
FindTopicsNumber_plot(optimalSaarland)

optimalSaarlandRef <- ldatuning::FindTopicsNumber(
  DTMsaarland,
  topics = seq(from = 12, to = 42, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2020, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalSaarlandRef)

### sachsen

x11()
FindTopicsNumber_plot(optimalSachsen)

optimalSachsenRef <- ldatuning::FindTopicsNumber(
  DTMsachsen,
  topics = seq(from = 12, to = 32, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1808, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalSachsenRef)

### sachsen-anhalt

x11()
FindTopicsNumber_plot(optimalSachsenAnhalt)

optimalSachsenAnhaltRef <- ldatuning::FindTopicsNumber(
  DTMsachsenAnhalt,
  topics = seq(from = 12, to = 22, by = 2),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1808, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalSachsenAnhaltRef)

### schleswig-holstein

x11()
FindTopicsNumber_plot(optimalSchlesHolst)

optimalSchlesHolstRef <- ldatuning::FindTopicsNumber(
  DTMschlesHolst,
  topics = seq(from = 12, to = 42, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1808, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalSchlesHolstRef)

### thüringen

x11()
FindTopicsNumber_plot(optimalThüringen)

optimalThüringenRef <- ldatuning::FindTopicsNumber(
  DTMthüringen,
  topics = seq(from = 12, to = 22, by = 2),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1808, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalThüringenRef)

### politiker

load("CaoJuan-Deveaud_results/POLITIKER_alleKoeff_k2-150_10er.RData")
x11()
FindTopicsNumber_plot(optimalPolitikR)

optimalPolitikerRef <- ldatuning::FindTopicsNumber(
  DTMpolitikR,
  topics = seq(from = 22, to = 62, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 8181, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalPolitikerRef)

optimalPolitikerRefII <- ldatuning::FindTopicsNumber(
  DTMpolitikR,
  topics = seq(from = 37, to = 42, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 9, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalPolitikerRefII)

### digitale

load("CaoJuan-Deveaud_results/DIGITALE_alleKoeff_k2-150_10er.RData")
x11()
FindTopicsNumber_plot(optimalDigitale)

optimalDigitaleRef <- ldatuning::FindTopicsNumber(
  DTMdigitale,
  topics = seq(from = 22, to = 72, by = 8),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 8282, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalDigitaleRef)

optimalDigitaleRefII <- ldatuning::FindTopicsNumber(
  DTMdigitale,
  topics = seq(from = 30, to = 46, by = 2),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 7, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalDigitaleRefII)

### öffentlich-rechtliche

load("CaoJuan-Deveaud_results/ÖFFENTLICHER_alleKoeff_k2-150_10er.RData")
x11()
FindTopicsNumber_plot(optimalÖffentlicher)

optimalÖffentlicherRef <- ldatuning::FindTopicsNumber(
  DTMöffentlicher,
  topics = seq(from = 22, to = 62, by = 5),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 3, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalÖffentlicherRef)

optimalÖffentlicherRefII <- ldatuning::FindTopicsNumber(
  DTMöffentlicher,
  topics = seq(from = 32, to = 37, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 6, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalÖffentlicherRefII)

### überregionale

load("CaoJuan-Deveaud_results/ÜBERREGIONAL_alleKoeff_k2-150_10er.RData")
x11()
FindTopicsNumber_plot(optimalÜberregional)

optimalÜberregionaleRef <- ldatuning::FindTopicsNumber(
  DTMüberregional,
  topics = seq(from = 32, to = 62, by = 4),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 42, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalÜberregionaleRef)

optimalÜberregionaleRefII <- ldatuning::FindTopicsNumber(
  DTMüberregional,
  topics = seq(from = 40, to = 44, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 28, iter=500),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(optimalÜberregionaleRefII)
