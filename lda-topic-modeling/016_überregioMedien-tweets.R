# ==============================================================================
# ============================ ÜBERREGIONALE ===================================
# ==============================================================================

# ==============================================================================
# ========================== Themenfindung + Model =============================
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

################################################################################

# test: ladal.edu.au
überregional <- read_csv("regionalisierung/demojized/überregio_demojized.csv")
überregional$text[2]

# ## für Corpus braucht der df einmal die doc_id column zum durchnummerieren und einmal die text column, die auch "text" heißen muss
überregional <- überregional[,-1]
überregional <- cbind(doc_id = 1:303573, überregional)
# überregional <- überregional %>% rename(text = content)

## url entfernen
überregional$text <- gsub("http*(.*)", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("https*(.*)", " ", überregional$text, perl = TRUE)

## @-namen entfernen: teste das, da viele davon die listen "verschmutzen"
überregional$text <- gsub("@\\w+", " ", überregional$text, perl=TRUE)

# zeichenkombinationen entfernen, die nicht mit den regex gruppen abgedeckt werden
# überregional$text <- chartr(old = "₀₁₂₃₄₅₆₇₈₉", new = "0123456789", überregional$text)

überregional$text <- gsub("\\n", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("lt3", " ", überregional$text, perl = TRUE)

überregional$text <- gsub("…", " ", überregional$text, perl = TRUE)
# überregional$text <- gsub("–", "", überregional$text, perl = TRUE) ## in python mit re.sub gemacht für extra word hyphens und "–"

## folgende zwei schon in python gemacht:
# überregional$text <- gsub("„", "", überregional$text, perl = TRUE)
# überregional$text <- gsub("“", "", überregional$text, perl = TRUE)

überregional$text <- gsub("’", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("‘", " ", überregional$text, perl = TRUE)

## entferne gruppen: u. a. symbole (emojis; ohne replacement), Satzzeichen, (hoffentlich) Währungssymbole, ...
# \\p{Cn}|\\p{Ps}|\\p{Pe}|\\p{Pi}|\\p{Pf}|\\p{Po}|\\p{S}

# überregional$text <- gsub("\\p{Cn}|\\p{Ps}|\\p{Pe}|\\p{Pi}|\\p{Pf}|\\p{Po}|\\p{S}", "", überregional$text, perl = TRUE)
überregional$text[2]
überregional$text[43]
überregional$text[83918]
überregional$text[100948]
# sample_überprüfung <- as_tibble(überregional[sample(nrow(überregional), 150), ])
# # chartr(old = "₀₁₂₃₄₅₆₇₈₉", new = "0123456789", sample_überprüfung$text)
# sample_überprüfung

# ## versuche des emoji replacements
# textclean::replace_emoji(überregional$text, emoji_dt = lexicon::hash_emojis)
# utf8::utf8_encode(textclean::replace_emoji_identifier(überregional$text[1:10], emoji_dt = lexicon::hash_emojis_identifier))
# 
# 
# string <- paste(c(letters[1:4], emoji_name[1:6]), collapse = " ")
# 
# emoji_replace_all(emoji_name[1:6], "_emoji_")
# 
# replace_non_ascii(replace_emoji(überregional$text))
# 
# emoji_replace(emoji_name[["ukraine"]], "ukraine")
# emoji::emoji_name[["ukraine"]]
# emoji::emoji_name[["ukraine"]]
# emoji::emoji_name[["de"]]
# 
# emojis <- as.data.frame(emoji::emoji_name)

## corpus object erstellen
corpusÜberregional <- Corpus(DataframeSource(überregional))

## processing I: kleinbuchstaben, nicht-wörter (d. h. zeichenkombis usw. wie "..."), stopwords deutsch x2, english und eigene liste
# processedCorpus <- tm_map(corpusBay, content_transformer(tolower)) ## in python erledigt

# processedCorpus_überregional <- tm_map(corpusBay, removeWords, stopwords::stopwords(language = "de", source = "marimo"))
# processedCorpus_überregional <- tm_map(processedCorpus_überregional, removeWords, stopwords::stopwords(language = "de", source = "stopwords-iso"))
# processedCorpus_überregional <- tm_map(processedCorpus_überregional, removeWords, stopwords::stopwords(language = "en", source = "stopwords-iso"))
# processedCorpus_überregional <- tm_map(processedCorpus_überregional, removeWords, myStopwordsDE)
processedCorpus_überregional <- tm_map(corpusÜberregional, removeWords, allstopwords)

## processing II: satzzeichen raus, beibehalten von bindestrichen, leerzeichen raus --- nummern bleiben (9-euro-ticket z. B.), stemmer unsinnig
processedCorpus_überregional <- tm_map(processedCorpus_überregional, removePunctuation, preserve_intra_word_dashes = TRUE) ## in python erledigt
# processedCorpus <- tm_map(processedCorpus, removeNumbers)
# processedCorpus <- tm_map(processedCorpus, stemDocument, language = "de")
processedCorpus_überregional <- tm_map(processedCorpus_überregional, stripWhitespace) ## unsicher ob gebraucht, da keine absätze drin -- lass mal laufen lieber
# processedCorpus <- rm_non_words(processedCorpus)

pal <- RColorBrewer::brewer.pal(8, "Dark2")
x11()
wordcloud(processedCorpus_überregional, min.freq=10, max.words = 150, random.order = F, col = pal) # schaut sinnvoll aus von wörtern her

# View(processedCorpus)
# summary(processedCorpus)

minimumFrequency <- 5
DTMüberregional <- DocumentTermMatrix(processedCorpus_überregional, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMüberregional)
DTMüberregional

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMüberregional) > 0
DTMüberregional <- DTMüberregional[sel_idx, ]
überregional <- überregional[sel_idx, ]

################################################################################

# deswegen machen wir das: ideale topic-anzahl rausfinden lassen
## achtung: extrem ressourcen-intensive computation; evtl. mit mc.cores = 2L oder 3L machen, zwei bzw. drei kerne nutzen und
## unter low resource DE laufen lassen (xfce, enlightenment, bspwm)

# resultBay <- ldatuning::FindTopicsNumber(
#   DTMüberregional,
#   topics = seq(from = 2, to = 40, by = 1),
#   metrics = c("CaoJuan2009", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 1555),
#   mc.cores = 3L,
#   verbose = T
# )
# 
# # save(resultBay, file = "CaoJuan-Deveaud_results/resultBay_both_k40.RData")
# load("CaoJuan-Deveaud_results/resultBay_both_k40.RData")
# 
# x11()
# FindTopicsNumber_plot(resultBay)
# 
# ######################## // CaoJuan + Deveaud, Bayern, 40-300 Themen, 25er steps
# 
# resultBay_caoJuan_deveaud_300 <- ldatuning::FindTopicsNumber(
#   DTMüberregional,
#   topics = seq(from = 40, to = 300, by = 25),
#   metrics = c("CaoJuan2009", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 0011),
#   mc.cores = 3L,
#   verbose = T
# )
# 
# x11()
# FindTopicsNumber_plot(resultBay_caoJuan_deveaud_300)
# 
# save(resultBay_caoJuan_deveaud_300, file = "CaoJuan-Deveaud_results/BAYERN_caoJuan-deveaud_k40-300_25er.RData")
# # load("CaoJuan-Deveaud_results/BAYERN_caoJuan-deveaud_k40-100.RData")
# 
# ######################## // Arun2010 + Griffiths2004 – Bayern, 40-100 Themen, 10er steps
# 
# resultBay_arun_griffith_300 <- ldatuning::FindTopicsNumber(
#   DTMüberregional,
#   topics = seq(from = 40, to = 300, by = 25),
#   metrics = c("Arun2010", "Griffiths2004"),
#   method = "Gibbs",
#   control = list(seed = 1100),
#   mc.cores = 3L,
#   verbose = T
# )
# 
# x11()
# FindTopicsNumber_plot(resultBay_arun_griffith_300)
# 
# # save(resultBay_arun_griffith_300, file = "CaoJuan-Deveaud_results/BAYERN_arun-griffith_k40-300_25er.RData")
# load("CaoJuan-Deveaud_results/BAYERN_arun-griffith_k40-300_25er.RData")

################################################################################

## alle koeffizienten für k =2:150 (über nacht laufen lassen)

resultBay_all <- ldatuning::FindTopicsNumber(
  DTMüberregional,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(resultBay_all)
### spätestes maximum: 62 (deveaud dann bei ~0.5, caojuan: [0;0.25], ab 62 topics steigt caojuan stark an, was schlecht ist)
### ideal wäre wohl zw. 12 bis max. 42 Themen

save(resultBay_all, file = "CaoJuan-Deveaud_results/BAWÜ_alleKoeff_k2-150_10er.RData")
# load("CaoJuan-Deveaud_results/BAWÜ_alleKoeff_k2-150_10er.RData")

# modell laufen lassen
# number of topics
# # set random number generator seed
# set.seed(9161)
# # compute the LDA model, inference via 1000 iterations of Gibbs sampling
# topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 2000, verbose = 25))
# 
# set.seed(8888)
# topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 2000, verbose = 25))

# topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 2000, verbose = 25))

# tmResult <- posterior(topicModel)
# 
# tmResult2 <- posterior(topicModel2)

################################################################################
# 12-42; spätestes 62; ideales wohl ca. 22 --- basierend auf caojuan und deveaud

kMin <- 12
kMax <- 42
kMedium <- 22

################

set.seed(3)
topicModel_kMin <- LDA(DTMüberregional, kMin, method="Gibbs", control=list(iter = 500, verbose = 10, best=T, alpha=0.01)) ## iter default = 2000, 500 für schneller
tmResultMin <- posterior(topicModel_kMin)
terms(topicModel_kMin, 10)

################

set.seed(4)
topicModel_kMedium <- LDA(DTMüberregional, kMedium, method="Gibbs", control=list(iter = 500, verbose = 10, best=T, alpha=0.01)) ## iter default = 2000, 500 für schneller
tmResultMedium <- posterior(topicModel_kMedium)
terms(topicModel_kMedium, 10)

tmResultMedium_df <- as_tibble(tmResultMedium$topics)
tmResultMedium_df <- cbind(doc_id = 1:105823, tmResultMedium_df)
überregional_zuordnungTest <- full_join(x=überregional, y=tmResultMedium_df, by="doc_id")

################

set.seed(5)
topicModel_kMax <- LDA(DTMüberregional, kMax, method="Gibbs", control=list(iter = 500, verbose = 10, best=T, alpha=0.01)) ## iter default = 2000, 500 für schneller
tmResultMax <- posterior(topicModel_kMax)
terms(topicModel_kMax, 10)

################

# topics are probability distributions over the entire vocabulary
beta <- tmResult3$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms

# for every document we have a probability distribution of its contained topics
theta <- tmResult3$topics 
dim(theta)               # nDocs(DTM) distributions over K topics



top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")