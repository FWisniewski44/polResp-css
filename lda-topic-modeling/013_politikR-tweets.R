# ==============================================================================
# ============================ PolitikerInnen ================================
# ==============================================================================

# ==============================================================================
# ========================== Versuch Concatenation =============================
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

# load("regionalisierung/rest/politikR_updated.RData")
# which(politikR$user == "StBrandner")
# tm_map(processedCorpus_pol, function(x) iconv(enc2utf8(x$content), sub = "byte"))

################################################################################

# eigene liste von stopwords
myStopwordsDE <- stopwords::stopwords(language = "de", source = "stopwords-iso")
myStopwordsDE <- append(myStopwordsDE, values = c("&lt;3", "amp", "lt3", "„",
                                                  "…", "“", "’", "\n"))

################################################################################
############################## ALTER CODE FOR REF ##############################
################################################################################

# ## A) erstellen des korpus
# politikerCorp <- corpus(politikR$content,
#                   docvars = data.frame(username = (politikR$user)))
# summary(politikerCorp)
# # summary(concatenate.documents(politikerCorp[1:147], politikerCorp[148:316]))
# # which(politikR$user == "Valentin_C_Abel")
# 
# ## B) vorbereiten für topic model
# politikerReshaped <- corpus_reshape(politikerCorp, to="documents")
# 
# ## C) matrix erschaffen aus dem ergebnis der tokenization
# politikerMatr <- tokens(politikerReshaped, verbose = T, what = "word", remove_punct = T,
#                   remove_symbols = T, remove_separators = T, remove_numbers = T,
#                   remove_url = T, split_hyphens = F, split_tags = T)
# politikerMatr[996]
# 
# ## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
# politikerNoStopDE <- tokens_select(politikerMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
# politikerNoStopDE_2 <- tokens_select(politikerNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
# politikerNoStopEN <- tokens_select(politikerNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
# politikerNoStop <- tokens_select(politikerNoStopEN, pattern = myStopwordsDE, selection = "remove")
# 
# ## E) document frequency matrix
# politikerDFM <- dfm(politikerNoStop, tolower = T, verbose = T, remove_padding = T)
# politikerDFM <- dfm_trim(politikerDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
# politikerDFM
# 
# ## F) document term matrix für tm erschaffen
# politikerDTM <- convert(politikerDFM, to = "topicmodels")
# summary(politikerDTM)


################################################################################

# test: ladal.edu.au
politikR <- read_csv("regionalisierung/demojized/politikR_demojized.csv")
politikR$text[2]

# ## für Corpus braucht der df einmal die doc_id column zum durchnummerieren und einmal die text column, die auch "text" heißen muss
politikR <- politikR[,-1]
politikR <- cbind(doc_id = 1:175017, politikR)
# politikR <- politikR %>% rename(text = content)

## url entfernen
politikR$text <- gsub("http*(.*)", "", politikR$text, perl = TRUE)
politikR$text <- gsub("https*(.*)", "", politikR$text, perl = TRUE)

## @-namen entfernen: teste das, da viele davon die listen "verschmutzen"
politikR$text <- gsub("@\\w+", "", politikR$text, perl=TRUE)

# zeichenkombinationen entfernen, die nicht mit den regex gruppen abgedeckt werden
# politikR$text <- chartr(old = "₀₁₂₃₄₅₆₇₈₉", new = "0123456789", politikR$text)

politikR$text <- gsub("\\n", "", politikR$text, perl = TRUE)
politikR$text <- gsub("lt3", "", politikR$text, perl = TRUE)

politikR$text <- gsub("…", "", politikR$text, perl = TRUE)
politikR$text <- gsub("–", "", politikR$text, perl = TRUE)

politikR$text <- gsub("„", "", politikR$text, perl = TRUE)
politikR$text <- gsub("“", "", politikR$text, perl = TRUE)

politikR$text <- gsub("’", "", politikR$text, perl = TRUE)
politikR$text <- gsub("‘", "", politikR$text, perl = TRUE)

## entferne gruppen: u. a. symbole (emojis; ohne replacement), Satzzeichen, (hoffentlich) Währungssymbole, ...
# \\p{Cn}|\\p{Ps}|\\p{Pe}|\\p{Pi}|\\p{Pf}|\\p{Po}|\\p{S}

# politikR$text <- gsub("\\p{Cn}|\\p{Ps}|\\p{Pe}|\\p{Pi}|\\p{Pf}|\\p{Po}|\\p{S}", "", politikR$text, perl = TRUE)
politikR$text[2]
politikR$text[43]
politikR$text[83918]
politikR$text[100948]
sample_überprüfung <- as_tibble(politikR[sample(nrow(politikR), 150), ])
# chartr(old = "₀₁₂₃₄₅₆₇₈₉", new = "0123456789", sample_überprüfung$text)
sample_überprüfung

# ## versuche des emoji replacements
# textclean::replace_emoji(politikR$text, emoji_dt = lexicon::hash_emojis)
# utf8::utf8_encode(textclean::replace_emoji_identifier(politikR$text[1:10], emoji_dt = lexicon::hash_emojis_identifier))
# 
# 
# string <- paste(c(letters[1:4], emoji_name[1:6]), collapse = " ")
# 
# emoji_replace_all(emoji_name[1:6], "_emoji_")
# 
# replace_non_ascii(replace_emoji(politikR$text))
# 
# emoji_replace(emoji_name[["ukraine"]], "ukraine")
# emoji::emoji_name[["ukraine"]]
# emoji::emoji_name[["ukraine"]]
# emoji::emoji_name[["de"]]
# 
# emojis <- as.data.frame(emoji::emoji_name)

## corpus object erstellen
corpusPol <- Corpus(DataframeSource(politikR))

## processing I: kleinbuchstaben, nicht-wörter (d. h. zeichenkombis usw. wie "..."), stopwords deutsch x2, english und eigene liste
# processedCorpus_pol <- tm_map(corpusPol, content_transformer(tolower)) ## in python erledigt

processedCorpus_pol <- tm_map(corpusPol, removeWords, stopwords::stopwords(language = "de", source = "marimo"))
processedCorpus_pol <- tm_map(processedCorpus_pol, removeWords, stopwords::stopwords(language = "de", source = "stopwords-iso"))
processedCorpus_pol <- tm_map(processedCorpus_pol, removeWords, stopwords::stopwords(language = "en", source = "stopwords-iso"))
processedCorpus_pol <- tm_map(processedCorpus_pol, removeWords, myStopwordsDE)

## processing II: satzzeichen raus, beibehalten von bindestrichen, leerzeichen raus --- nummern bleiben (9-euro-ticket z. B.), stemmer unsinnig
processedCorpus_pol <- tm_map(processedCorpus_pol, removePunctuation, preserve_intra_word_dashes = TRUE) ## in python erledigt
# processedCorpus_pol <- tm_map(processedCorpus_pol, removeNumbers)
# processedCorpus_pol <- tm_map(processedCorpus_pol, stemDocument, language = "de")
processedCorpus_pol <- tm_map(processedCorpus_pol, stripWhitespace) ## unsicher ob gebraucht, da keine absätze drin -- lass mal laufen lieber
# processedCorpus_pol <- rm_non_words(processedCorpus_pol)

# View(processedCorpus_pol)
# summary(processedCorpus_pol)

minimumFrequency <- 5
DTMpol <- DocumentTermMatrix(processedCorpus_pol, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMpol)
DTMpol

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMpol) > 0
DTMpol <- DTMpol[sel_idx, ]
politikR <- politikR[sel_idx, ]


# deswegen machen wir das: ideale topic-anzahl rausfinden lassen
## achtung: extrem ressourcen-intensive computation; evtl. mit mc.cores = 2L oder 3L machen, zwei bzw. drei kerne nutzen und
## unter low resource DE laufen lassen (xfce, enlightenment, bspwm)

resultPol_all <- ldatuning::FindTopicsNumber(
  DTMpol,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

x11()
FindTopicsNumber_plot(resultPol_all)
### 

save(resultPol_all, file = "CaoJuan-Deveaud_results/POLITIKR_alleKoeff_k2-150_10er.RData")
# load("CaoJuan-Deveaud_results/BAYERN_alleKoeff_k2-150_10er.RData")

# result_test_100 <- ldatuning::FindTopicsNumber(
#   DTMpol,
#   topics = seq(from = 40, to = 100, by = 10),
#   metrics = c("CaoJuan2009",  "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 1555),
#   mc.cores = 3L,
#   verbose = T
# )

# # save(result_test_100, file="CaoJuan-Deveaud_results/result_politiker_k100.RData")
# load("CaoJuan-Deveaud_results/result_politiker_k100.RData")
# 
# # save(result, file="results_CaoJuan2009_Deveaud2014.RData")
# load("CaoJuan-Deveaud_results/results_CaoJuan2009_Deveaud2014.RData")
# 
# #save(resultII, file="resultsII_CaoJuan2009_Deveaud2014.RData")
# load("CaoJuan-Deveaud_results/resultsII_CaoJuan2009_Deveaud2014.RData")

# x11()
# FindTopicsNumber_plot(result_test_100)
# x11()
# FindTopicsNumber_plot(resultII)
# x11()
# FindTopicsNumber_plot(result)

# modell laufen lassen
# number of topics
kAlt <- 12
kBer <- 60 ## auf basis von caojuan für K 40:100 // 60 als guter tradeoff identifizierbar, deshalb TEST
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

set.seed(0101)
topicModel_k60 <- LDA(DTMpol, kBer, method="Gibbs", control=list(iter = 2000, verbose = 25))
tmResult_k60 <- posterior(topicModel_k60)
terms(topicModel_k60, 25)


# # topics are probability distributions over the entire vocabulary
# beta <- tmResult3$terms   # get beta from results
# dim(beta)                # K distributions over nTerms(DTM) terms
# 
# # for every document we have a probability distribution of its contained topics
# theta <- tmResult3$topics 
# dim(theta)               # nDocs(DTM) distributions over K topics



top5termsPerTopic_k60 <- terms(topicModel_k60, 5)
topicNames_k60 <- apply(top5termsPerTopic_k60, 2, paste, collapse=" ")

# exampleIds <- c(15, 8570, 20000, 110000)
# lapply(corpusPol[exampleIds], as.character)
# 
# N <- length(exampleIds)
# # get topic proportions form example documents
# topicProportionExamples <- theta[exampleIds,]
# colnames(topicProportionExamples) <- topicNames
# vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
# ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
#   geom_bar(stat="identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
#   coord_flip() +
#   facet_wrap(~ document, ncol = N)
