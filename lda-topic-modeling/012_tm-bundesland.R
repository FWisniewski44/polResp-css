# ==============================================================================
# ============================ REGIONALE MEDIEN ================================
# ==============================================================================

# ==============================================================================
# ====================== TM pro Bundesland =========================
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

# if needed, set working directory
setwd(dir="~/Documents/uni/masterarbeit/methods/representative-speech/")

load(file = "regionalisierung/bawü_discPow.RData")
load(file = "regionalisierung/bayern_discPow.RData")
load(file = "regionalisierung/berlin_discPow.RData")
load(file = "regionalisierung/brandenburg_discPow.RData")

################################################################################

# eigene liste von stopwords
myStopwordsDE <- stopwords::stopwords(language = "de", source = "stopwords-iso")
myStopwordsDE <- append(myStopwordsDE, values = c("&lt;3", "amp"))

################################################################################
################################################################################

# BaWü
## A) erstellen des korpus
baWüCorp <- corpus(bawü$content,
                   docvars = data.frame(username = (bawü$user)),
                   docnames = bawü$tweetID)
summary(baWüCorp)

## B) vorbereiten für topic model
baWüReshaped <- corpus_reshape(baWüCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
baWüMatr <- tokens(baWüReshaped, verbose = T, what = "word", remove_punct = T,
                    remove_symbols = T, remove_separators = T, remove_numbers = T,
                    remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
baWüNoStopDE <- tokens_select(baWüMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
baWüNoStopDE_2 <- tokens_select(baWüNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
baWüNoStopEN <- tokens_select(baWüNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
baWüNoStop <- tokens_select(baWüNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
baWüDFM <- dfm(baWüNoStop, tolower = T, verbose = T, remove_padding = T)
baWüDFM <- dfm_trim(baWüDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
baWüDFM

## F) document term matrix für tm erschaffen
baWüDTM <- convert(baWüDFM, to = "topicmodels")
baWüDTM

################################################################################

# MODELL 1
bawü1 <- LDA(baWüDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(bawü1, file = "lda_saves/bawü1.RData")

# MODELL 2
bawü2 <- LDA(baWüDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(bawü2, file = "lda_saves/bawü2.RData")

# MODELL 3
bawü3 <- LDA(baWüDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(bawü3, file = "lda_saves/bawü3.RData")

# MODELL 4
bawü4 <- LDA(baWüDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(bawü4, file = "lda_saves/bawü4.RData")

### posterior-ergebnisse abspeichern
post_bawü1 <- posterior(bawü1)
post_bawü2 <- posterior(bawü2)
post_bawü3 <- posterior(bawü3)
post_bawü4 <- posterior(bawü4)

terms(bawü1, 20)
terms(bawü2, 20)
terms(bawü3, 20)
terms(bawü4, 20)

################################################################################
################################################################################

# bayern

## A) erstellen des korpus
bayCorp <- corpus(bayern$content,
                   docvars = data.frame(username = (bayern$user)))
summary(bayCorp)

## B) vorbereiten für topic model
bayReshaped <- corpus_reshape(bayCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
bayMatr <- tokens(bayReshaped, verbose = T, what = "word", remove_punct = T,
                   remove_symbols = T, remove_separators = T, remove_numbers = T,
                   remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
bayNoStopDE <- tokens_select(bayMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
bayNoStopDE_2 <- tokens_select(bayNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
bayNoStopEN <- tokens_select(bayNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
bayNoStop <- tokens_select(bayNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
bayDFM <- dfm(bayNoStop, tolower = T, verbose = T, remove_padding = T)
bayDFM <- dfm_trim(bayDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
bayDFM

## F) document term matrix für tm erschaffen
bayDTM <- convert(bayDFM, to = "topicmodels")
bayDTM

################################################################################

# MODELL 1
bay1 <- LDA(bayDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(bay1, file = "lda_saves/bay1.RData")

# MODELL 2
bay2 <- LDA(bayDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(bay2, file = "lda_saves/bay2.RData")

# MODELL 3
bay3 <- LDA(bayDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(bay3, file = "lda_saves/bay3.RData")

# MODELL 4
bay4 <- LDA(bayDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(bay4, file = "lda_saves/bay4.RData")

### posterior-ergebnisse abspeichern
post_bay1 <- posterior(bay1)
post_bay2 <- posterior(bay2)
post_bay3 <- posterior(bay3)
post_bay4 <- posterior(bay4)

terms(bay1, 20)
terms(bay2, 20)
terms(bay3, 20)
terms(bay4, 20)

################################################################################

# berlin

## A) erstellen des korpus
berCorp <- corpus(berlin$content,
                  docvars = data.frame(username = (berlin$user)))
summary(berCorp)

## B) vorbereiten für topic model
berReshaped <- corpus_reshape(berCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
berMatr <- tokens(berReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
berNoStopDE <- tokens_select(berMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
berNoStopDE_2 <- tokens_select(berNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
berNoStopEN <- tokens_select(berNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
berNoStop <- tokens_select(berNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
berDFM <- dfm(berNoStop, tolower = T, verbose = T, remove_padding = T)
berDFM <- dfm_trim(berDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
berDFM

## F) document term matrix für tm erschaffen
berDTM <- convert(berDFM, to = "topicmodels")
berDTM

################################################################################

# MODELL 1
ber1 <- LDA(berDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(ber1, file = "lda_saves/ber1.RData")

# MODELL 2
ber2 <- LDA(berDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(ber2, file = "lda_saves/ber2.RData")

# MODELL 3
ber3 <- LDA(berDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(ber3, file = "lda_saves/ber3.RData")

# MODELL 4
ber4 <- LDA(berDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(ber4, file = "lda_saves/ber4.RData")

### posterior-ergebnisse abspeichern
post_ber1 <- posterior(ber1)
post_ber2 <- posterior(ber2)
post_ber3 <- posterior(ber3)
post_ber4 <- posterior(ber4)

terms(ber1, 20)
terms(ber2, 20)
terms(ber3, 20)
terms(ber4, 20)

################################################################################

# brandenburg

## A) erstellen des korpus
braCorp <- corpus(brandenburg$content,
                  docvars = data.frame(username = (brandenburg$user)))
summary(braCorp)

## B) vorbereiten für topic model
braReshaped <- corpus_reshape(braCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
braMatr <- tokens(braReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
braNoStopDE <- tokens_select(braMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
braNoStopDE_2 <- tokens_select(braNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
braNoStopEN <- tokens_select(braNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
braNoStop <- tokens_select(braNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
braDFM <- dfm(braNoStop, tolower = T, verbose = T, remove_padding = T)
braDFM <- dfm_trim(braDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
braDFM

## F) document term matrix für tm erschaffen
braDTM <- convert(braDFM, to = "topicmodels")
braDTM

################################################################################

# MODELL 1
bra1 <- LDA(braDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(bra1, file = "lda_saves/bra1.RData")

# MODELL 2
bra2 <- LDA(braDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(bra2, file = "lda_saves/bra2.RData")

# MODELL 3
bra3 <- LDA(braDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(bra3, file = "lda_saves/bra3.RData")

# MODELL 4
bra4 <- LDA(braDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(bra4, file = "lda_saves/bra4.RData")

### posterior-ergebnisse abspeichern
post_bra1 <- posterior(bra1)
post_bra2 <- posterior(bra2)
post_bra3 <- posterior(bra3)
post_bra4 <- posterior(bra4)

terms(bra1, 20)
terms(bra2, 20)
terms(bra3, 20)
terms(bra4, 20)

################################################################################

# bremen

load(file = "regionalisierung/bremen_discPow.RData")

## A) erstellen des korpus
brmCorp <- corpus(bremen$content,
                  docvars = data.frame(username = (bremen$user)))
summary(brmCorp)

## B) vorbereiten für topic model
brmReshaped <- corpus_reshape(brmCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
brmMatr <- tokens(brmReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
brmNoStopDE <- tokens_select(brmMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
brmNoStopDE_2 <- tokens_select(brmNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
brmNoStopEN <- tokens_select(brmNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
brmNoStop <- tokens_select(brmNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
brmDFM <- dfm(brmNoStop, tolower = T, verbose = T, remove_padding = T)
brmDFM <- dfm_trim(brmDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
brmDFM

## F) document term matrix für tm erschaffen
brmDTM <- convert(brmDFM, to = "topicmodels")
brmDTM

################################################################################

# MODELL 1
brm1 <- LDA(brmDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(brm1, file = "lda_saves/brm1.RData")

# MODELL 2
brm2 <- LDA(brmDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(brm2, file = "lda_saves/brm2.RData")

# MODELL 3
brm3 <- LDA(brmDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(brm3, file = "lda_saves/brm3.RData")

# MODELL 4
brm4 <- LDA(brmDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(brm4, file = "lda_saves/brm4.RData")

### posterior-ergebnisse abspeichern
post_brm1 <- posterior(brm1)
post_brm2 <- posterior(brm2)
post_brm3 <- posterior(brm3)
post_brm4 <- posterior(brm4)

terms(brm1, 20)
terms(brm2, 20)
terms(brm3, 20)
terms(brm4, 20)

################################################################################

# hamburg

load(file = "regionalisierung/hamburg_discPow.RData")

## A) erstellen des korpus
hhCorp <- corpus(hamburg$content,
                  docvars = data.frame(username = (hamburg$user)))
summary(hhCorp)

## B) vorbereiten für topic model
hhReshaped <- corpus_reshape(hhCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
hhMatr <- tokens(hhReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
hhNoStopDE <- tokens_select(hhMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
hhNoStopDE_2 <- tokens_select(hhNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
hhNoStopEN <- tokens_select(hhNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
hhNoStop <- tokens_select(hhNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
hhDFM <- dfm(hhNoStop, tolower = T, verbose = T, remove_padding = T)
hhDFM <- dfm_trim(hhDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
hhDFM

## F) document term matrix für tm erschaffen
hhDTM <- convert(hhDFM, to = "topicmodels")
hhDTM

################################################################################

# MODELL 1
hh1 <- LDA(hhDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(hh1, file = "lda_saves/hh1.RData")

# MODELL 2
hh2 <- LDA(hhDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(hh2, file = "lda_saves/hh2.RData")

# MODELL 3
hh3 <- LDA(hhDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(hh3, file = "lda_saves/hh3.RData")

# MODELL 4
hh4 <- LDA(hhDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(hh4, file = "lda_saves/hh4.RData")

### posterior-ergebnisse abspeichern
post_hh1 <- posterior(hh1)
post_hh2 <- posterior(hh2)
post_hh3 <- posterior(hh3)
post_hh4 <- posterior(hh4)

terms(hh1, 20)
terms(hh2, 20)
terms(hh3, 20)
terms(hh4, 20)

################################################################################

# hessen

load(file = "regionalisierung/hessen_discPow.RData")

## A) erstellen des korpus
hesCorp <- corpus(hessen$content,
                 docvars = data.frame(username = (hessen$user)))
summary(hesCorp)

## B) vorbereiten für topic model
hesReshaped <- corpus_reshape(hesCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
hesMatr <- tokens(hesReshaped, verbose = T, what = "word", remove_punct = T,
                 remove_symbols = T, remove_separators = T, remove_numbers = T,
                 remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
hesNoStopDE <- tokens_select(hesMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
hesNoStopDE_2 <- tokens_select(hesNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
hesNoStopEN <- tokens_select(hesNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
hesNoStop <- tokens_select(hesNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
hesDFM <- dfm(hesNoStop, tolower = T, verbose = T, remove_padding = T)
hesDFM <- dfm_trim(hesDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
hesDFM

## F) document term matrix für tm erschaffen
hesDTM <- convert(hesDFM, to = "topicmodels")
hesDTM

################################################################################

# MODELL 1
hes1 <- LDA(hesDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(hes1, file = "lda_saves/hes1.RData")

# MODELL 2
hes2 <- LDA(hesDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(hes2, file = "lda_saves/hes2.RData")

# MODELL 3
hes3 <- LDA(hesDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(hes3, file = "lda_saves/hes3.RData")

# MODELL 4
hes4 <- LDA(hesDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(hes4, file = "lda_saves/hes4.RData")

### posterior-ergebnisse abspeichern
post_hes1 <- posterior(hes1)
post_hes2 <- posterior(hes2)
post_hes3 <- posterior(hes3)
post_hes4 <- posterior(hes4)

terms(hes1, 20)
terms(hes2, 20)
terms(hes3, 20)
terms(hes4, 20)

################################################################################

# mecklenburg vorpommern

load(file = "regionalisierung/meckPomm_discPow.RData")

## A) erstellen des korpus
mpCorp <- corpus(meckPomm$content,
                 docvars = data.frame(username = (meckPomm$user)))
summary(mpCorp)

## B) vorbereiten für topic model
mpReshaped <- corpus_reshape(mpCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
mpMatr <- tokens(mpReshaped, verbose = T, what = "word", remove_punct = T,
                 remove_symbols = T, remove_separators = T, remove_numbers = T,
                 remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
mpNoStopDE <- tokens_select(mpMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
mpNoStopDE_2 <- tokens_select(mpNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
mpNoStopEN <- tokens_select(mpNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
mpNoStop <- tokens_select(mpNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
mpDFM <- dfm(mpNoStop, tolower = T, verbose = T, remove_padding = T)
mpDFM <- dfm_trim(mpDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
mpDFM

## F) document term matrix für tm erschaffen
mpDTM <- convert(mpDFM, to = "topicmodels")
mpDTM

################################################################################

# MODELL 1
mp1 <- LDA(mpDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(mp1, file = "lda_saves/mp1.RData")

# MODELL 2
mp2 <- LDA(mpDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(mp2, file = "lda_saves/mp2.RData")

# MODELL 3
mp3 <- LDA(mpDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(mp3, file = "lda_saves/mp3.RData")

# MODELL 4
mp4 <- LDA(mpDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(mp4, file = "lda_saves/mp4.RData")

### posterior-ergebnisse abspeichern
post_mp1 <- posterior(mp1)
post_mp2 <- posterior(mp2)
post_mp3 <- posterior(mp3)
post_mp4 <- posterior(mp4)

terms(mp1, 20)
terms(mp2, 20)
terms(mp3, 20)
terms(mp4, 20)

################################################################################

# niedersachsen

load(file = "regionalisierung/niedersachsen_discPow.RData")

## A) erstellen des korpus
ndsCorp <- corpus(niedersachsen$content,
                 docvars = data.frame(username = (niedersachsen$user)))
summary(ndsCorp)

## B) vorbereiten für topic model
ndsReshaped <- corpus_reshape(ndsCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
ndsMatr <- tokens(ndsReshaped, verbose = T, what = "word", remove_punct = T,
                 remove_symbols = T, remove_separators = T, remove_numbers = T,
                 remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
ndsNoStopDE <- tokens_select(ndsMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
ndsNoStopDE_2 <- tokens_select(ndsNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
ndsNoStopEN <- tokens_select(ndsNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
ndsNoStop <- tokens_select(ndsNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
ndsDFM <- dfm(ndsNoStop, tolower = T, verbose = T, remove_padding = T)
ndsDFM <- dfm_trim(ndsDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
ndsDFM

## F) document term matrix für tm erschaffen
ndsDTM <- convert(ndsDFM, to = "topicmodels")
ndsDTM

################################################################################

# MODELL 1
nds1 <- LDA(ndsDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(nds1, file = "lda_saves/nds1.RData")

# MODELL 2
nds2 <- LDA(ndsDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(nds2, file = "lda_saves/nds2.RData")

# MODELL 3
nds3 <- LDA(ndsDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(nds3, file = "lda_saves/nds3.RData")

# MODELL 4
nds4 <- LDA(ndsDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(nds4, file = "lda_saves/nds4.RData")

### posterior-ergebnisse abspeichern
post_nds1 <- posterior(nds1)
post_nds2 <- posterior(nds2)
post_nds3 <- posterior(nds3)
post_nds4 <- posterior(nds4)

terms(nds1, 20)
terms(nds2, 20)
terms(nds3, 20)
terms(nds4, 20)

################################################################################

# nordrhein westfalen

load(file = "regionalisierung/nrw_discPow.RData")

## A) erstellen des korpus
nrwCorp <- corpus(nrw$content,
                 docvars = data.frame(username = (nrw$user)))
summary(nrwCorp)

## B) vorbereiten für topic model
nrwReshaped <- corpus_reshape(nrwCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
nrwMatr <- tokens(nrwReshaped, verbose = T, what = "word", remove_punct = T,
                 remove_symbols = T, remove_separators = T, remove_numbers = T,
                 remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
nrwNoStopDE <- tokens_select(nrwMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
nrwNoStopDE_2 <- tokens_select(nrwNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
nrwNoStopEN <- tokens_select(nrwNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
nrwNoStop <- tokens_select(nrwNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
nrwDFM <- dfm(nrwNoStop, tolower = T, verbose = T, remove_padding = T)
nrwDFM <- dfm_trim(nrwDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
nrwDFM

## F) document term matrix für tm erschaffen
nrwDTM <- convert(nrwDFM, to = "topicmodels")
nrwDTM

################################################################################

# MODELL 1
nrw1 <- LDA(nrwDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(nrw1, file = "lda_saves/nrw1.RData")

# MODELL 2
nrw2 <- LDA(nrwDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(nrw2, file = "lda_saves/nrw2.RData")

# MODELL 3
nrw3 <- LDA(nrwDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(nrw3, file = "lda_saves/nrw3.RData")

# MODELL 4
nrw4 <- LDA(nrwDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(nrw4, file = "lda_saves/nrw4.RData")

### posterior-ergebnisse abspeichern
post_nrw1 <- posterior(nrw1)
post_nrw2 <- posterior(nrw2)
post_nrw3 <- posterior(nrw3)
post_nrw4 <- posterior(nrw4)

terms(nrw1, 20)
terms(nrw2, 20)
terms(nrw3, 20)
terms(nrw4, 20)

################################################################################

# rheinland pfalz

load(file = "regionalisierung/rlp_discPow.RData")

## A) erstellen des korpus
rlpCorp <- corpus(rlp$content,
                  docvars = data.frame(username = (rlp$user)))
summary(rlpCorp)

## B) vorbereiten für topic model
rlpReshaped <- corpus_reshape(rlpCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
rlpMatr <- tokens(rlpReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
rlpNoStopDE <- tokens_select(rlpMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
rlpNoStopDE_2 <- tokens_select(rlpNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
rlpNoStopEN <- tokens_select(rlpNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
rlpNoStop <- tokens_select(rlpNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
rlpDFM <- dfm(rlpNoStop, tolower = T, verbose = T, remove_padding = T)
rlpDFM <- dfm_trim(rlpDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
rlpDFM

## F) document term matrix für tm erschaffen
rlpDTM <- convert(rlpDFM, to = "topicmodels")
rlpDTM

################################################################################

# MODELL 1
rlp1 <- LDA(rlpDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(rlp1, file = "lda_saves/rlp1.RData")

# MODELL 2
rlp2 <- LDA(rlpDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(rlp2, file = "lda_saves/rlp2.RData")

# MODELL 3
rlp3 <- LDA(rlpDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(rlp3, file = "lda_saves/rlp3.RData")

# MODELL 4
rlp4 <- LDA(rlpDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(rlp4, file = "lda_saves/rlp4.RData")

### posterior-ergebnisse abspeichern
post_rlp1 <- posterior(rlp1)
post_rlp2 <- posterior(rlp2)
post_rlp3 <- posterior(rlp3)
post_rlp4 <- posterior(rlp4)

terms(rlp1, 20)
terms(rlp2, 20)
terms(rlp3, 20)
terms(rlp4, 20)

################################################################################

# saarland

load(file = "regionalisierung/saarland_discPow.RData")

## A) erstellen des korpus
saarCorp <- corpus(saarland$content,
                  docvars = data.frame(username = (saarland$user)))
summary(saarCorp)

## B) vorbereiten für topic model
saarReshaped <- corpus_reshape(saarCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
saarMatr <- tokens(saarReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
saarNoStopDE <- tokens_select(saarMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
saarNoStopDE_2 <- tokens_select(saarNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
saarNoStopEN <- tokens_select(saarNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
saarNoStop <- tokens_select(saarNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
saarDFM <- dfm(saarNoStop, tolower = T, verbose = T, remove_padding = T)
saarDFM <- dfm_trim(saarDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
saarDFM

## F) document term matrix für tm erschaffen
saarDTM <- convert(saarDFM, to = "topicmodels")
saarDTM

################################################################################

# MODELL 1
saar1 <- LDA(saarDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(saar1, file = "lda_saves/saar1.RData")

# MODELL 2
saar2 <- LDA(saarDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(saar2, file = "lda_saves/saar2.RData")

# MODELL 3
saar3 <- LDA(saarDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(saar3, file = "lda_saves/saar3.RData")

# MODELL 4
saar4 <- LDA(saarDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(saar4, file = "lda_saves/saar4.RData")

### posterior-ergebnisse abspeichern
post_saar1 <- posterior(saar1)
post_saar2 <- posterior(saar2)
post_saar3 <- posterior(saar3)
post_saar4 <- posterior(saar4)

terms(saar1, 20)
terms(saar2, 20)
terms(saar3, 20)
terms(saar4, 20)

################################################################################

# sachsen

load(file = "regionalisierung/sachsen_discPow.RData")

## A) erstellen des korpus
sacCorp <- corpus(sachsen$content,
                  docvars = data.frame(username = (sachsen$user)))
summary(sacCorp)

## B) vorbereiten für topic model
sacReshaped <- corpus_reshape(sacCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
sacMatr <- tokens(sacReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
sacNoStopDE <- tokens_select(sacMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
sacNoStopDE_2 <- tokens_select(sacNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
sacNoStopEN <- tokens_select(sacNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
sacNoStop <- tokens_select(sacNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
sacDFM <- dfm(sacNoStop, tolower = T, verbose = T, remove_padding = T)
sacDFM <- dfm_trim(sacDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
sacDFM

## F) document term matrix für tm erschaffen
sacDTM <- convert(sacDFM, to = "topicmodels")
sacDTM

################################################################################

# MODELL 1
sac1 <- LDA(sacDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(sac1, file = "lda_saves/sac1.RData")

# MODELL 2
sac2 <- LDA(sacDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(sac2, file = "lda_saves/sac2.RData")

# MODELL 3
sac3 <- LDA(sacDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(sac3, file = "lda_saves/sac3.RData")

# MODELL 4
sac4 <- LDA(sacDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(sac4, file = "lda_saves/sac4.RData")

### posterior-ergebnisse abspeichern
post_sac1 <- posterior(sac1)
post_sac2 <- posterior(sac2)
post_sac3 <- posterior(sac3)
post_sac4 <- posterior(sac4)

terms(sac1, 20)
terms(sac2, 20)
terms(sac3, 20)
terms(sac4, 20)

################################################################################

# sachsen anhalt

load(file = "regionalisierung/sachsen_anhalt_discPow.RData")

## A) erstellen des korpus
sanCorp <- corpus(sachsen_anhalt$content,
                  docvars = data.frame(username = (sachsen_anhalt$user)))
summary(sanCorp)

## B) vorbereiten für topic model
sanReshaped <- corpus_reshape(sanCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
sanMatr <- tokens(sanReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
sanNoStopDE <- tokens_select(sanMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
sanNoStopDE_2 <- tokens_select(sanNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
sanNoStopEN <- tokens_select(sanNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
sanNoStop <- tokens_select(sanNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
sanDFM <- dfm(sanNoStop, tolower = T, verbose = T, remove_padding = T)
sanDFM <- dfm_trim(sanDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
sanDFM

## F) document term matrix für tm erschaffen
sanDTM <- convert(sanDFM, to = "topicmodels")
sanDTM

################################################################################

# MODELL 1
san1 <- LDA(sanDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(san1, file = "lda_saves/san1.RData")

# MODELL 2
san2 <- LDA(sanDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(san2, file = "lda_saves/san2.RData")

# MODELL 3
san3 <- LDA(sanDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(san3, file = "lda_saves/san3.RData")

# MODELL 4
san4 <- LDA(sanDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(san4, file = "lda_saves/san4.RData")

### posterior-ergebnisse abspeichern
post_san1 <- posterior(san1)
post_san2 <- posterior(san2)
post_san3 <- posterior(san3)
post_san4 <- posterior(san4)

terms(san1, 20)
terms(san2, 20)
terms(san3, 20)
terms(san4, 20)

################################################################################

# schleswig holstein

load(file = "regionalisierung/schleswig_discPow.RData")

## A) erstellen des korpus
shCorp <- corpus(schleswig$content,
                  docvars = data.frame(username = (schleswig$user)))
summary(shCorp)

## B) vorbereiten für topic model
shReshaped <- corpus_reshape(shCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
shMatr <- tokens(shReshaped, verbose = T, what = "word", remove_punct = T,
                  remove_symbols = T, remove_separators = T, remove_numbers = T,
                  remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
shNoStopDE <- tokens_select(shMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
shNoStopDE_2 <- tokens_select(shNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
shNoStopEN <- tokens_select(shNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
shNoStop <- tokens_select(shNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
shDFM <- dfm(shNoStop, tolower = T, verbose = T, remove_padding = T)
shDFM <- dfm_trim(shDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
shDFM

## F) document term matrix für tm erschaffen
shDTM <- convert(shDFM, to = "topicmodels")
shDTM

################################################################################

# MODELL 1
sh1 <- LDA(shDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(sh1, file = "lda_saves/sh1.RData")

# MODELL 2
sh2 <- LDA(shDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(sh2, file = "lda_saves/sh2.RData")

# MODELL 3
sh3 <- LDA(shDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(sh3, file = "lda_saves/sh3.RData")

# MODELL 4
sh4 <- LDA(shDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(sh4, file = "lda_saves/sh4.RData")

### posterior-ergebnisse abspeichern
post_sh1 <- posterior(sh1)
post_sh2 <- posterior(sh2)
post_sh3 <- posterior(sh3)
post_sh4 <- posterior(sh4)

terms(sh1, 20)
terms(sh2, 20)
terms(sh3, 20)
terms(sh4, 20)

################################################################################

# thüringen

load(file = "regionalisierung/thüringen_discPow.RData")

## A) erstellen des korpus
thüCorp <- corpus(thüringen$content,
                 docvars = data.frame(username = (thüringen$user)))
summary(thüCorp)

## B) vorbereiten für topic model
thüReshaped <- corpus_reshape(thüCorp, to="documents")

## C) matrix erschaffen aus dem ergebnis der tokenization
thüMatr <- tokens(thüReshaped, verbose = T, what = "word", remove_punct = T,
                 remove_symbols = T, remove_separators = T, remove_numbers = T,
                 remove_url = T, split_hyphens = F, split_tags = T)

## D) stopwords von zwei verschiedenen gängigen listen für DE entfernen + EN stopwords entfernen
thüNoStopDE <- tokens_select(thüMatr, pattern = stopwords::stopwords("german", source = "marimo"), selection = "remove")
thüNoStopDE_2 <- tokens_select(thüNoStopDE, pattern = stopwords::stopwords("german", source = "stopwords-iso"), selection = "remove")
thüNoStopEN <- tokens_select(thüNoStopDE_2, pattern = stopwords::stopwords("english", source = "stopwords-iso"), selection = "remove")
thüNoStop <- tokens_select(thüNoStopEN, pattern = myStopwordsDE, selection = "remove")

## E) document frequency matrix
thüDFM <- dfm(thüNoStop, tolower = T, verbose = T, remove_padding = T)
thüDFM <- dfm_trim(thüDFM, min_docfreq = 5, docfreq_type = "count", verbose = T)
thüDFM

## F) document term matrix für tm erschaffen
thüDTM <- convert(thüDFM, to = "topicmodels")
thüDTM

################################################################################

# MODELL 1
thü1 <- LDA(thüDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 0.1, seed = 11))
save(thü1, file = "lda_saves/thü1.RData")

# MODELL 2
thü2 <- LDA(thüDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 0.1, seed = 111))
save(thü2, file = "lda_saves/thü2.RData")

# MODELL 3
thü3 <- LDA(thüDTM, method = "Gibbs", k = 30, beta = 0.1, control = list(verbose = 1, alpha = 1, seed = 1111))
save(thü3, file = "lda_saves/thü3.RData")

# MODELL 4
thü4 <- LDA(thüDTM, method = "Gibbs", k = 30, beta = 1, control = list(verbose = 1, alpha = 1, seed = 11111))
save(thü4, file = "lda_saves/thü4.RData")

### posterior-ergebnisse abspeichern
post_thü1 <- posterior(thü1)
post_thü2 <- posterior(thü2)
post_thü3 <- posterior(thü3)
post_thü4 <- posterior(thü4)

terms(thü1, 30)
terms(thü2, 30)
terms(thü3, 30)
terms(thü4, 30)

write_csv(x = data.frame(terms(thü1, 30)), file = "../../../masterarbeit/thüringen_terms.csv")
write_csv(x = data.frame(terms(thü2, 30)), file = "../../../masterarbeit/thüringen_terms2.csv")
write_csv(x = data.frame(terms(thü3, 30)), file = "../../../masterarbeit/thüringen_terms3.csv")
write_csv(x = data.frame(terms(thü4, 30)), file = "../../../masterarbeit/thüringen_terms4.csv")









thü5 <- LDA(thüDTM, method = "Gibbs", k = 15, control = list(verbose = 1, seed = 55))
terms(thü5, 20)
posterior(thü5)[["topics"]]

#########

stopwordsNamen <- c("olaf", "wladimir", "donald", "robert", "boris", "elon", "joe", "karl", "wolodymyr", "friedrich", "annalena", "christian",
                    "emmanuel", "liz", "marine", "markus", "walter", "nancy", "sergej", "andrij")


thüringen$strippedContent <- gsub("http.*", "", thüringen$content)
thüringen$strippedContent <- gsub("http.*", "", thüringen$content)

trigramsTHÜ <- thüringen %>%
  dplyr::select(strippedContent) %>%
  tidytext::unnest_tokens(paired_words,
                          strippedContent,
                          token ="ngrams", n=3)

trigramsSepTHÜ <- trigramsTHÜ %>%
  separate(paired_words, c("word1", "word2", "word3"), sep = " ")

trigramsFilteredTHÜ <- trigramsSepTHÜ %>% 
  filter(!word1 %in% stopwords::stopwords("german", source="marimo")) %>%
  filter(!word2 %in% stopwords::stopwords("german", source="marimo")) %>%
  filter(!word3 %in% stopwords::stopwords("german", source="marimo")) %>%
  filter(!word1 %in% stopwords::stopwords("german", source="stopwords-iso")) %>%
  filter(!word2 %in% stopwords::stopwords("german", source="stopwords-iso")) %>%
  filter(!word3 %in% stopwords::stopwords("german", source="stopwords-iso")) %>%
  filter(!word1 %in% stopwords::stopwords("english", source="stopwords-iso")) %>%
  filter(!word2 %in% stopwords::stopwords("english", source="stopwords-iso")) %>%
  filter(!word3 %in% stopwords::stopwords("english", source="stopwords-iso")) %>%
  filter(!word1 %in% myStopwordsDE) %>%
  filter(!word2 %in% myStopwordsDE) %>%
  filter(!word3 %in% myStopwordsDE) %>%
  filter(!word1 %in% stopwordsNamen) %>%
  filter(!word2 %in% stopwordsNamen) %>%
  filter(!word3 %in% stopwordsNamen)

# counts für unigrams, bigrams, trigrams
trigramsCountsTHÜ <- trigramsFilteredTHÜ %>% count(word1, word2, word3, sort=T)
head(trigramsCountsTHÜ, 10)
























# tokensThü <- tokens(thüringen$content, verbose = T, what = "word", remove_punct = T,
#        remove_symbols = T, remove_separators = T, remove_numbers = T,
#        remove_url = T, split_hyphens = F, split_tags = T)
# 
# typeof(tokensThü)
# as.list.data.frame(tokensThü)
# 
# unigramsTHÜ <- thüringen %>%
#   dplyr::select(strippedContent) %>%
#   tidytext::unnest_tokens(word, strippedContent)
# 
# typeof(unigramsTHÜ)
# str(unigramsTHÜ)
# 
# 
# unigramsFilteredTHÜ <- unigramsTHÜ %>% 
#   filter(!word %in% stopwords::stopwords("german", source="marimo")) %>% 
#   filter(!word %in% stopwords::stopwords("german", source="stopwords-iso")) %>% 
#   filter(!word %in% stopwords::stopwords("english", source="stopwords-iso")) %>% 
#   filter(!word %in% myStopwordsDE) %>% 
#   filter(!word %in% stopwordsNamen)
# 
# unigrTHÜ <- unigramsFilteredTHÜ$word
# write_csv(unigramsFilteredTHÜ, file = "../representative-speech/unigrTHÜ.csv")


