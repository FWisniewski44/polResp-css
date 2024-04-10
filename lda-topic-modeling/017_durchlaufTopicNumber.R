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
## daten
bayRn <- read_csv("regionalisierung/demojized/bayern_demojized.csv")
baden <- read_csv("regionalisierung/demojized/bawü_demojized.csv")
berl <- read_csv("regionalisierung/demojized/berlin_demojized.csv")
bran <- read_csv("regionalisierung/demojized/brandenburg_demojized.csv")
bremen <- read_csv("regionalisierung/demojized/bremen_demojized.csv")
hamburg <- read_csv("regionalisierung/demojized/hamburg_demojized.csv")
hessen <- read_csv("regionalisierung/demojized/hessen_demojized.csv")
mecklenburg <- read_csv("regionalisierung/demojized/meckPomm_demojized.csv")
niedersachsen <- read_csv("regionalisierung/demojized/niedersachsen_demojized.csv")
nordrhein <- read_csv("regionalisierung/demojized/nrw_demojized.csv")
rheinland <- read_csv("regionalisierung/demojized/rlp_demojized.csv")
saarland <- read_csv("regionalisierung/demojized/saarland_demojized.csv")
sachsen <- read_csv("regionalisierung/demojized/sachsen_demojized.csv")
sachsenAnhalt <- read_csv("regionalisierung/demojized/sachsen_anhalt_demojized.csv")
schlesHolst <- read_csv("regionalisierung/demojized/schleswig_demojized.csv")
thüringen <- read_csv("regionalisierung/demojized/thüringen_demojized.csv")

## rest
politikR <- read_csv("regionalisierung/demojized/politikR_demojized.csv")
überregional <- read_csv("regionalisierung/demojized/überregio_demojized.csv")
öffentlicher <- read_csv("regionalisierung/demojized/örr_demojized.csv")
digitale <- read_csv("regionalisierung/demojized/digitale_demojized.csv")

################################################################################
## vorbereitungen
bayRn <- bayRn[,-1]
bayRn <- cbind(doc_id = 1:nrow(bayRn), bayRn)

baden <- baden[,-1]
baden <- cbind(doc_id = 1:nrow(baden), baden)

berl <- berl[,-1]
berl <- cbind(doc_id = 1:nrow(berl), berl)

bran <- bran[,-1]
bran <- cbind(doc_id = 1:nrow(bran), bran)

bremen <- bremen[,-1]
bremen <- cbind(doc_id = 1:nrow(bremen), bremen)

hamburg <- hamburg[,-1]
hamburg <- cbind(doc_id = 1:nrow(hamburg), hamburg)

hessen <- hessen[,-1]
hessen <- cbind(doc_id = 1:nrow(hessen), hessen)

mecklenburg <- mecklenburg[,-1]
mecklenburg <- cbind(doc_id = 1:nrow(mecklenburg), mecklenburg)

niedersachsen <- niedersachsen[,-1]
niedersachsen <- cbind(doc_id = 1:nrow(niedersachsen), niedersachsen)

nordrhein <- nordrhein[,-1]
nordrhein <- cbind(doc_id = 1:nrow(nordrhein), nordrhein)

rheinland <- rheinland[,-1]
rheinland <- cbind(doc_id = 1:nrow(rheinland), rheinland)

saarland <- saarland[,-1]
saarland <- cbind(doc_id = 1:nrow(saarland), saarland)

sachsen <- sachsen[,-1]
sachsen <- cbind(doc_id = 1:nrow(sachsen), sachsen)

sachsenAnhalt <- sachsenAnhalt[,-1]
sachsenAnhalt <- cbind(doc_id = 1:nrow(sachsenAnhalt), sachsenAnhalt)

schlesHolst <- schlesHolst[,-1]
schlesHolst <- cbind(doc_id = 1:nrow(schlesHolst), schlesHolst)

thüringen <- thüringen[,-1]
thüringen <- cbind(doc_id = 1:nrow(thüringen), thüringen)

politikR <- politikR[,-1]
politikR <- cbind(doc_id = 1:nrow(politikR), politikR)

öffentlicher <- öffentlicher[,-1]
öffentlicher <- cbind(doc_id = 1:nrow(öffentlicher), öffentlicher)

überregional <- überregional[,-1]
überregional <- cbind(doc_id = 1:nrow(überregional), überregional)

digitale <- digitale[,-1]
digitale <- cbind(doc_id = 1:nrow(digitale), digitale)

################################################################################

bayRn$text <- gsub("http*(.*)", " ", bayRn$text, perl = TRUE)
bayRn$text <- gsub("https*(.*)", " ", bayRn$text, perl = TRUE)
bayRn$text <- gsub("@\\w+", " ", bayRn$text, perl=TRUE)
bayRn$text <- gsub("\\n", " ", bayRn$text, perl = TRUE)
bayRn$text <- gsub("lt3", " ", bayRn$text, perl = TRUE)
bayRn$text <- gsub("…", " ", bayRn$text, perl = TRUE)
bayRn$text <- gsub("’", " ", bayRn$text, perl = TRUE)
bayRn$text <- gsub("‘", " ", bayRn$text, perl = TRUE)
#
corpusBayRn <- Corpus(DataframeSource(bayRn))
processedCorpus_bayRn <- tm_map(corpusBayRn, removeWords, allstopwords)
processedCorpus_bayRn <- tm_map(processedCorpus_bayRn, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_bayRn <- tm_map(processedCorpus_bayRn, stripWhitespace)

################################################################################

baden$text <- gsub("http*(.*)", " ", baden$text, perl = TRUE)
baden$text <- gsub("https*(.*)", " ", baden$text, perl = TRUE)
baden$text <- gsub("@\\w+", " ", baden$text, perl=TRUE)
baden$text <- gsub("\\n", " ", baden$text, perl = TRUE)
baden$text <- gsub("lt3", " ", baden$text, perl = TRUE)
baden$text <- gsub("…", " ", baden$text, perl = TRUE)
baden$text <- gsub("’", " ", baden$text, perl = TRUE)
baden$text <- gsub("‘", " ", baden$text, perl = TRUE)
#
corpusBaden <- Corpus(DataframeSource(baden))
processedCorpus_baden <- tm_map(corpusBaden, removeWords, allstopwords)
processedCorpus_baden <- tm_map(processedCorpus_baden, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_baden <- tm_map(processedCorpus_baden, stripWhitespace)

################################################################################

berl$text <- gsub("http*(.*)", " ", berl$text, perl = TRUE)
berl$text <- gsub("https*(.*)", " ", berl$text, perl = TRUE)
berl$text <- gsub("@\\w+", " ", berl$text, perl=TRUE)
berl$text <- gsub("\\n", " ", berl$text, perl = TRUE)
berl$text <- gsub("lt3", " ", berl$text, perl = TRUE)
berl$text <- gsub("…", " ", berl$text, perl = TRUE)
berl$text <- gsub("’", " ", berl$text, perl = TRUE)
berl$text <- gsub("‘", " ", berl$text, perl = TRUE)
#
corpusBerl <- Corpus(DataframeSource(berl))
processedCorpus_berl <- tm_map(corpusBerl, removeWords, allstopwords)
processedCorpus_berl <- tm_map(processedCorpus_berl, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_berl <- tm_map(processedCorpus_berl, stripWhitespace)

################################################################################

bran$text <- gsub("http*(.*)", " ", bran$text, perl = TRUE)
bran$text <- gsub("https*(.*)", " ", bran$text, perl = TRUE)
bran$text <- gsub("@\\w+", " ", bran$text, perl=TRUE)
bran$text <- gsub("\\n", " ", bran$text, perl = TRUE)
bran$text <- gsub("lt3", " ", bran$text, perl = TRUE)
bran$text <- gsub("…", " ", bran$text, perl = TRUE)
bran$text <- gsub("’", " ", bran$text, perl = TRUE)
bran$text <- gsub("‘", " ", bran$text, perl = TRUE)
#
corpusBran <- Corpus(DataframeSource(bran))
processedCorpus_bran <- tm_map(corpusBran, removeWords, allstopwords)
processedCorpus_bran <- tm_map(processedCorpus_bran, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_bran <- tm_map(processedCorpus_bran, stripWhitespace)

################################################################################

bremen$text <- gsub("http*(.*)", " ", bremen$text, perl = TRUE)
bremen$text <- gsub("https*(.*)", " ", bremen$text, perl = TRUE)
bremen$text <- gsub("@\\w+", " ", bremen$text, perl=TRUE)
bremen$text <- gsub("\\n", " ", bremen$text, perl = TRUE)
bremen$text <- gsub("lt3", " ", bremen$text, perl = TRUE)
bremen$text <- gsub("…", " ", bremen$text, perl = TRUE)
bremen$text <- gsub("’", " ", bremen$text, perl = TRUE)
bremen$text <- gsub("‘", " ", bremen$text, perl = TRUE)
#
corpusBremen <- Corpus(DataframeSource(bremen))
processedCorpus_bremen <- tm_map(corpusBremen, removeWords, allstopwords)
processedCorpus_bremen <- tm_map(processedCorpus_bremen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_bremen <- tm_map(processedCorpus_bremen, stripWhitespace)

################################################################################

hamburg$text <- gsub("http*(.*)", " ", hamburg$text, perl = TRUE)
hamburg$text <- gsub("https*(.*)", " ", hamburg$text, perl = TRUE)
hamburg$text <- gsub("@\\w+", " ", hamburg$text, perl=TRUE)
hamburg$text <- gsub("\\n", " ", hamburg$text, perl = TRUE)
hamburg$text <- gsub("lt3", " ", hamburg$text, perl = TRUE)
hamburg$text <- gsub("…", " ", hamburg$text, perl = TRUE)
hamburg$text <- gsub("’", " ", hamburg$text, perl = TRUE)
hamburg$text <- gsub("‘", " ", hamburg$text, perl = TRUE)
#
corpusHamburg <- Corpus(DataframeSource(hamburg))
processedCorpus_hamburg <- tm_map(corpusHamburg, removeWords, allstopwords)
processedCorpus_hamburg <- tm_map(processedCorpus_hamburg, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_hamburg <- tm_map(processedCorpus_hamburg, stripWhitespace)

################################################################################

hessen$text <- gsub("http*(.*)", " ", hessen$text, perl = TRUE)
hessen$text <- gsub("https*(.*)", " ", hessen$text, perl = TRUE)
hessen$text <- gsub("@\\w+", " ", hessen$text, perl=TRUE)
hessen$text <- gsub("\\n", " ", hessen$text, perl = TRUE)
hessen$text <- gsub("lt3", " ", hessen$text, perl = TRUE)
hessen$text <- gsub("…", " ", hessen$text, perl = TRUE)
hessen$text <- gsub("’", " ", hessen$text, perl = TRUE)
hessen$text <- gsub("‘", " ", hessen$text, perl = TRUE)
#
corpusHessen <- Corpus(DataframeSource(hessen))
processedCorpus_hessen <- tm_map(corpusHessen, removeWords, allstopwords)
processedCorpus_hessen <- tm_map(processedCorpus_hessen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_hessen <- tm_map(processedCorpus_hessen, stripWhitespace)

################################################################################

mecklenburg$text <- gsub("http*(.*)", " ", mecklenburg$text, perl = TRUE)
mecklenburg$text <- gsub("https*(.*)", " ", mecklenburg$text, perl = TRUE)
mecklenburg$text <- gsub("@\\w+", " ", mecklenburg$text, perl=TRUE)
mecklenburg$text <- gsub("\\n", " ", mecklenburg$text, perl = TRUE)
mecklenburg$text <- gsub("lt3", " ", mecklenburg$text, perl = TRUE)
mecklenburg$text <- gsub("…", " ", mecklenburg$text, perl = TRUE)
mecklenburg$text <- gsub("’", " ", mecklenburg$text, perl = TRUE)
mecklenburg$text <- gsub("‘", " ", mecklenburg$text, perl = TRUE)
#
corpusMecklenburg <- Corpus(DataframeSource(mecklenburg))
processedCorpus_mecklenburg <- tm_map(corpusMecklenburg, removeWords, allstopwords)
processedCorpus_mecklenburg <- tm_map(processedCorpus_mecklenburg, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_mecklenburg <- tm_map(processedCorpus_mecklenburg, stripWhitespace)

################################################################################

niedersachsen$text <- gsub("http*(.*)", " ", niedersachsen$text, perl = TRUE)
niedersachsen$text <- gsub("https*(.*)", " ", niedersachsen$text, perl = TRUE)
niedersachsen$text <- gsub("@\\w+", " ", niedersachsen$text, perl=TRUE)
niedersachsen$text <- gsub("\\n", " ", niedersachsen$text, perl = TRUE)
niedersachsen$text <- gsub("lt3", " ", niedersachsen$text, perl = TRUE)
niedersachsen$text <- gsub("…", " ", niedersachsen$text, perl = TRUE)
niedersachsen$text <- gsub("’", " ", niedersachsen$text, perl = TRUE)
niedersachsen$text <- gsub("‘", " ", niedersachsen$text, perl = TRUE)
#
corpusNiedersachsen <- Corpus(DataframeSource(niedersachsen))
processedCorpus_niedersachsen <- tm_map(corpusNiedersachsen, removeWords, allstopwords)
processedCorpus_niedersachsen <- tm_map(processedCorpus_niedersachsen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_niedersachsen <- tm_map(processedCorpus_niedersachsen, stripWhitespace)

################################################################################

nordrhein$text <- gsub("http*(.*)", " ", nordrhein$text, perl = TRUE)
nordrhein$text <- gsub("https*(.*)", " ", nordrhein$text, perl = TRUE)
nordrhein$text <- gsub("@\\w+", " ", nordrhein$text, perl=TRUE)
nordrhein$text <- gsub("\\n", " ", nordrhein$text, perl = TRUE)
nordrhein$text <- gsub("lt3", " ", nordrhein$text, perl = TRUE)
nordrhein$text <- gsub("…", " ", nordrhein$text, perl = TRUE)
nordrhein$text <- gsub("’", " ", nordrhein$text, perl = TRUE)
nordrhein$text <- gsub("‘", " ", nordrhein$text, perl = TRUE)
#
corpusNordrhein <- Corpus(DataframeSource(nordrhein))
processedCorpus_nordrhein <- tm_map(corpusNordrhein, removeWords, allstopwords)
processedCorpus_nordrhein <- tm_map(processedCorpus_nordrhein, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_nordrhein <- tm_map(processedCorpus_nordrhein, stripWhitespace)

################################################################################

rheinland$text <- gsub("http*(.*)", " ", rheinland$text, perl = TRUE)
rheinland$text <- gsub("https*(.*)", " ", rheinland$text, perl = TRUE)
rheinland$text <- gsub("@\\w+", " ", rheinland$text, perl=TRUE)
rheinland$text <- gsub("\\n", " ", rheinland$text, perl = TRUE)
rheinland$text <- gsub("lt3", " ", rheinland$text, perl = TRUE)
rheinland$text <- gsub("…", " ", rheinland$text, perl = TRUE)
rheinland$text <- gsub("’", " ", rheinland$text, perl = TRUE)
rheinland$text <- gsub("‘", " ", rheinland$text, perl = TRUE)
#
corpusRheinland <- Corpus(DataframeSource(rheinland))
processedCorpus_rheinland <- tm_map(corpusRheinland, removeWords, allstopwords)
processedCorpus_rheinland <- tm_map(processedCorpus_rheinland, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_rheinland <- tm_map(processedCorpus_rheinland, stripWhitespace)

################################################################################

saarland$text <- gsub("http*(.*)", " ", saarland$text, perl = TRUE)
saarland$text <- gsub("https*(.*)", " ", saarland$text, perl = TRUE)
saarland$text <- gsub("@\\w+", " ", saarland$text, perl=TRUE)
saarland$text <- gsub("\\n", " ", saarland$text, perl = TRUE)
saarland$text <- gsub("lt3", " ", saarland$text, perl = TRUE)
saarland$text <- gsub("…", " ", saarland$text, perl = TRUE)
saarland$text <- gsub("’", " ", saarland$text, perl = TRUE)
saarland$text <- gsub("‘", " ", saarland$text, perl = TRUE)
#
corpusSaarland <- Corpus(DataframeSource(saarland))
processedCorpus_saarland <- tm_map(corpusSaarland, removeWords, allstopwords)
processedCorpus_saarland <- tm_map(processedCorpus_saarland, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_saarland <- tm_map(processedCorpus_saarland, stripWhitespace)
################################################################################

sachsen$text <- gsub("http*(.*)", " ", sachsen$text, perl = TRUE)
sachsen$text <- gsub("https*(.*)", " ", sachsen$text, perl = TRUE)
sachsen$text <- gsub("@\\w+", " ", sachsen$text, perl=TRUE)
sachsen$text <- gsub("\\n", " ", sachsen$text, perl = TRUE)
sachsen$text <- gsub("lt3", " ", sachsen$text, perl = TRUE)
sachsen$text <- gsub("…", " ", sachsen$text, perl = TRUE)
sachsen$text <- gsub("’", " ", sachsen$text, perl = TRUE)
sachsen$text <- gsub("‘", " ", sachsen$text, perl = TRUE)
#
corpusSachsen <- Corpus(DataframeSource(sachsen))
processedCorpus_sachsen <- tm_map(corpusSachsen, removeWords, allstopwords)
processedCorpus_sachsen <- tm_map(processedCorpus_sachsen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_sachsen <- tm_map(processedCorpus_sachsen, stripWhitespace)

################################################################################

sachsenAnhalt$text <- gsub("http*(.*)", " ", sachsenAnhalt$text, perl = TRUE)
sachsenAnhalt$text <- gsub("https*(.*)", " ", sachsenAnhalt$text, perl = TRUE)
sachsenAnhalt$text <- gsub("@\\w+", " ", sachsenAnhalt$text, perl=TRUE)
sachsenAnhalt$text <- gsub("\\n", " ", sachsenAnhalt$text, perl = TRUE)
sachsenAnhalt$text <- gsub("lt3", " ", sachsenAnhalt$text, perl = TRUE)
sachsenAnhalt$text <- gsub("…", " ", sachsenAnhalt$text, perl = TRUE)
sachsenAnhalt$text <- gsub("’", " ", sachsenAnhalt$text, perl = TRUE)
sachsenAnhalt$text <- gsub("‘", " ", sachsenAnhalt$text, perl = TRUE)
#
corpusSachsenAnhalt <- Corpus(DataframeSource(sachsenAnhalt))
processedCorpus_sachsenAnhalt <- tm_map(corpusSachsenAnhalt, removeWords, allstopwords)
processedCorpus_sachsenAnhalt <- tm_map(processedCorpus_sachsenAnhalt, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_sachsenAnhalt <- tm_map(processedCorpus_sachsenAnhalt, stripWhitespace)

################################################################################

schlesHolst$text <- gsub("http*(.*)", " ", schlesHolst$text, perl = TRUE)
schlesHolst$text <- gsub("https*(.*)", " ", schlesHolst$text, perl = TRUE)
schlesHolst$text <- gsub("@\\w+", " ", schlesHolst$text, perl=TRUE)
schlesHolst$text <- gsub("\\n", " ", schlesHolst$text, perl = TRUE)
schlesHolst$text <- gsub("lt3", " ", schlesHolst$text, perl = TRUE)
schlesHolst$text <- gsub("…", " ", schlesHolst$text, perl = TRUE)
schlesHolst$text <- gsub("’", " ", schlesHolst$text, perl = TRUE)
schlesHolst$text <- gsub("‘", " ", schlesHolst$text, perl = TRUE)
#
corpusSchlesHolst <- Corpus(DataframeSource(schlesHolst))
processedCorpus_schlesHolst <- tm_map(corpusSchlesHolst, removeWords, allstopwords)
processedCorpus_schlesHolst <- tm_map(processedCorpus_schlesHolst, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_schlesHolst <- tm_map(processedCorpus_schlesHolst, stripWhitespace)

################################################################################

thüringen$text <- gsub("http*(.*)", " ", thüringen$text, perl = TRUE)
thüringen$text <- gsub("https*(.*)", " ", thüringen$text, perl = TRUE)
thüringen$text <- gsub("@\\w+", " ", thüringen$text, perl=TRUE)
thüringen$text <- gsub("\\n", " ", thüringen$text, perl = TRUE)
thüringen$text <- gsub("lt3", " ", thüringen$text, perl = TRUE)
thüringen$text <- gsub("…", " ", thüringen$text, perl = TRUE)
thüringen$text <- gsub("’", " ", thüringen$text, perl = TRUE)
thüringen$text <- gsub("‘", " ", thüringen$text, perl = TRUE)
#
corpusThüringen <- Corpus(DataframeSource(thüringen))
processedCorpus_thüringen <- tm_map(corpusThüringen, removeWords, allstopwords)
processedCorpus_thüringen <- tm_map(processedCorpus_thüringen, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_thüringen <- tm_map(processedCorpus_thüringen, stripWhitespace)

################################################################################

öffentlicher$text <- gsub("http*(.*)", " ", öffentlicher$text, perl = TRUE)
öffentlicher$text <- gsub("https*(.*)", " ", öffentlicher$text, perl = TRUE)
öffentlicher$text <- gsub("@\\w+", " ", öffentlicher$text, perl=TRUE)
öffentlicher$text <- gsub("\\n", " ", öffentlicher$text, perl = TRUE)
öffentlicher$text <- gsub("lt3", " ", öffentlicher$text, perl = TRUE)
öffentlicher$text <- gsub("…", " ", öffentlicher$text, perl = TRUE)
öffentlicher$text <- gsub("’", " ", öffentlicher$text, perl = TRUE)
öffentlicher$text <- gsub("‘", " ", öffentlicher$text, perl = TRUE)
#
corpusÖffentlicher <- Corpus(DataframeSource(öffentlicher))
processedCorpus_öffentlicher <- tm_map(corpusÖffentlicher, removeWords, allstopwords)
processedCorpus_öffentlicher <- tm_map(processedCorpus_öffentlicher, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_öffentlicher <- tm_map(processedCorpus_öffentlicher, stripWhitespace)

################################################################################

digitale$text <- gsub("http*(.*)", " ", digitale$text, perl = TRUE)
digitale$text <- gsub("https*(.*)", " ", digitale$text, perl = TRUE)
digitale$text <- gsub("@\\w+", " ", digitale$text, perl=TRUE)
digitale$text <- gsub("\\n", " ", digitale$text, perl = TRUE)
digitale$text <- gsub("lt3", " ", digitale$text, perl = TRUE)
digitale$text <- gsub("…", " ", digitale$text, perl = TRUE)
digitale$text <- gsub("’", " ", digitale$text, perl = TRUE)
digitale$text <- gsub("‘", " ", digitale$text, perl = TRUE)
#
corpusDigitale <- Corpus(DataframeSource(digitale))
processedCorpus_digitale <- tm_map(corpusDigitale, removeWords, allstopwords)
processedCorpus_digitale <- tm_map(processedCorpus_digitale, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_digitale <- tm_map(processedCorpus_digitale, stripWhitespace)

################################################################################

politikR$text <- gsub("http*(.*)", " ", politikR$text, perl = TRUE)
politikR$text <- gsub("https*(.*)", " ", politikR$text, perl = TRUE)
politikR$text <- gsub("@\\w+", " ", politikR$text, perl=TRUE)
politikR$text <- gsub("\\n", " ", politikR$text, perl = TRUE)
politikR$text <- gsub("lt3", " ", politikR$text, perl = TRUE)
politikR$text <- gsub("…", " ", politikR$text, perl = TRUE)
politikR$text <- gsub("’", " ", politikR$text, perl = TRUE)
politikR$text <- gsub("‘", " ", politikR$text, perl = TRUE)
#
corpusPolitikR <- Corpus(DataframeSource(politikR))
processedCorpus_politikR <- tm_map(corpusPolitikR, removeWords, allstopwords)
processedCorpus_politikR <- tm_map(processedCorpus_politikR, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_politikR <- tm_map(processedCorpus_politikR, stripWhitespace)

################################################################################

überregional$text <- gsub("http*(.*)", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("https*(.*)", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("@\\w+", " ", überregional$text, perl=TRUE)
überregional$text <- gsub("\\n", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("lt3", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("…", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("’", " ", überregional$text, perl = TRUE)
überregional$text <- gsub("‘", " ", überregional$text, perl = TRUE)
#
corpusÜberregional <- Corpus(DataframeSource(überregional))
processedCorpus_überregional <- tm_map(corpusÜberregional, removeWords, allstopwords)
processedCorpus_überregional <- tm_map(processedCorpus_überregional, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus_überregional <- tm_map(processedCorpus_überregional, stripWhitespace)

################################################################################

minimumFrequency <- 5

################################################################################

DTMbayRn <- DocumentTermMatrix(processedCorpus_bayRn, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMbayRn)
DTMbayRn

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMbayRn) > 0
DTMbayRn <- DTMbayRn[sel_idx, ]
bayRn <- bayRn[sel_idx, ]

################################################################################

DTMbaden <- DocumentTermMatrix(processedCorpus_baden, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMbaden)
DTMbaden

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMbaden) > 0
DTMbaden <- DTMbaden[sel_idx, ]
baden <- baden[sel_idx, ]

################################################################################

DTMberl <- DocumentTermMatrix(processedCorpus_berl, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMberl)
DTMberl

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMberl) > 0
DTMberl <- DTMberl[sel_idx, ]
berl <- berl[sel_idx, ]

################################################################################

DTMbran <- DocumentTermMatrix(processedCorpus_bran, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMbran)
DTMbran

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMbran) > 0
DTMbran <- DTMbran[sel_idx, ]
bran <- bran[sel_idx, ]

################################################################################

DTMbremen <- DocumentTermMatrix(processedCorpus_bremen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMbremen)
DTMbremen

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMbremen) > 0
DTMbremen <- DTMbremen[sel_idx, ]
bremen <- bremen[sel_idx, ]

################################################################################

DTMhamburg <- DocumentTermMatrix(processedCorpus_hamburg, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMhamburg)
DTMhamburg

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMhamburg) > 0
DTMhamburg <- DTMhamburg[sel_idx, ]
hamburg <- hamburg[sel_idx, ]

################################################################################

DTMhessen <- DocumentTermMatrix(processedCorpus_hessen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMhessen)
DTMhessen

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMhessen) > 0
DTMhessen <- DTMhessen[sel_idx, ]
hessen <- hessen[sel_idx, ]

################################################################################

DTMmecklenburg <- DocumentTermMatrix(processedCorpus_mecklenburg, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMmecklenburg)
DTMmecklenburg

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMmecklenburg) > 0
DTMmecklenburg <- DTMmecklenburg[sel_idx, ]
mecklenburg <- mecklenburg[sel_idx, ]

################################################################################

DTMniedersachsen <- DocumentTermMatrix(processedCorpus_niedersachsen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMniedersachsen)
DTMniedersachsen

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMniedersachsen) > 0
DTMniedersachsen <- DTMniedersachsen[sel_idx, ]
niedersachsen <- niedersachsen[sel_idx, ]

################################################################################

DTMnordrhein <- DocumentTermMatrix(processedCorpus_nordrhein, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMnordrhein)
DTMnordrhein

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMnordrhein) > 0
DTMnordrhein <- DTMnordrhein[sel_idx, ]
nordrhein <- nordrhein[sel_idx, ]

################################################################################

DTMrheinland <- DocumentTermMatrix(processedCorpus_rheinland, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMrheinland)
DTMrheinland

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMrheinland) > 0
DTMrheinland <- DTMrheinland[sel_idx, ]
rheinland <- rheinland[sel_idx, ]

################################################################################

DTMsaarland <- DocumentTermMatrix(processedCorpus_saarland, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMsaarland)
DTMsaarland

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMsaarland) > 0
DTMsaarland <- DTMsaarland[sel_idx, ]
saarland <- saarland[sel_idx, ]

################################################################################

DTMsachsen <- DocumentTermMatrix(processedCorpus_sachsen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMsachsen)
DTMsachsen

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMsachsen) > 0
DTMsachsen <- DTMsachsen[sel_idx, ]
sachsen <- sachsen[sel_idx, ]

################################################################################

DTMsachsenAnhalt <- DocumentTermMatrix(processedCorpus_sachsenAnhalt, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMsachsenAnhalt)
DTMsachsenAnhalt

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMsachsenAnhalt) > 0
DTMsachsenAnhalt <- DTMsachsenAnhalt[sel_idx, ]
sachsenAnhalt <- sachsenAnhalt[sel_idx, ]

################################################################################

DTMschlesHolst <- DocumentTermMatrix(processedCorpus_schlesHolst, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMschlesHolst)
DTMschlesHolst

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMschlesHolst) > 0
DTMschlesHolst <- DTMschlesHolst[sel_idx, ]
schlesHolst <- schlesHolst[sel_idx, ]

################################################################################

DTMthüringen <- DocumentTermMatrix(processedCorpus_thüringen, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMthüringen)
DTMthüringen

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMthüringen) > 0
DTMthüringen <- DTMthüringen[sel_idx, ]
thüringen <- thüringen[sel_idx, ]

################################################################################

DTMüberregional <- DocumentTermMatrix(processedCorpus_überregional, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMüberregional)
DTMüberregional

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMüberregional) > 0
DTMüberregional <- DTMüberregional[sel_idx, ]
überregional <- überregional[sel_idx, ]

################################################################################

DTMöffentlicher <- DocumentTermMatrix(processedCorpus_öffentlicher, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMöffentlicher)
DTMöffentlicher

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMöffentlicher) > 0
DTMöffentlicher <- DTMöffentlicher[sel_idx, ]
öffentlicher <- öffentlicher[sel_idx, ]

################################################################################

DTMdigitale <- DocumentTermMatrix(processedCorpus_digitale, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMdigitale)
DTMdigitale

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMdigitale) > 0
DTMdigitale <- DTMdigitale[sel_idx, ]
digitale <- digitale[sel_idx, ]

################################################################################

DTMpolitikR <- DocumentTermMatrix(processedCorpus_politikR, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMpolitikR)
DTMpolitikR

## haut die observationen raus, die 0 als row sum haben ( also kein inhalt )
sel_idx <- slam::row_sums(DTMpolitikR) > 0
DTMpolitikR <- DTMpolitikR[sel_idx, ]
politikR <- politikR[sel_idx, ]

################################################################################

# optimalBayRn <- ldatuning::FindTopicsNumber(
#   DTMbayRn,
#   topics = seq(from = 2, to = 150, by = 10),
#   metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
#   method = "Gibbs",
#   control = list(seed = 0),
#   mc.cores = 3L,
#   verbose = T
# )
# 
# save(optimalBayRn, file = "CaoJuan-Deveaud_results/BAYERN_alleKoeff_k2-150_10er.RData")
# 
# x11()
# FindTopicsNumber_plot(optimalBayRn)

################################################################################

optimalBaden <- ldatuning::FindTopicsNumber(
  DTMbaden,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalBaden, file = "CaoJuan-Deveaud_results/BAWÜ_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalBaden)

################################################################################

optimalBerl <- ldatuning::FindTopicsNumber(
  DTMberl,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalBerl, file = "CaoJuan-Deveaud_results/BERLIN_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalBerl)

################################################################################

optimalBran <- ldatuning::FindTopicsNumber(
  DTMbran,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalBran, file = "CaoJuan-Deveaud_results/BRANDENBURG_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalBran)

################################################################################

optimalBremen <- ldatuning::FindTopicsNumber(
  DTMbremen,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalBremen, file = "CaoJuan-Deveaud_results/BREMEN_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalBremen)

################################################################################

optimalHamburg <- ldatuning::FindTopicsNumber(
  DTMhamburg,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalHamburg, file = "CaoJuan-Deveaud_results/HAMBURG_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalHamburg)

################################################################################

optimalHessen <- ldatuning::FindTopicsNumber(
  DTMhessen,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalHessen, file = "CaoJuan-Deveaud_results/HESSEN_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalHessen)

################################################################################

optimalMecklenburg <- ldatuning::FindTopicsNumber(
  DTMmecklenburg,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalMecklenburg, file = "CaoJuan-Deveaud_results/MECKLENBURG_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalMecklenburg)

################################################################################

optimalNiedersachsen <- ldatuning::FindTopicsNumber(
  DTMniedersachsen,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalNiedersachsen, file = "CaoJuan-Deveaud_results/NIEDERSACHSEN_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalNiedersachsen)

################################################################################

optimalNordrhein <- ldatuning::FindTopicsNumber(
  DTMnordrhein,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalNordrhein, file = "CaoJuan-Deveaud_results/NORDRHEIN_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalNordrhein)

################################################################################

optimalRheinland <- ldatuning::FindTopicsNumber(
  DTMrheinland,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalRheinland, file = "CaoJuan-Deveaud_results/RHEINLAND_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalRheinland)

################################################################################

optimalSaarland <- ldatuning::FindTopicsNumber(
  DTMsaarland,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalSaarland, file = "CaoJuan-Deveaud_results/SAARLAND_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalSaarland)

################################################################################

optimalSachsen <- ldatuning::FindTopicsNumber(
  DTMsachsen,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalSachsen, file = "CaoJuan-Deveaud_results/SACHSEN_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalSachsen)

################################################################################

optimalSachsenAnhalt <- ldatuning::FindTopicsNumber(
  DTMsachsenAnhalt,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalSachsenAnhalt, file = "CaoJuan-Deveaud_results/SACHSEN-ANHALT_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalSachsenAnhalt)

################################################################################

optimalSchlesHolst <- ldatuning::FindTopicsNumber(
  DTMschlesHolst,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalSchlesHolst, file = "CaoJuan-Deveaud_results/SCHLESWIG_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalSchlesHolst)

################################################################################

optimalThüringen <- ldatuning::FindTopicsNumber(
  DTMthüringen,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalThüringen, file = "CaoJuan-Deveaud_results/THÜRINGEN_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalThüringen)

################################################################################

optimalDigitale <- ldatuning::FindTopicsNumber(
  DTMdigitale,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalDigitale, file = "CaoJuan-Deveaud_results/DIGITALE_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalDigitale)

################################################################################

optimalPolitikR <- ldatuning::FindTopicsNumber(
  DTMpolitikR,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalPolitikR, file = "CaoJuan-Deveaud_results/POLITIKER_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalPolitikR)

################################################################################

optimalÖffentlicher <- ldatuning::FindTopicsNumber(
  DTMöffentlicher,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalÖffentlicher, file = "CaoJuan-Deveaud_results/ÖFFENTLICHER_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalÖffentlicher)

################################################################################

optimalÜberregional <- ldatuning::FindTopicsNumber(
  DTMüberregional,
  topics = seq(from = 2, to = 150, by = 10),
  metrics = c("CaoJuan2009", "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 0),
  mc.cores = 3L,
  verbose = T
)

save(optimalÜberregional, file = "CaoJuan-Deveaud_results/ÜBERREGIONAL_alleKoeff_k2-150_10er.RData")

x11()
FindTopicsNumber_plot(optimalÜberregional)

################################################################################ END OF FILE