### modelle
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

k <- 19
## 19 ist der durchschnittswert für alle topics der medien über die länder hinweg

k2 <- 38
## 38 als durchschnittswert für alle topics der anderen, übergreifenden datensätze

k3 <- 6
## 8 als durchschnittswert für alle topics der politiker über die länder hinweg

k4 <- 13
## verrechnung der durchschnittswerte zwischen k und k3; besserer fit für regionale politiker als k3

################################################################################

set.seed(2121)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmBayern <- LDA(DTMbayRn, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmBayern_post <- posterior(tmBayern)
topicmodels::logLik(tmBayern)
#
termsBayern <- as_tibble(as.data.frame(topicmodels::terms(tmBayern, 25)))
write_csv(termsBayern, "mediendaten/termsBayern.csv")

# ### test mit bestem K für bayern
# set.seed(0220)
# ## iter default = 2000, 500 für schneller aber potentiell ungenauer
# tmBayern_TEST <- LDA(DTMbayRn, k = 22, method="Gibbs", control=list(iter = 500, verbose = 10))
# # tmBayern_post <- posterior(tmBayern)
# #
# topicmodels::terms(tmBayern_TEST, 15)

### gibt keine krassen neuen erkenntnisse, 19 scheint OK

################################################################################

set.seed(2222)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmBaden <- LDA(DTMbaden, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmBaden_post <- posterior(tmBaden)
#
termsBaden <- as_tibble(as.data.frame(topicmodels::terms(tmBaden, 25)))
write_csv(termsBaden, "mediendaten/termsBaden.csv")

################################################################################

set.seed(2323)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmBerlin <- LDA(DTMberl, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmBerlin_post <- posterior(tmBerlin)
#
termsBerlin <- as_tibble(as.data.frame(topicmodels::terms(tmBerlin, 25)))
write_csv(termsBerlin, "mediendaten/termsBerlin.csv")

################################################################################

set.seed(2424)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmBrandenburg <- LDA(DTMbran, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmBrandenburg_post <- posterior(tmBrandenburg)
#
termsBrandenburg <- as_tibble(as.data.frame(topicmodels::terms(tmBrandenburg, 25)))
write_csv(termsBrandenburg, "mediendaten/termsBrandenburg.csv")

################################################################################

set.seed(2525)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmBremen <- LDA(DTMbremen, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmBremen_post <- posterior(tmBremen)
#
termsBremen <- as_tibble(as.data.frame(topicmodels::terms(tmBremen, 25)))
write_csv(termsBremen, "mediendaten/termsBremen.csv")

################################################################################

set.seed(2626)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmHamburg <- LDA(DTMhamburg, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmHamburg_post <- posterior(tmHamburg)
#
termsHamburg <- as_tibble(as.data.frame(topicmodels::terms(tmHamburg, 25)))
write_csv(termsHamburg, "mediendaten/termsHamburg.csv")

################################################################################

set.seed(2727)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmHessen <- LDA(DTMhessen, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmHessen_post <- posterior(tmHessen)
#
termsHessen <- as_tibble(as.data.frame(topicmodels::terms(tmHessen, 25)))
write_csv(termsHessen, "mediendaten/termsHessen.csv")

################################################################################

set.seed(2828)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmMecklenburg <- LDA(DTMmecklenburg, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmMecklenburg_post <- posterior(tmMecklenburg)
#
termsMeckPomm <- as_tibble(as.data.frame(topicmodels::terms(tmMecklenburg, 25)))
write_csv(termsMeckPomm, "mediendaten/termsMeckPomm.csv")

################################################################################

set.seed(2929)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmNiedersachsen <- LDA(DTMniedersachsen, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmNiedersachsen_post <- posterior(tmNiedersachsen)
#
termsNDS <- as_tibble(as.data.frame(topicmodels::terms(tmNiedersachsen, 25)))
write_csv(termsNDS, "mediendaten/termsNDS.csv")

################################################################################

set.seed(3030)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmNRW <- LDA(DTMnordrhein, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmNRW_post <- posterior(tmNRW)
#
termsNRW <- as_tibble(as.data.frame(topicmodels::terms(tmNRW, 25)))
write_csv(termsNRW, "mediendaten/termsNRW.csv")

################################################################################

set.seed(3131)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmRLP <- LDA(DTMrheinland, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmRLP_post <- posterior(tmRLP)
#
termsRLP <- as_tibble(as.data.frame(topicmodels::terms(tmRLP, 25)))
write_csv(termsRLP, "mediendaten/termsRLP.csv")

################################################################################

set.seed(3232)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmSaarland <- LDA(DTMsaarland, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmSaarland_post <- posterior(tmSaarland)
#
termsSaarland <- as_tibble(as.data.frame(topicmodels::terms(tmSaarland, 25)))
write_csv(termsSaarland, "mediendaten/termsSaarland.csv")

################################################################################

set.seed(3333)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmSachsen <- LDA(DTMsachsen, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmSachsen_post <- posterior(tmSachsen)
#
topicmodels::terms(tmSachsen, 25)
termsSachsen <- as_tibble(as.data.frame(topicmodels::terms(tmSachsen, 25)))
write_csv(termsSachsen, "mediendaten/termsSachsen.csv")

################################################################################

set.seed(3434)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmSAn <- LDA(DTMsachsenAnhalt, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmSAn_post <- posterior(tmSAn)
#
termsSAn <- as_tibble(as.data.frame(topicmodels::terms(tmSAn, 25)))
write_csv(termsSAn, "mediendaten/termsSAn.csv")

################################################################################

set.seed(3535)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmSH <- LDA(DTMschlesHolst, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmSH_post <- posterior(tmSH)
#
termsSH <- as_tibble(as.data.frame(topicmodels::terms(tmSH, 25)))
write_csv(termsSH, "mediendaten/termsSH.csv")

################################################################################

set.seed(3636)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmTHÜ <- LDA(DTMthüringen, k, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmTHÜ_post <- posterior(tmTHÜ)
#
termsThü <- as_tibble(as.data.frame(topicmodels::terms(tmTHÜ, 25)))
write_csv(termsThü, file = "mediendaten/termsThü.csv")

################################################################################

set.seed(3737)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolitiker <- LDA(DTMpolitikR, k2, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPOL_post <- posterior(tmPolitiker)
#
termsPolitiker <- as_tibble(as.data.frame(topicmodels::terms(tmPolitiker, 25)))
write_csv(termsPolitiker, file = "mediendaten/termsPolitiker.csv")

################################################################################

set.seed(3838)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmDigitale <- LDA(DTMdigitale, k2, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmDIG_post <- posterior(tmDigitale)
#
termsDigitale <- as_tibble(as.data.frame(topicmodels::terms(tmDigitale, 25)))
write_csv(termsDigitale, file = "mediendaten/termsDigitale.csv")

################################################################################

set.seed(3939)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmÖRR <- LDA(DTMöffentlicher, k2, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmÖRR_post <- posterior(tmÖRR)
#
termsÖRR <- as_tibble(as.data.frame(topicmodels::terms(tmÖRR, 25)))
write_csv(termsÖRR, file = "mediendaten/termsÖRR.csv")

################################################################################

set.seed(4040)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmÜberregional <- LDA(DTMüberregional, k2, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmÜREG_post <- posterior(tmÜberregional)
#
termsÜberregional <- as_tibble(as.data.frame(topicmodels::terms(tmÜberregional, 25)))
write_csv(termsÜberregional, file = "mediendaten/termsÜberregional.csv")

################################################################################

set.seed(4141)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolBawü <- LDA(DTM_polBawü, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolBawü_post <- posterior(tmPolBawü)
#
termsPolBawü <- as_tibble(as.data.frame(topicmodels::terms(tmPolBawü, 25)))
write_csv(termsPolBawü, file = "daten_terms_politiker/BADEN_termsPolitiker.csv")

################################################################################

set.seed(4242)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolBay <- LDA(DTM_polBay, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolBay_post <- posterior(tmPolBay)
#
termsPolBay <- as_tibble(as.data.frame(topicmodels::terms(tmPolBay, 25)))
write_csv(termsPolBay, file = "daten_terms_politiker/BAYERN_termsPolitiker.csv")

################################################################################

set.seed(4343)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolBerlin <- LDA(DTM_polBerlin, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolBerlin_post <- posterior(tmPolBerlin)
#
termsPolBerlin <- as_tibble(as.data.frame(topicmodels::terms(tmPolBerlin, 25)))
write_csv(termsPolBerlin, file = "daten_terms_politiker/BERLIN_termsPolitiker.csv")

################################################################################

set.seed(4444)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolBrandenburg <- LDA(DTM_polBrandenburg, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolBrandenburg_post <- posterior(tmPolBrandenburg)
#
termsPolBrandenburg <- as_tibble(as.data.frame(topicmodels::terms(tmPolBrandenburg, 25)))
write_csv(termsPolBrandenburg, file = "daten_terms_politiker/BRANDENBURG_termsPolitiker.csv")

################################################################################

set.seed(4545)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolBremen <- LDA(DTM_polBremen, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolBremen_post <- posterior(tmPolBremen)
#
termsPolBremen <- as_tibble(as.data.frame(topicmodels::terms(tmPolBremen, 25)))
write_csv(termsPolBremen, file = "daten_terms_politiker/BREMEN_termsPolitiker.csv")

################################################################################

set.seed(4646)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolHamburg <- LDA(DTM_polHamburg, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolHamburg_post <- posterior(tmPolHamburg)
#
termsPolHamburg <- as_tibble(as.data.frame(topicmodels::terms(tmPolHamburg, 25)))
write_csv(termsPolHamburg, file = "daten_terms_politiker/HAMBURG_termsPolitiker.csv")

################################################################################

set.seed(4747)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolHessen <- LDA(DTM_polHessen, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolHessen_post <- posterior(tmPolHessen)
#
termsPolHessen <- as_tibble(as.data.frame(topicmodels::terms(tmPolHessen, 25)))
write_csv(termsPolHessen, file = "daten_terms_politiker/HESSEN_termsPolitiker.csv")

################################################################################

set.seed(4848)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolMeckPomm <- LDA(DTM_polMeckPomm, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolMeckPomm_post <- posterior(tmPolMeckPomm)
#
termsPolMeckPomm <- as_tibble(as.data.frame(topicmodels::terms(tmPolMeckPomm, 25)))
write_csv(termsPolMeckPomm, file = "daten_terms_politiker/MECKPOMM_termsPolitiker.csv")

################################################################################

set.seed(4949)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolNds <- LDA(DTM_polNds, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolNds_post <- posterior(tmPolNds)
#
termsPolNds <- as_tibble(as.data.frame(topicmodels::terms(tmPolNds, 25)))
write_csv(termsPolNds, file = "daten_terms_politiker/NIEDERSACHSEN_termsPolitiker.csv")

################################################################################

set.seed(5050)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolNrw <- LDA(DTM_polNrw, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolNrw_post <- posterior(tmPolNrw)
#
termsPolNrw <- as_tibble(as.data.frame(topicmodels::terms(tmPolNrw, 25)))
write_csv(termsPolNrw, file = "daten_terms_politiker/NRW_termsPolitiker.csv")

################################################################################

set.seed(5151)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolRlp <- LDA(DTM_polRlp, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolRlp_post <- posterior(tmPolRlp)
#
termsPolRlp <- as_tibble(as.data.frame(topicmodels::terms(tmPolRlp, 25)))
write_csv(termsPolRlp, file = "daten_terms_politiker/RHEINLAND_termsPolitiker.csv")

################################################################################

set.seed(5252)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolSaar <- LDA(DTM_polSaar, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolSaar_post <- posterior(tmPolSaar)
#
termsPolSaar <- as_tibble(as.data.frame(topicmodels::terms(tmPolSaar, 25)))
write_csv(termsPolSaar, file = "daten_terms_politiker/SAARLAND_termsPolitiker.csv")

################################################################################

set.seed(5353)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolSachsen <- LDA(DTM_polSachsen, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolSachsen_post <- posterior(tmPolSachsen)
#
termsPolSachsen <- as_tibble(as.data.frame(topicmodels::terms(tmPolSachsen, 25)))
write_csv(termsPolSachsen, file = "daten_terms_politiker/SACHSEN_termsPolitiker.csv")

################################################################################

set.seed(5454)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolSachsenAnhalt <- LDA(DTM_polSachsenAnhalt, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolSachsenAnhalt_post <- posterior(tmPolSachsenAnhalt)
#
termsPolSachsenAnhalt <- as_tibble(as.data.frame(topicmodels::terms(tmPolSachsenAnhalt, 25)))
write_csv(termsPolSachsenAnhalt, file = "daten_terms_politiker/SACHSENANHALT_termsPolitiker.csv")

################################################################################

set.seed(5555)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolSchleswig <- LDA(DTM_polSchleswig, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolSchleswig_post <- posterior(tmPolSchleswig)
#
termsPolSchleswig <- as_tibble(as.data.frame(topicmodels::terms(tmPolSchleswig, 25)))
write_csv(termsPolSchleswig, file = "daten_terms_politiker/SCHLESWIG_termsPolitiker.csv")

################################################################################

set.seed(5656)
## iter default = 2000, 500 für schneller aber potentiell ungenauer
tmPolThüringen <- LDA(DTM_polThüringen, k4, method="Gibbs", control=list(iter = 2000, verbose = 10))
tmPolThüringen_post <- posterior(tmPolThüringen)
#
termsPolThüringen <- as_tibble(as.data.frame(topicmodels::terms(tmPolThüringen, 25)))
write_csv(termsPolThüringen, file = "daten_terms_politiker/THÜRINGEN_termsPolitiker.csv")

################################################################################



