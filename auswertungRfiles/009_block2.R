# auswertungsansätze für block 2

# ==============================================================================================================================================================
# ==============================================================================================================================================================
# => ERSTE ÜBERBLICKGRAPHIK: vergleich zwischen themenaufmerksamkeit zwischen pol und med im querschnitt, summiert, keine zeitreihe
# ==============================================================================================================================================================
# ==============================================================================================================================================================

# BILDUNG SUMME TWEETS PRO THEMA: politikerdaten
# ==============================================================================================================================================================
# themenüberblickPolitiker <- data.frame(apply(X = timePol_maximus[,c(5:19)], MARGIN = 2, FUN = sum)) # gleiches ergebnis, mehr text

# themenüberblick mittels apply und summierung tweets pro thema
themenüberblickPolitiker <- as_tidytable(apply(X = timePol_topics[,-1], MARGIN = 2, FUN = sum), .keep_rownames = T)
themenüberblickPolitiker <- themenüberblickPolitiker %>% rename(names = rn, anzahl = x)

# ansicht
themenüberblickPolitiker

# factor reorder absteigend
themenüberblickPolitiker <- themenüberblickPolitiker %>% mutate(names = fct_reorder(names, desc(-anzahl)))

# tweets politiker insgesamt
sum(themenüberblickPolitiker$anzahl)

# alternativ auch das möglich
# themenüberblickPolitiker <- data.frame(apply(X = politikerAnalysedatenThemen[,-1], MARGIN = 2, FUN = sum))

# ==============================================================================================================================================================

# BILDUNG SUMME TWEETS PRO THEMA: mediendaten
# unterschiedliche zahlen bei discpow datensatz vs. tagesbasis; hier nutzung der tagessummierung
# themenüberblickMedien <- data.frame(apply(X = medienAnalysedaten_discursivePower[,c(6:20)], MARGIN = 2, FUN = sum)) ## NICHT NUTZEN

themenüberblickMedien <- as_tidytable(apply(X = medienAnalysedaten_tagesbasis[,-1], MARGIN = 2, FUN = sum), .keep_rownames = T)
themenüberblickMedien <- themenüberblickMedien %>% rename(names = rn, anzahl = x)

# tidytable + ansicht
themenüberblickMedien

# factor reorder absteigend
themenüberblickMedien <- themenüberblickMedien %>% mutate(names = fct_reorder(names, desc(-anzahl)))

# tweets medien insgesamt
sum(themenüberblickMedien$anzahl)

# ==============================================================================================================================================================

## NORMALISIERUNG AUF DEN PROZENTUALEN ANTEIL VON POSTS PRO THEMA
# normalisierung politiker- und medienthemen mentions auf gesamtzahl aller themen-mentions
proportionalPolitiker <- as_tidytable(apply(themenüberblickPolitiker[,2], MARGIN = 2, FUN = newNormalPol))
proportionalPolitiker$names <- themenüberblickPolitiker$names

proportionalMedia <- as_tidytable(apply(themenüberblickMedien[,2], MARGIN = 2, FUN = newNormalMed))
proportionalMedia$names <- themenüberblickPolitiker$names

proportionalPolitiker <- proportionalPolitiker %>% mutate(names = fct_reorder(names, desc(-anzahl)))
# proportionalMedia <- proportionalMedia %>% mutate(names = fct_reorder(names, desc(-anzahl)))

# MIN-MAX-NORMALISIERUNG
processPol <- preProcess(as_tidytable(themenüberblickPolitiker), method = "range")
minmaxPolitiker <- predict(processPol, as_tidytable(themenüberblickPolitiker))

processMedien <- preProcess(as_tidytable(themenüberblickMedien), method = "range")
minmaxMedia <- predict(processMedien, as_tidytable(themenüberblickMedien))

# gruppe hinzufügen zu proportionalen datensätzen
proportionalPolitiker$GRUPPE <- "Politiker"
proportionalMedia$GRUPPE <- "Medien"

# ==============================================================================================================================================================

# GRAPHISCH
überblPol <- ggplot(data = proportionalPolitiker, mapping = aes(y=names, x=anzahl, fill=names)) +
  stat_summary(geom = "bar", fun = sum) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10, face = "bold")) +
  scale_x_percent(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) +
  scale_fill_manual(name="", values = topicsDistinctColours) +
  xlab("Anteil %") +
  ylab ("") +
  ggtitle("PolitikerInnen: Themenaufmerksamkeit", subtitle = "Prozentualer Anteil am Gesamtkorpus")

# ggplot(data= themenüberblickPolitiker) +
#    geom_bar(aes(y=anzahl, fill=names), position = "fill")

überblMedia <- ggplot(data = proportionalMedia, mapping = aes(y=names, x=anzahl, fill=names)) +
  stat_summary(geom = "bar", fun = sum) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10, face = "bold")) +
  scale_x_percent(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) +
  scale_fill_manual(name="", values = topicsDistinctColours) +
  xlab("Anteil %") +
  ylab ("") +
  ggtitle("Medien: Themenaufmerksamkeit", subtitle = "Prozentualer Anteil am Gesamtkorpus")

(themen_gegenüber_basic <- ggarrange(überblPol, überblMedia, legend = "none", labels = c("1)", "2)")))

# ==============================================================================================================================================================
# ==============================================================================================================================================================
# => ZWEITER TEIL VON GRAPHIKEN: zeitreihendaten





















