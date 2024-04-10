# auswertungsansätze für block 2

# =========================================================================================================================================================
# =========================================================================================================================================================
# => ERSTE ÜBERBLICKGRAPHIK: vergleich zwischen themenaufmerksamkeit zwischen pol und med im querschnitt, summiert, keine zeitreihe
# =========================================================================================================================================================
# =========================================================================================================================================================

# andere zeitebenen

timePol_topics$weekAsDate <- as.Date(cut(timePol_topics$dateTime, breaks = "week"))
timePol_topics$weeknumber <- lubridate::week(timePol_topics$dateTime)
timePol_topics$monthAsDate <- as.Date(cut(timePol_topics$dateTime, breaks = "month"))
timePol_topics$monthNumber <- lubridate::month(timePol_topics$dateTime)

timeMedia_topics$weekAsDate <- as.Date(cut(timeMedia_topics$dateTime, breaks = "week"))
timeMedia_topics$weeknumber <- lubridate::week(timeMedia_topics$dateTime)
timeMedia_topics$monthAsDate <- as.Date(cut(timeMedia_topics$dateTime, breaks = "month"))
timeMedia_topics$monthNumber <- lubridate::month(timeMedia_topics$dateTime)

# BILDUNG SUMME TWEETS PRO THEMA: politikerdaten
# ==============================================================================================================================================================
# themenüberblickPolitiker <- data.frame(apply(X = timePol_maximus[,c(5:19)], MARGIN = 2, FUN = sum)) # gleiches ergebnis, mehr text

# themenüberblick mittels apply und summierung tweets pro thema
themenüberblickPolitiker <- as_tidytable(apply(X = timePol_topics[,-c(1, 17:20)], MARGIN = 2, FUN = sum), .keep_rownames = T)
themenüberblickPolitiker <- themenüberblickPolitiker %>% rename(names = rn, anzahl = x)

# ansicht
themenüberblickPolitiker

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

# tweets medien insgesamt
sum(themenüberblickMedien$anzahl)

# ==============================================================================================================================================================

# variablennamen anpassen
themenüberblickMedien$names2 <- c("Covid", "Ukraine", "Energie", "Soziales", "Verteidigungspolitik", "PolizistenmordKusel", "FlutAhrtal",
                              "PolitikEuropa", "PolitikInternational", "Klima", "ProtesteIran", "Verkehr",
                              "PluralismusMedien", "Zukunft", "Verfassungsfeindlich")
themenüberblickPolitiker$names2 <- c("Covid", "Ukraine", "Energie", "Soziales", "Verteidigungspolitik", "PolizistenmordKusel", "FlutAhrtal",
                                  "PolitikEuropa", "PolitikInternational", "Klima", "ProtesteIran", "Verkehr",
                                  "PluralismusMedien", "Zukunft", "Verfassungsfeindlich")

# factor reorder absteigend
themenüberblickMedien <- themenüberblickMedien %>% mutate(names2 = fct_reorder(names2, desc(-anzahl)))
# factor reorder absteigend
themenüberblickPolitiker <- themenüberblickPolitiker %>% mutate(names2 = fct_reorder(names2, desc(-anzahl)))

# ==============================================================================================================================================================

## NORMALISIERUNG AUF DEN PROZENTUALEN ANTEIL VON POSTS PRO THEMA
# normalisierung politiker- und medienthemen mentions auf gesamtzahl aller themen-mentions
proportionalPolitiker <- as_tidytable(apply(themenüberblickPolitiker[,2], MARGIN = 2, FUN = newNormalPol))
proportionalPolitiker$names <- themenüberblickPolitiker$names2

proportionalMedia <- as_tidytable(apply(themenüberblickMedien[,2], MARGIN = 2, FUN = newNormalMed))
proportionalMedia$names <- themenüberblickPolitiker$names2

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

(themen_gegenüber_basic <- ggarrange(überblPol, überblMedia, legend = "none", labels = c("1", "2")))


# ==============================================================================================================================================================
# ZWEITER TEIL VON GRAPHIKEN: zeitreihendaten
# ==============================================================================================================================================================

# MINMAX POLITIKER
politikerMinMax <- as_tidytable(apply(timePol_topics[,-c(1, 17:20)],  MARGIN = 2, FUN = minMaxNorm))

# DATUM
politikerMinMax$dateTime <- timePol_topics$dateTime
politikerMinMax$weekAsDate <- timePol_topics$weekAsDate
politikerMinMax$weeknumber <- timePol_topics$weeknumber
politikerMinMax$monthAsDate <- timePol_topics$monthAsDate
politikerMinMax$monthNumber <- timePol_topics$monthNumber

# MINMAX MEDIEN
medienMinMax <- as_tidytable(apply(timeMedia_topics[,-c(1, 17:20)],  MARGIN = 2, FUN = minMaxNorm))

# DATUM
medienMinMax$dateTime <- timeMedia_topics$dateTime
medienMinMax$weekAsDate <- timeMedia_topics$weekAsDate
medienMinMax$weeknumber <- timeMedia_topics$weeknumber
medienMinMax$monthAsDate <- timeMedia_topics$monthAsDate
medienMinMax$monthNumber <- timeMedia_topics$monthNumber

# plot(y = medienProzentualisiert$ukraine, x = medienProzentualisiert$dateTime)


# PROZENTUALISIERT POLITIKER
politikerProzentualisiert <- as_tidytable(apply(timePol_topics[,-c(1, 17:20)], MARGIN = 2, FUN = formel_prozentualisierung))

politikerProzentualisiert$dateTime <- timePol_topics$dateTime
politikerProzentualisiert$weekAsDate <- timePol_topics$weekAsDate
politikerProzentualisiert$weeknumber <- timePol_topics$weeknumber
politikerProzentualisiert$monthAsDate <- timePol_topics$monthAsDate
politikerProzentualisiert$monthNumber <- timePol_topics$monthNumber

# MEDIEN PROZENTUALISIERT
medienProzentualisiert <- as_tidytable(apply(timeMedia_topics[,-c(1, 17:20)],  MARGIN = 2, FUN = formel_prozentualisierung))
# as_tidytable(apply(timeMedia_discPow_days[,-c(1:6, 22:25)],  MARGIN = 2, FUN = formel_prozentualisierung))
## alternative mit discursive power datensatz: pro user UND pro tag

medienProzentualisiert$dateTime <- timeMedia_topics$dateTime
medienProzentualisiert$weekAsDate <- timePol_topics$weekAsDate
medienProzentualisiert$weeknumber <- timePol_topics$weeknumber
medienProzentualisiert$monthAsDate <- timeMedia_topics$monthAsDate
medienProzentualisiert$monthNumber <- timePol_topics$monthNumber

## prototyp
(politikerCov <- ggplot() +
  geom_line(data = politikerProzentualisiert, aes(x=dateTime, y=ukraine, color="Politiker")) +
  #geom_line(data = timeMedia_topics, aes(x=dateTime, y=ukraine, color="Medien")) +
  scale_color_manual(name="Akteursgruppe", values = politikerMedienFarben) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  xlab("") +
  scale_y_percent() +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle("PolitikerInnen: Zeitverlauf Thema Covid"))

(medienCov <- ggplot() +
  #geom_line(data = timePol_topics, aes(x=dateTime, y=ukraine, color="Politiker")) +
  geom_line(data = medienProzentualisiert, aes(x=dateTime, y=ukraine, color="Medien")) +
  scale_color_manual(name="Akteursgruppe", values = politikerMedienFarben) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  xlab("") +
  scale_y_percent(breaks = c(0, 0.005, 0.01, 0.015, 0.02), labels = c(0, "0.5%", "1%", "1.5%", 0.02)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle("Medien: Zeitverlauf Thema Covid", subtitle = "Visualisierung auf Tagesbasis"))

ggarrange(medienCov, politikerCov, ncol = 1, nrow = 2, common.legend = F, legend = "none", labels = c("1", "2"))

# ==============================================================================================================================================================
# diagramm + ggarrange für ukraine und energie in wöchentlicher auflösung für politikerinnen und medien

highlightUKR <- data.frame(xmin = "2022-02-14", xmax = "2022-04-03", ymin = -Inf, ymax = Inf) # daten: einmarsch bis massaker von bucha
highlightUKR$xmin <- as.Date(highlightUKR$xmin)
highlightUKR$xmax <- as.Date(highlightUKR$xmax)

highlightENER_nordstreamII <- data.frame(xmin = as.Date("2022-08-31"), xmax = as.Date("2022-09-02"), ymin = -Inf, ymax = Inf)
highlightENER_nordstreamI <- data.frame(xmin = as.Date("2022-07-11"), xmax = as.Date("2022-07-21"), ymin = -Inf, ymax = Inf)
highlightENER_wendepunkt <- data.frame(xmin = as.Date("2022-06-15"), xmax = as.Date("2022-07-01"), ymin = -Inf, ymax = Inf)

# ====================================================================================================================

(wöchentlichPOLITIKER_prozent_UKR_ENER <- politikerProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=ukraine, colour = "Ukraine"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=energie, colour = "Energie"), fun = sum) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent(breaks = seq(0, .13, .02), labels = c("0%", "2%", "4%", "6%", "8%", "10%", "12%"), limits = c(0, .13)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 9)) +
  # ggtitle("PolitikerInnen (1) und Medien (2): Themenbehandlung Ukraine und Energie im Zeitverlauf",
  #         subtitle = "Zeitebene: wöchentlich | Angaben: Anteil in % am Gesamtvolumen des Themas pro Zeiteinheit") +
  geom_rect(data=highlightUKR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="black",
            fill="#0000cdaa",
            alpha=0.5,
            inherit.aes = FALSE) +
  annotate(geom="point", x=as.Date("2022-02-21"), y=0.125, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Russische\nInvasion", x=as.Date("2022-03-04"), y=0.12, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-02-27"), y=0.07, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Zeitenwende-Rede", x=as.Date("2022-03-16"), y=0.07, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-04-03"), y=0.033, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Massaker von\nBucha", x=as.Date("2022-04-15"), y=0.045, color = "#0000cd", size=3.5) +
  geom_rect(data=highlightENER_nordstreamI, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#00ff00aa",
            color="black",
            alpha=0.5,
            inherit.aes = FALSE) +
 geom_rect(data=highlightENER_nordstreamII, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
           fill="#00ff00aa",
           color="black",
           alpha=0.5,
           inherit.aes = FALSE) +
   geom_rect(data=highlightENER_wendepunkt, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
             fill="#00ff00aa",
             color="black",
             alpha=0.5,
             inherit.aes = FALSE) +
  annotate(geom="text", label = "Lieferengpässe\nNord-Stream-1", x=as.Date("2022-08-10"), y=0.085, color = "#00ff00", size=4.5) +
  annotate(geom="point", x=as.Date("2022-09-04"), y=0.05, size=10, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Entlastungs-\npaket 3", x=as.Date("2022-09-12"), y=0.063, color = "#00ff00", size=3) +
  annotate(geom="point", x=as.Date("2022-09-26"), y=0.06, size=10, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Scholz'\nDoppel-Wumms", x=as.Date("2022-10-01"), y=0.075, color = "#00ff00", size=3.5) +
  annotate(geom="text", label = "Alarmstufe\nGas &\nerste\nAKW-Diskussionen", x=as.Date("2022-06-01"), y=0.085, color = "#00ff00", size=3.5))

# ====================================================================================================================

(wöchentlichMEDIEN_prozent_UKR_ENER <- medienProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=ukraine, colour = "Ukraine"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=energie, colour = "Energie"), fun = sum) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent(limits = c(0, .09), breaks = seq(0, .1, .02)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9)) +
  #ggtitle("Medien: Themenbehandlung Ukraine und Energie im Zeitverlauf",
  #        subtitle = "Zeitebene: wöchentlich | Angaben: Anteil in % am Gesamtvolumen des Themas pro Zeiteinheit") +
  geom_rect(data=highlightUKR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#0000cdaa",
            color="black",
            alpha=0.5,
            inherit.aes = FALSE) +
  annotate(geom="point", x=as.Date("2022-02-21"), y=0.066, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Russische\nInvasion", x=as.Date("2022-02-28"), y=0.055, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-02-28"), y=0.082, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Zeitenwende-Rede", x=as.Date("2022-03-15"), y=0.085, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-04-03"), y=0.04, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Massaker von\nBucha", x=as.Date("2022-04-14"), y=0.048, color = "#0000cd", size=3.5) +
  geom_rect(data=highlightENER_nordstreamI, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#00ff00aa",
            color="black",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_rect(data=highlightENER_nordstreamII, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
           fill="#00ff00aa",
           color="black",
           alpha=0.5,
           inherit.aes = FALSE) +
  geom_rect(data=highlightENER_wendepunkt, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
             fill="#00ff00aa",
             color="black",
             alpha=0.5,
             inherit.aes = FALSE) +
  annotate(geom="text", label = "Lieferengpässe\nNord-Stream-1", x=as.Date("2022-08-10"), y=0.065, color = "#00ff00", size=4.5) +
  annotate(geom="point", x=as.Date("2022-09-04"), y=0.039, size=10, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Entlastungspaket 3", x=as.Date("2022-09-14"), y=0.027, color = "#00ff00", size=3) +
  annotate(geom="point", x=as.Date("2022-09-26"), y=0.049, size=10, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Scholz'\n'Doppel-Wumms'", x=as.Date("2022-10-01"), y=0.061, color = "#00ff00", size=3.5) +
  annotate(geom="text", label = "Alarmstufe\nGas &\nerste\nAKW-Diskussionen", x=as.Date("2022-06-01"), y=0.065, color = "#00ff00", size=3.5))



# arrangement beider sphären für die zwei ersten themen
medPol_UKR_ENER_ts <- ggarrange(nrow = 2, ncol = 1,
                                wöchentlichPOLITIKER_prozent_UKR_ENER,
                                wöchentlichMEDIEN_prozent_UKR_ENER,
                                labels = c("1", "2"),
                                common.legend = T,
                                legend = "bottom")
annotate_figure(medPol_UKR_ENER_ts,
                bottom=text_grob("Verteilung von Anteilen der Tweets am Gesamtvolumen eines Themas, PolitikerInnen zu Medien: 11% zu 89% (Ukraine) bzw. 16% zu 84% (Energie).",
                                 #face = "italic",
                                 size = 8,
                                 color = "grey20"),
                top = text_grob("PolitikerInnen (1) und Medien (2): Behandlung der Themen Ukraine und Energie im Zeitverlauf (Zeit = wöchentlich)",
                                face = "bold",
                                size = 16,
                                color = "black"))

# # potenzielle ergänzung: smoother line
# (wöchentlichPOLITIKER_smoother_UKR_ENER <- ggplot(timePol_topics) +
#   # stat_summary(geom = "line", aes(x=weekAsDate, y=ukraine, colour = "ukraine"), fun = sum) +
#   # stat_summary(geom = "line", aes(x=weekAsDate, y=energie, colour = "energie"), fun = sum) +
#   stat_smooth(aes(x=dateTime, y=ukraine, colour = "Ukraine")) +
#   stat_smooth(aes(x=dateTime, y=energie, colour = "Energie")) +
#   scale_color_manual(name="Thema", values = topicsDistinctColours) +
#   scale_x_date(breaks = "1 month", labels = date_format(format = "%b, KW %W", locale = "de")) +
#   xlab("") +
#   #scale_y_percent(breaks = c(0, .002, .004, .006), labels = c(0, "0,2%", "0,4%", "0,6%")) +
#   ylab("Anteil % Gesamtvolumen") +
#   theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
#   ggtitle("Ukraine und Energie: Themenaufmerksamkeit im Zeitverlauf im Vergleich",
#           subtitle = "Zeitebene: wöchentlich | Angaben: Anteil in % am Gesamtvolumen des Themas pro Zeiteinheit"))

# ==============================================================================================================================================================
# DARSTELLUNG DES KIPP-PUNKTES: wann wird energie stärker als ukraine?
# kipppunkt ist früher bei den politikern
# generell gilt: im sommer, mai bis junig 2022, ist der kipp-punkt

politikerZoom <- politikerProzentualisiert %>% filter(monthNumber >= 4 & monthNumber <= 9)
medienZoom <- medienProzentualisiert %>% filter(monthNumber >= 4 & monthNumber <= 9)

ggplot() +
  stat_summary(data = politikerZoom, geom = "line", aes(x=monthAsDate, y=ukraine, colour = "PolitikerInnen"), fun = sum) +
  stat_summary(data = politikerZoom, geom = "line", aes(x=monthAsDate, y=energie, colour = "PolitikerInnen"), fun = sum) +
  stat_summary(data = medienZoom, geom = "line", aes(x=monthAsDate, y=ukraine, colour = "Medien"), fun = sum) +
  stat_summary(data = medienZoom, geom = "line", aes(x=monthAsDate, y=energie, colour = "Medien"), fun = sum) +
  scale_color_manual(name="Thema", values = medienPolitikerFarben) +
  scale_x_date(breaks = "1 week", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent() +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle("Medien: Themenbehandlung Ukraine und Energie im Zeitverlauf",
          subtitle = "Zeitebene: wöchentlich | Angaben: Anteil in % am Gesamtvolumen des Themas pro Zeiteinheit")

# ==============================================================================================================================================================
# covid und soziales

# highlights
highlightCOV <- data.frame(xmin = as.Date("2022-01-01"), xmax = as.Date("2022-02-14"), ymin = -Inf, ymax = Inf) # thema impfpflicht
highlightSOZ <- data.frame(xmin = as.Date("2022-03-14"), xmax = as.Date("2022-03-24"), ymin = -Inf, ymax = Inf) # entlastungspaket 2
highlightSOZII <- data.frame(xmin = as.Date("2022-08-26"), xmax = as.Date("2022-09-04"), ymin = -Inf, ymax = Inf) # entlastungspaket 3
highlightSOZIII <- data.frame(xmin = as.Date("2022-05-26"), xmax = as.Date("2022-06-04"), ymin = -Inf, ymax = Inf) # mindestlohn,rente,9euroticket

wöchentlichPOLITIKER_prozent_COV_SOZ <- politikerProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=covid, colour = "Covid"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=soziales, colour = "Soziales"), fun = sum) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent(limits = c(0, .12), breaks = seq(0, .12, .02)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 9)) +
  #ggtitle("PolitikerInnen: Themenbehandlung Covid und Soziales im Zeitverlauf",
  #        subtitle = "Zeitebene: wöchentlich | Angaben: Anteil in % am Gesamtvolumen des Themas pro Zeiteinheit") +
  geom_rect(data=highlightCOV, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#6b8523aa",
            color="black",alpha=0.5,
            inherit.aes = FALSE) +
  annotate(geom="text", label = "Scheitern der\nallg. Impfpflicht", x=as.Date("2022-04-17"), y=0.105, color = "#6b8e23", size=3.5) +
  annotate(geom="point", x=as.Date("2022-04-04"), y=0.113, size=10, shape=21, fill="transparent", color="#6b8e23") +
  annotate(geom="text", label = "Thema\nImpfen", x=as.Date("2021-12-22"), y=0.105, color = "#6b8e23", size=3.5) +
  geom_rect(data=highlightSOZ, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#ff0000aa",
            color="black",alpha=0.5,
            inherit.aes = FALSE) +
  geom_rect(data=highlightSOZII, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#ff0000aa",
            color="black",alpha=0.5,
            inherit.aes = FALSE) +
  geom_rect(data=highlightSOZIII, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#ff0000aa",
            color="black",alpha=0.5,
            inherit.aes = FALSE) +
  annotate(geom="text", label = "Entlastungs-\npaket 2", x=as.Date("2022-03-03"), y=0.07, color = "#ff0000", size=3.5) +
  annotate(geom="text", label = "Beschluss Mindest-\nlohn, Rente &\nSondervermögen;\nEinführung\n9-Euro-Ticket",
           x=as.Date("2022-05-12"), y=0.07,
           color = "#ff0000", size=3.5) +
  annotate(geom="text", label = "Entlastungs-\npaket 1", x=as.Date("2022-02-23"), y=0.04, color = "#ff0000", size=3.5) +
  annotate(geom="point", x=as.Date("2022-02-23"), y=0.023, size=10, shape=21, fill="transparent", color="#ff0000") +
  annotate(geom="point", x=as.Date("2022-09-26"), y=0.037, size=10, shape=21, fill="transparent", color="#ff0000") +
  annotate(geom="text", label = "Scholz'\n'Doppel-Wumms'", x=as.Date("2022-10-01"), y=0.055, color = "#ff0000", size=3.5) +
  annotate(geom="text", label = "Entlastungs-\npaket 3", x=as.Date("2022-08-16"), y=0.07, color = "#ff0000", size=3.5)

wöchentlichMEDIEN_prozent_COV_SOZ <- medienProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=covid, colour = "Covid"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=soziales, colour = "Soziales"), fun = sum) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent(limits = c(0, .08), breaks = seq(0, .08, .02)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  # ggtitle("Medien: Themenbehandlung Covid und Soziales im Zeitverlauf",
  #         subtitle = "Zeitebene: wöchentlich | Angaben: Anteil in % am Gesamtvolumen des Themas pro Zeiteinheit") +
    geom_rect(data=highlightCOV, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="#6b8523aa",
              color="black",alpha=0.5,
              inherit.aes = FALSE) +
    annotate(geom="text", label = "Scheitern der\nallg. Impfpflicht", x=as.Date("2022-04-05"), y=0.045, color = "#6b8e23", size=3.5) +
    annotate(geom="point", x=as.Date("2022-04-04"), y=0.031, size=10, shape=21, fill="transparent", color="#6b8e23") +
    annotate(geom="text", label = "Thema\nImpfen", x=as.Date("2021-12-22"), y=0.06, color = "#6b8e23", size=3.5) +
    geom_rect(data=highlightSOZ, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="#ff0000aa",
              color="black",alpha=0.5,
              inherit.aes = FALSE) +
    annotate(geom="text", label = "Entlastungs-\npaket 2", x=as.Date("2022-03-03"), y=0.05, color = "#ff0000", size=3.5) +
    geom_rect(data=highlightSOZII, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="#ff0000aa",
              color="black",alpha=0.5,
              inherit.aes = FALSE) +
    annotate(geom="text", label = "Beschluss Mindest-\nlohn, Rente &\nSondervermögen;\nEinführung\n9-Euro-Ticket",
             x=as.Date("2022-05-12"), y=0.05,
             color = "#ff0000", size=3.5) +
    geom_rect(data=highlightSOZIII, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="#ff0000aa",
              color="black",alpha=0.5,
              inherit.aes = FALSE) +
    annotate(geom="text", label = "Entlastungs-\npaket 1", x=as.Date("2022-02-25"), y=0.01, color = "#ff0000", size=3.5) +
    annotate(geom="point", x=as.Date("2022-02-23"), y=0.019, size=10, shape=21, fill="transparent", color="#ff0000") +
    annotate(geom="text", label = "Scholz'\n'Doppel-Wumms'", x=as.Date("2022-10-01"), y=0.045, color = "#ff0000", size=3.5) +
    annotate(geom="point", x=as.Date("2022-09-26"), y=0.029, size=10, shape=21, fill="transparent", color="#ff0000") +
    annotate(geom="text", label = "Entlastungs-\npaket 3", x=as.Date("2022-08-16"), y=0.05, color = "#ff0000", size=3.5)

# arrangement beider sphären für die zwei ersten themen
medPol_COV_SOZ_ts <- ggarrange(nrow = 2, ncol = 1,
          wöchentlichPOLITIKER_prozent_COV_SOZ,
          wöchentlichMEDIEN_prozent_COV_SOZ,
          labels = c("1", "2"),
          common.legend = T,
          legend = "bottom")
annotate_figure(medPol_COV_SOZ_ts,
                bottom=text_grob("Verteilung von Anteilen der Tweets am Gesamtvolumen eines Themas, PolitikerInnen zu Medien: 6% zu 94% (Covid) bzw. 14% zu 86% (Soziales).",
                                 #face = "italic",
                                 size = 8,
                                 color = "grey20"),
                top = text_grob("PolitikerInnen (1) und Medien (2): Behandlung der Themen Covid und Soziales im Zeitverlauf (Zeit = wöchentlich)",
                                face = "bold",
                                size = 16,
                                color = "black"))

# ==============================================================================================================================================================
# ukraine und covid
# warum die zwei? ---> peaks ähneln sich zeitlich; außerdem ähnliche dynamik wie bei covid und klima nach rauchfleisch zu vermuten; killer issue UKR?

(wöchentlichPOLITIKER_prozent_UKR_COV <- politikerProzentualisiert %>% ggplot() +
    stat_summary(geom = "line", aes(x=weekAsDate, y=ukraine, colour = "Ukraine"), fun = sum) +
    stat_summary(geom = "line", aes(x=weekAsDate, y=covid, colour = "Covid"), fun = sum) +
    scale_color_manual(name="", values = topicsDistinctColours) +
    scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
    xlab("") +
    scale_y_percent(breaks = seq(0, .13, .02), labels = c("0%", "2%", "4%", "6%", "8%", "10%", "12%"), limits = c(0, .13)) +
    ylab("Anteil % Gesamtvolumen") +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 9)) +
    # ggtitle("PolitikerInnen (1) und Medien (2): Themenbehandlung Ukraine und Energie im Zeitverlauf",
    #         subtitle = "Zeitebene: wöchentlich | Angaben: Anteil in % am Gesamtvolumen des Themas pro Zeiteinheit") +
    geom_rect(data=highlightUKR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color="black",
              fill="#0000cdaa",
              alpha=0.5,
              inherit.aes = FALSE) +
    geom_rect(data=highlightCOV, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color="black",
              fill="#6b8e23aa",
              alpha=0.5,
              inherit.aes = FALSE) +
    annotate(geom="point", x=as.Date("2022-02-21"), y=0.125, size=10, shape=21, fill="transparent", color="#0000cd") +
    annotate(geom="text", label = "Russische\nInvasion", x=as.Date("2022-03-06"), y=0.116, color = "#0000cd", size=3.5) +
    annotate(geom="point", x=as.Date("2022-02-27"), y=0.07, size=10, shape=21, fill="transparent", color="#0000cd") +
    annotate(geom="text", label = "Zeitenwende-Rede", x=as.Date("2022-03-16"), y=0.084, color = "#0000cd", size=3.5) +
    annotate(geom="point", x=as.Date("2022-04-03"), y=0.033, size=10, shape=21, fill="transparent", color="#0000cd") +
    annotate(geom="text", label = "Massaker von\nBucha", x=as.Date("2022-04-15"), y=0.054, color = "#0000cd", size=3.5) +
    annotate(geom="text", label = "Scheitern der\nImpfpflicht", x=as.Date("2022-04-19"), y=0.105, color = "#6b8e23", size=3.5) +
    annotate(geom="point", x=as.Date("2022-04-04"), y=0.115, size=10, shape=21, fill="transparent", color="#6b8e23") +
    annotate(geom="text", label = "Thema\nImpfen", x=as.Date("2021-12-22"), y=0.105, color = "#6b8e23", size=3.5))

# ====================================================================================================================

wöchentlichMEDIEN_prozent_UKR_COV <- medienProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=ukraine, colour = "Ukraine"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=covid, colour = "Covid"), fun = sum) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent(limits = c(0, .09), breaks = seq(0, .1, .02)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9)) +
  # ggtitle("Medien: Themenbehandlung Ukraine und Energie im Zeitverlauf",
  #         subtitle = "Zeitebene: wöchentlich | Angaben: Anteil in % am Gesamtvolumen des Themas pro Zeiteinheit") +
  geom_rect(data=highlightUKR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#0000cdaa",
            color="black",
            alpha=0.5,
            inherit.aes = FALSE) +
  annotate(geom="point", x=as.Date("2022-02-21"), y=0.066, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Russische\nInvasion", x=as.Date("2022-03-02"), y=0.055, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-02-27"), y=0.083, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Zeitenwende-\nRede", x=as.Date("2022-03-20"), y=0.078, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-04-03"), y=0.04, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Massaker von\nBucha", x=as.Date("2022-04-15"), y=0.052, color = "#0000cd", size=3.5) +
  geom_rect(data=highlightCOV, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
             fill="#6b8523aa",
             color="black",
             alpha=0.5,
             inherit.aes = FALSE) +
  annotate(geom="text", label = "Scheitern der\nImpfpflicht", x=as.Date("2022-04-25"), y=0.023, color = "#6b8e23", size=3.5) +
  annotate(geom="point", x=as.Date("2022-04-04"), y=0.031, size=10, shape=21, fill="transparent", color="#6b8e23") +
  annotate(geom="text", label = "Thema\nImpfen", x=as.Date("2021-12-22"), y=0.07, color = "#6b8e23", size=3.5)



# arrangement beider sphären für die zwei ersten themen
medPol_UKR_COV_ts <- ggarrange(nrow = 2, ncol = 1,
                                wöchentlichPOLITIKER_prozent_UKR_COV,
                                wöchentlichMEDIEN_prozent_UKR_COV,
                                labels = c("1", "2"), common.legend = T, legend = "bottom")
annotate_figure(medPol_UKR_COV_ts,
                bottom=text_grob("Verteilung von Anteilen der Tweets am Gesamtvolumen eines Themas, Politiker zu Medien: 6% zu 94% (Covid) bzw. 11% zu 89% (Ukraine).",
                                 #face = "italic",
                                 size = 8,
                                 color = "grey20"),
                top = text_grob("PolitikerInnen (1) und Medien (2): Behandlung der Themen Ukraine und Energie im Zeitverlauf (Zeit = wöchentlich)",
                                face = "bold",
                                size = 16,
                                color = "black"))



# gesamtüberblick alle themen in eine graphik packen
gesamtüberblickMedien <- medienProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=ukraine, colour = "Ukraine"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=covid, colour = "Covid"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=energie, colour = "Energie"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=soziales, colour = "Soziales"), fun = sum) +
  geom_vline(colour="tomato", xintercept = as.Date("2022-02-28")) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent(limits = c(0, .09), breaks = seq(0, .1, .02)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9))

gesamtüberblickPolitiker <- politikerProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=ukraine, colour = "Ukraine"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=covid, colour = "Covid"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=energie, colour = "Energie"), fun = sum) +
  stat_summary(geom = "line", aes(x=weekAsDate, y=soziales, colour = "Soziales"), fun = sum) +
  geom_vline(colour="tomato", xintercept = as.Date("2022-02-21")) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent(breaks = seq(0, .13, .02), labels = c("0%", "2%", "4%", "6%", "8%", "10%", "12%"), limits = c(0, .13)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9))

gesamtüberblickBeide <- ggarrange(nrow = 2, ncol = 1,
                               gesamtüberblickPolitiker,
                               gesamtüberblickMedien,
                               labels = c("1", "2"), common.legend = T, legend = "bottom")
annotate_figure(gesamtüberblickBeide,
                top = text_grob("PolitikerInnen (1) und Medien (2): Top-Vier-Themen im Zeitverlauf (wöchentlich)",
                                face = "bold",
                                size = 16,
                                color = "black"),
                bottom=text_grob("Rote Linie = Peak-Punkte des Themas Ukraine.",
                                 #face = "italic",
                                 size = 8,
                                 color = "tomato"))

# zusatzgraphik killer issue ukraine vs covid
# tagesebene
# mit smoother

medienMinmax_killerissue <- medienMinMax %>% ggplot() +
  geom_line(aes(x=dateTime, y=ukraine), colour = "#0000cdaa") +
  geom_line(aes(x=dateTime, y=covid), colour = "#6b8e23aa") +
  annotate(geom="point", x=as.Date("2022-02-24"), y=1.0, size=9, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Russische\nInvasion", x=as.Date("2022-03-07"), y=0.95, color = "#0000cd", size=3.5) +
  geom_smooth(aes(x=dateTime, y=ukraine, colour = "Ukraine"), method = "gam", se = F) +
  geom_smooth(aes(x=dateTime, y=covid, colour = "Covid"), method = "gam", se = F) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de"),
               limits = as.Date(c("2022-01-01", "2022-10-31")), expand = c(0,0)) +
  xlab("") +
  scale_y_percent(expand = c(0,0.1)) +
  ylab("") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic")) +
  ggtitle("Medien")

politikerMinmax_killerissue <- politikerMinMax %>% ggplot() +
  geom_line(aes(x=dateTime, y=ukraine), colour = "#0000cdaa") +
  geom_line(aes(x=dateTime, y=covid), colour = "#6b8e23aa") +
  annotate(geom="point", x=as.Date("2022-02-24"), y=1.0, size=9, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Russische\nInvasion", x=as.Date("2022-03-07"), y=0.95, color = "#0000cd", size=3.5) +
  # annotate(geom="text", label = "Scheitern der\nallg. Impfpflicht", x=as.Date("2022-04-20"), y=0.95, color = "#6b8e23", size=3.5) +
  # annotate(geom="point", x=as.Date("2022-04-07"), y=1.0, size=10, shape=21, fill="transparent", color="#6b8e23") +
  geom_smooth(aes(x=dateTime, y=ukraine, colour = "Ukraine"), method = "gam", se = F) +
  geom_smooth(aes(x=dateTime, y=covid, colour = "Covid"), method = "gam", se = F) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de"),
               limits = as.Date(c("2022-01-01", "2022-10-31")), expand = c(0,0)) +
  xlab("") +
  scale_y_percent(expand = c(0, 0.1)) +
  ylab("") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic")) +
  ggtitle("PolitikerInnen")

killerissueArrange <- ggarrange(nrow = 2, ncol = 1,
                                legend = "bottom", common.legend = T,
                                politikerMinmax_killerissue, medienMinmax_killerissue,
                                labels = c("1", "2"))
annotate_figure(killerissueArrange, top = text_grob(label = "Killer Issue 'Ukraine' im Themen-Zusammenspiel mit Issue 'Covid'", size = 20, face = "bold"),
                bottom = text_grob(hjust = 1, x = 1, label = "Zeitebene: täglich\nGlättungsfunktion: GAM", size = 8))



ggplot(politikerMinMax) +
  geom_smooth(method = "gam", se = F, aes(x = dateTime, y = covid, colour = "Covid")) +
  geom_smooth(method = "gam", se = F, aes(x = dateTime, y = soziales, colour = "Soziales")) +
  geom_line(aes(dateTime, covid), colour = "#6b8e23aa") +
  geom_line(aes(dateTime, soziales), colour = "#ff0000aa") +
  scale_color_manual(name="", values = topicsDistinctColours)




(appendix17a <- ggplot(politikerMinMax) +
  geom_smooth(method = "gam", se = F, aes(x = dateTime, y = ukraine, colour = "Ukraine")) +
  geom_smooth(method = "gam", se = F, aes(x = dateTime, y = energie, colour = "Energie")) +
  geom_line(aes(dateTime, ukraine), colour = "#0000cdaa") +
  geom_line(aes(dateTime, energie), colour = "#00ff00aa") +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de"),
               limits = as.Date(c("2022-01-01", "2022-10-31")), expand = c(0,0)) +
  xlab("") +
  scale_y_percent(breaks = seq(0, 1, 0.2), expand = c(0, 0.1)) +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 9), legend.position = "bottom",
        legend.title = element_text(face = "bold"), legend.text = element_text(face = "italic")) +
  # ggtitle("Ukraine und Energie: Themenverlauf auf Tagesbasis mit Trajektorien") +
  annotate(geom="point", x=as.Date("2022-02-24"), y=1, size=8, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Russische\nInvasion", x=as.Date("2022-02-12"), y=0.98, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-02-27"), y=0.83, size=8, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Zeitenwende-Rede", x=as.Date("2022-03-14"), y=0.8, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-04-03"), y=0.22, size=8, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Massaker von\nBucha", x=as.Date("2022-04-13"), y=0.26, color = "#0000cd", size=3.5) +
  annotate(geom="point", x=as.Date("2022-09-05"), y=0.64, size=8, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Entlastungs-\npaket 3", x=as.Date("2022-08-26"), y=0.72, color = "#00ff00", size=3.5) +
  annotate(geom="point", x=as.Date("2022-09-29"), y=1, size=8, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Scholz'\nDoppel-Wumms", x=as.Date("2022-09-16"), y=0.97, color = "#00ff00", size=3.5)+
  annotate(geom="point", x=as.Date("2022-10-17"), y=0.84, size=8, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Richtlinienkompetenz\nScholz zu\nAKW", x=as.Date("2022-10-17"), y=0.999, color = "#00ff00", size=3))

(appendix17b <- ggplot(medienMinMax) +
    geom_smooth(method = "gam", se = F, aes(x = dateTime, y = ukraine, colour = "Ukraine")) +
    geom_smooth(method = "gam", se = F, aes(x = dateTime, y = energie, colour = "Energie")) +
    geom_line(aes(dateTime, ukraine), colour = "#0000cdaa") +
    geom_line(aes(dateTime, energie), colour = "#00ff00aa") +
    scale_color_manual(name="", values = topicsDistinctColours) +
    scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de"),
                 limits = as.Date(c("2022-01-01", "2022-10-31")), expand = c(0,0)) +
    xlab("") +
    scale_y_percent(breaks = seq(0, 1, 0.2), expand = c(0, 0.1)) +
    ylab("Anteil % Gesamtvolumen") +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 9), legend.position = "bottom",
          legend.title = element_text(face = "bold"), legend.text = element_text(face = "italic")) +
    # ggtitle("Ukraine und Energie: Themenverlauf auf Tagesbasis mit Trajektorien") +
    annotate(geom="point", x=as.Date("2022-02-24"), y=1, size=8, shape=21, fill="transparent", color="#0000cd") +
    annotate(geom="text", label = "Russische\nInvasion", x=as.Date("2022-02-12"), y=0.98, color = "#0000cd", size=3.5) +
    annotate(geom="point", x=as.Date("2022-02-27"), y=0.83, size=8, shape=21, fill="transparent", color="#0000cd") +
    annotate(geom="text", label = "Zeitenwende-Rede", x=as.Date("2022-03-14"), y=0.88, color = "#0000cd", size=3.5) +
    annotate(geom="point", x=as.Date("2022-04-03"), y=0.38, size=8, shape=21, fill="transparent", color="#0000cd") +
    annotate(geom="text", label = "Massaker von\nBucha", x=as.Date("2022-04-13"), y=0.5, color = "#0000cd", size=3.5) +
    annotate(geom="point", x=as.Date("2022-09-05"), y=0.64, size=8, shape=21, fill="transparent", color="#00ff00") +
    annotate(geom="text", label = "Entlastungs-\npaket 3", x=as.Date("2022-08-30"), y=0.76, color = "#00ff00", size=3.5) +
    annotate(geom="point", x=as.Date("2022-09-29"), y=1, size=8, shape=21, fill="transparent", color="#00ff00") +
    annotate(geom="text", label = "Scholz'\nDoppel-Wumms", x=as.Date("2022-09-16"), y=0.97, color = "#00ff00", size=3.5)+
    annotate(geom="point", x=as.Date("2022-10-17"), y=0.81, size=8, shape=21, fill="transparent", color="#00ff00") +
    annotate(geom="text", label = "Richtlinienkompetenz\nScholz zu\nAKW", x=as.Date("2022-10-17"), y=0.98, color = "#00ff00", size=3))


appendix17 <- ggarrange(nrow = 2, ncol = 1, labels = c("1", "2"),
          appendix17a, appendix17b, common.legend = T, legend = "bottom")
annotate_figure(appendix17, top = text_grob("Ukraine und Energie: Themenverlauf auf Tagesbasis mit Trajektorien,\nPolitikerInnen (1) und Medien (2)", size = 20, face = "bold"))















