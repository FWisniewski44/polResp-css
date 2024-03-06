# block 4
# konzentration auf das verhältnis der sphären zueinander
# wer spricht themen wann an
# gibt es unterschiedliche betonungen von ereignissen zwischen den sphären
# täler vs spitzen: unterschiede?

# überblicksgraphik lollipops:
# vergleich der politiker und der medien hinsichtlich deren themenbehandlung
komplettoGanzBRD <- as_tidytable(data.frame("names"=komplettoPolitiker$names,
                                            "absolutPolitiker"=(komplettoWesten$absolutPolitiker + komplettoOsten$absolutPolitiker),
                                            "absolutMedien"=themenüberblickMedien$anzahl
))
komplettoGanzBRD <- komplettoGanzBRD %>% mutate(proporzPolitiker = absolutPolitiker / sum(absolutPolitiker))
komplettoGanzBRD <- komplettoGanzBRD %>% mutate(proporzMedien = absolutMedien / sum(absolutMedien))
komplettoGanzBRD

ggplot(komplettoGanzBRD) +
  geom_segment(aes(x=proporzPolitiker,
                   xend=proporzMedien,
                   y=reorder(names, absolutPolitiker),
                   yend=reorder(names, absolutPolitiker)),
               colour="grey50") +
  geom_point(aes(x=proporzPolitiker,
                 y=reorder(names, absolutPolitiker),
                 colour = "PolitikerInnen")) +
  geom_point(aes(x=proporzMedien,
                 y=reorder(names, absolutPolitiker),
                 colour = "Medien")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom") +
  scale_x_percent(breaks = seq(0, 0.24, 0.02), limits = c(0, 0.24)) +
  scale_colour_manual(name=" ", values = farbenPolitikerMedien) +
  #coord_flip() +
  ggtitle(label = "Vergleich der Themenaufmerksamkeit der Akteurssphären in Twitter-Deutschland") +
  xlab("Anteil %") +
  ylab(" ")

# =============================================================================================================================================================

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

# =============================================================================================================================================================

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

# =============================================================================================================================================================

# ukraine: gesamtgraphik im vergleich der sphären mit min-max-normalisierter skala
# interpretativ: wo sind die jeweiligen maximalen und minimalen themenwerte in den graphiken, macht vergleichbarkeit besser als prozentskala, da
# dort radikal verschiedene grundwerte wären --- mit min-max ist das erstmal egal, weil man da einfach die werte zwischen 0.0 und 1.0 standardisiert und damit
# auf das thema zentriert, weniger auf den anteil vom insgesamten

ukraineZoom_medien <- medienMinMax %>% filter(dateTime >= as.Date("2022-01-01") & dateTime <= as.Date("2022-02-23"))
ukraineZoom_pol <- politikerMinMax %>% filter(dateTime >= as.Date("2022-01-01") & dateTime <= as.Date("2022-02-23"))

ukraineZoom_medien_kriegsbeg <- medienMinMax %>% filter(monthNumber >= 2 & monthNumber <= 3)
ukraineZoom_pol_kriegsbeg <- politikerMinMax %>% filter(monthNumber >= 2 & monthNumber <= 3)

ukraineZoom_medien_augSep <- medienMinMax %>% filter(monthNumber == 9)
ukraineZoom_pol_augSep <- politikerMinMax %>% filter(monthNumber == 9)

ukrZoom_baerbockreise <- data.frame(xmin = as.Date("2022-01-15"), xmax = as.Date("2022-01-20"), ymin = -Inf, ymax = Inf)

(ukraineZoom_graphik <- ggplot() +
  geom_line(data = ukraineZoom_medien, aes(x=dateTime, y=ukraine, colour = "Medien")) +
  geom_line(data = ukraineZoom_pol, aes(x=dateTime, y=ukraine, colour = "PolitikerInnen")) +
  # geom_smooth(method="gam", data = ukraineZoom_medien, aes(x=dateTime, y=ukraine), colour = "#ffc125aa", se = F) +
  # geom_smooth(method="gam", data = ukraineZoom_pol, aes(x=dateTime, y=ukraine), colour = "#0000cdaa", se = F) +
  geom_vline(xintercept = as.Date("2022-01-17"), colour = "#ff6347aa", linewidth = 1.5) +
  # annotate(geom="text", label = "Zeitenwende-\nRede", x=as.Date("2022-03-02"), y=0.1, color = "#ff0000", size=3.5) +
  # geom_rect(data=ukrZoom_baerbockreise, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
  #           fill="#ff6347aa",
  #           color="black",alpha=0.5,
  #           inherit.aes = FALSE) +
  annotate(geom="text", label = "Reise\nA. Baerbock\nUKR + RUS", x=as.Date("2022-01-21"), y=0.2, color = "#ff6347", size=3.5) +
  scale_color_manual(name="", values = farbenPolitikerMedien) +
  scale_x_date(breaks = "1 week", labels = date_format(format = "%b\nKW %W", locale = "de"),
               limits = as.Date(c("2022-01-01", "2022-02-24")), expand = c(0,0)) +
  xlab("") +
  scale_y_percent(limits = c(0,.42)) +
  ylab(" ") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
        legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
  ggtitle("Zoom: Jan. bis Mitte Feb., Prä-Kriegsphase"))

# ggplotly(ukraineZoom_graphik)
# View(allePolitiker %>% filter(ukraine == 1) %>% filter(dateTime == "2022-02-03"))

(ukraineZoom_graphik_kriegsbeg <- ggplot() +
    geom_line(data = ukraineZoom_medien_kriegsbeg, aes(x=dateTime, y=ukraine, colour = "Medien")) +
    geom_line(data = ukraineZoom_pol_kriegsbeg, aes(x=dateTime, y=ukraine, colour = "PolitikerInnen")) +
    # geom_smooth(method="gam", data = ukraineZoom_medien_kriegsbeg, aes(x=dateTime, y=ukraine), colour = "#ffc125aa", se = F) +
    # geom_smooth(method="gam", data = ukraineZoom_pol_kriegsbeg, aes(x=dateTime, y=ukraine), colour = "#0000cdaa", se = F) +
    geom_vline(xintercept = as.Date("2022-02-27"), colour = "#ff6347") +
    annotate(geom="text", label = "Zeitenwende-Rede", x=as.Date("2022-03-05"), y=0.9, color = "#ff6347", size=3.5) +
    # geom_rect(data=ukrZoom_baerbockreise, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
    #           fill="#ff6347aa",
    #           color="black",alpha=0.5,
    #           inherit.aes = FALSE) +
    # annotate(geom="text", label = "Reise\nA. Baerbock\nUKR + RUS", x=as.Date("2022-01-23"), y=0.7, color = "#ff0000", size=3.5) +
    scale_color_manual(name="", values = farbenPolitikerMedien) +
    scale_x_date(breaks = "1 week", labels = date_format(format = "%b\nKW %W", locale = "de"),
                 limits = as.Date(c("2022-02-01", "2022-03-31")), expand = c(0,0)) +
    xlab("") +
    scale_y_percent() +
    ylab(" ") +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
          legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
    ggtitle("Zoom: Kriegsbeginn"))
# ggplotly(ukraineZoom_graphik)


(ukraine_graphik <- ggplot() +
  geom_line(data = medienMinMax, aes(x=dateTime, y=ukraine, colour = "Medien")) +
  geom_line(data = politikerMinMax, aes(x=dateTime, y=ukraine, colour = "PolitikerInnen")) +
  geom_smooth(method="gam", data = medienMinMax, aes(x=dateTime, y=ukraine), colour = "#ffc125aa", se = F) +
  geom_smooth(method="gam", data = politikerMinMax, aes(x=dateTime, y=ukraine), colour = "#0000cdaa", se = F) +
  # geom_vline(xintercept = as.Date("2022-02-27"), colour = "#ff0000") +
  scale_color_manual(name="", values = farbenPolitikerMedien) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b, %W", locale = "de"),
               limits = as.Date(c("2022-01-01", "2022-10-31")), expand = c(0,0)) +
  xlab("") +
  scale_y_percent() +
  ylab(" ") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
        legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
  ggtitle("Themenaufmerksamkeit im Zeitverlauf"))

augsep <- ggplot() +
  geom_line(data = ukraineZoom_medien_augSep, aes(x=dateTime, y=ukraine, colour = "Medien")) +
  geom_line(data = ukraineZoom_pol_augSep, aes(x=dateTime, y=ukraine, colour = "PolitikerInnen")) +
  # geom_smooth(method="gam", data = ukraineZoom_medien_kriegsbeg, aes(x=dateTime, y=ukraine), colour = "#ffc125aa", se = F) +
  # geom_smooth(method="gam", data = ukraineZoom_pol_kriegsbeg, aes(x=dateTime, y=ukraine), colour = "#0000cdaa", se = F) +
  geom_vline(xintercept = as.Date("2022-09-12"), colour = "#ff6347aa", linewidth = 1.5) +
  annotate(geom="text", label = "Forderung v.\nWaffenlieferungen", x=as.Date("2022-09-09"), y=0.25, color = "#ff6347", size=3.5) +
  geom_vline(xintercept = as.Date("2022-09-21"), colour = "#ff6347aa", linewidth = 1.5) +
  annotate(geom="text", label = "Rus. Teil-\nmobilmachung", x=as.Date("2022-09-24"), y=0.25, color = "#ff6347", size=3.5) +
  # geom_rect(data=ukrZoom_baerbockreise, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
  #           fill="#ff6347aa",
  #           color="black",alpha=0.5,
  #           inherit.aes = FALSE) +
  # annotate(geom="text", label = "Reise\nA. Baerbock\nUKR + RUS", x=as.Date("2022-01-23"), y=0.7, color = "#ff0000", size=3.5) +
  scale_color_manual(name="", values = farbenPolitikerMedien) +
  scale_x_date(breaks = "1 week", labels = date_format(format = "%b\nKW %W", locale = "de"),
               limits = as.Date(c("2022-09-01", "2022-09-30")), expand = c(0,0)) +
  xlab("") +
  scale_y_percent(limits = c(0, .30)) +
  ylab(" ") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
        legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
  ggtitle("Zoom: Sept., moderate Phase")

View(allePolitiker %>% filter(ukraine == 1) %>% filter(dateTime == "2022-09-20"))

block4Ukraine <- ggarrange(nrow = 2, ncol = 1, ukraine_graphik, legend = "none", labels = "1",
                           ggarrange(nrow = 1, ncol = 2, ukraineZoom_graphik, augsep,
                                     labels = c("2", "3"), common.legend = T, legend = "bottom"))
annotate_figure(block4Ukraine,
                top = text_grob("Vergleichende Untersuchung der Akteurssphären für das Thema Ukraine", face = "bold", size = 20),
                bottom = text_grob("Zeitebene: täglich.", size = 8, face = "italic", hjust = 1, x = 1))

# =========================================================================================================================================================

# covid

covidZoom_medien_anfang2022 <- medienMinMax %>% filter(monthNumber >= 1 & monthNumber <= 3)
covidZoom_pol_anfang2022 <- politikerMinMax %>% filter(monthNumber >= 1 & monthNumber <= 3)

covidZoom_medien_impfpflicht <- medienMinMax %>% filter(monthNumber >= 3 & monthNumber <= 4)
covidZoom_pol_impfpflicht <- politikerMinMax %>% filter(monthNumber >= 3 & monthNumber <= 4)

covidMedien_maxima <- data.frame(xmin = as.Date("2022-01-09"), xmax = as.Date("2022-01-16"), ymin = -Inf, ymax = Inf)

(covidZoom_graphik_anfang2022 <- ggplot() +
    geom_line(data = covidZoom_medien_anfang2022, aes(x=dateTime, y=covid, colour = "Medien")) +
    geom_line(data = covidZoom_pol_anfang2022, aes(x=dateTime, y=covid, colour = "PolitikerInnen")) +
    geom_vline(xintercept = as.Date("2022-02-24"), linewidth = 1.5, colour = "#ff6347aa") +
    annotate(geom="text", label = "Russ.\nInvasion", x=as.Date("2022-03-01"), y=0.90, color = "#ff6347", size=3.5) +
    geom_vline(xintercept = as.Date("2022-01-26"), linewidth = 1.5, colour = "#ff6347aa") +
    annotate(geom="text", label = "Orientierungsdeb.\nImpfpflicht", x=as.Date("2022-02-02"), y=0.35, color = "#ff6347", size=3) +
    scale_color_manual(name="Akteursgruppe", values = farbenPolitikerMedien) +
    scale_x_date(breaks = "1 week", labels = date_format(format = "%b\nKW %W", locale = "de"),
                 limits = as.Date(c("2022-01-01", "2022-03-31")), expand = c(0,0)) +
    xlab("") +
    scale_y_percent() +
    ylab(" ") +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
          legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
    ggtitle("Zoom: Anfang 2022, Prä-Kriegsphase"))
# ggplotly(covidZoom_graphik_anfang2022)

(covidZoom_graphik_impfpflicht <- ggplot() +
    geom_line(data = covidZoom_medien_impfpflicht, aes(x=dateTime, y=covid, colour = "Medien")) +
    geom_line(data = covidZoom_pol_impfpflicht, aes(x=dateTime, y=covid, colour = "PolitikerInnen")) +
    geom_vline(xintercept = as.Date("2022-03-17"), linewidth = 1.5, colour = "#ff6347aa") +
    annotate(geom="text", label = "Selenskyj-Rede\n+ Impfpflicht-Debatte", x=as.Date("2022-03-23"), y=0.90, color = "#ff6347", size=3.5) +
    geom_vline(xintercept = as.Date("2022-04-07"), linewidth = 1.5, colour = "#ff6347aa") +
    annotate(geom="text", label = "Abstimmung\nImpfpflicht", x=as.Date("2022-04-12"), y=0.75, color = "#ff6347", size=3.5) +
    scale_color_manual(name="Akteursgruppe", values = farbenPolitikerMedien) +
    scale_x_date(breaks = "1 week", labels = date_format(format = "%b\nKW %W", locale = "de"),
                 limits = as.Date(c("2022-03-01", "2022-04-30")), expand = c(0,0)) +
    xlab("") +
    scale_y_percent() +
    ylab(" ") +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
          legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
    ggtitle("Zoom: April, Abstimmung Impfpflicht"))
#ggplotly(covidZoom_graphik_impfpflicht)

(covid_graphik <- ggplot() +
    geom_line(data = medienMinMax, aes(x=dateTime, y=covid, colour = "Medien")) +
    geom_line(data = politikerMinMax, aes(x=dateTime, y=covid, colour = "PolitikerInnen")) +
    geom_smooth(method="gam", data = medienMinMax, aes(x=dateTime, y=covid), colour = "#ffc125aa", se = F) +
    geom_smooth(method="gam", data = politikerMinMax, aes(x=dateTime, y=covid), colour = "#0000cdaa", se = F) +
    geom_vline(xintercept = as.Date("2022-02-24"), linewidth = 1.5, colour = "#ff6347aa") +
    annotate(geom="text", label = "Russ.\nInvasion", x=as.Date("2022-03-03"), y=0.90, color = "#ff6347", size=3.5) +
    geom_vline(xintercept = as.Date("2022-03-17"), linewidth = 1.5, colour = "#ff6347aa") +
    annotate(geom="text", label = "Selenskyj-Rede\n+ Impfpflicht-\nDebatte", x=as.Date("2022-03-27"), y=0.85, color = "#ff6347", size=3.5) +
    geom_rect(data = covidMedien_maxima,
              aes(xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin),
              fill = "#ff6347aa", colour = "black", alpha = 0.5, inherit.aes = F) +
    annotate(geom="text", label = "Proteste,\nImpfpflicht,\nOmikron", x=as.Date("2022-01-21"), y=0.4, color = "#ff6347", size=3) +
    scale_color_manual(name="Akteursgruppe", values = farbenPolitikerMedien) +
    scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de"),
                 limits = as.Date(c("2022-01-01", "2022-11-31")), expand = c(0,0)) +
    xlab("") +
    scale_y_percent() +
    ylab("") +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
          legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
  ggtitle("Themenaufmerksamkeit im Zeitverlauf"))

block4Covid <- ggarrange(nrow = 2, ncol = 1, covid_graphik, legend = "none", labels = "1", ggarrange(nrow = 1, ncol = 2, covidZoom_graphik_anfang2022, covidZoom_graphik_impfpflicht, labels = c("2", "3"), common.legend = T, legend = "bottom"))
annotate_figure(block4Covid,
                top = text_grob("Vergleichende Untersuchung der Akteurssphären für das Thema Covid", size = 20, face = "bold"),
                bottom = text_grob("Zeitebene: täglich.", size = 8, face = "italic", hjust = 1, x = 1))

View(alleMedien %>% filter(covid ==1) %>% filter(dateTime == "2022-02-01"))

# =========================================================================================================================================================

medienVerk_49euro <- medienMinMax %>% filter(monthNumber >= 3 & monthNumber <= 5)
politikerVerk_49euro <- politikerMinMax %>% filter(monthNumber >= 3 & monthNumber <= 5)


(ggplot() +
  geom_line(data = medienMinMax, aes(x=dateTime, y=verteidigungspolitik, colour = "Medien")) +
  geom_line(data = politikerMinMax, aes(x=dateTime, y=verteidigungspolitik, colour = "PolitikerInnen")) +
  geom_smooth(method="loess", data = medienMinMax, aes(x=dateTime, y=verteidigungspolitik), colour = "#ffc125aa", se = F) +
  geom_smooth(method="loess", data = politikerMinMax, aes(x=dateTime, y=verteidigungspolitik), colour = "#0000cdaa", se = F) +
  # geom_vline(xintercept = as.Date("2022-03-24")) +
  # geom_vline(xintercept = as.Date("2022-05-20")) +
  scale_color_manual(name="Akteursgruppe", values = farbenPolitikerMedien) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de")) +
  xlab("") +
  scale_y_percent() +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
        legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
  ggtitle("Themenaufmerksamkeit im Zeitverlauf"))
# View(alleMedien %>% filter(verkehr == 1) %>% filter(dateTime == "2022-06-08"))

# =========================================================================================================================================================

ccf(alleMedien$covid, allePolitiker$covid, plot = T, type = "correlation")
ccf(timePol_topics$ukraine, timePol_topics$energie, plot = T, type = "correlation")
ccf(timeMedia_topics$ukraine, timeMedia_topics$covid, plot = T, type = "correlation")


















ggplot() +
  geom_line(data = medienMinMax, aes(x=dateTime, y=ukraine, colour = "Medien")) +
  geom_line(data = politikerMinMax, aes(x=dateTime, y=ukraine, colour = "PolitikerInnen")) +
  geom_smooth(method="gam", data = medienMinMax, aes(x=dateTime, y=ukraine), colour = "#ffc125aa", se = F) +
  geom_smooth(method="gam", data = politikerMinMax, aes(x=dateTime, y=ukraine), colour = "#0000cdaa", se = F) +
  geom_vline(xintercept = as.Date("2022-09-12"), colour = "#ff0000") +
  scale_color_manual(name="", values = farbenPolitikerMedien) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b, %W", locale = "de"),
               limits = as.Date(c("2022-01-01", "2022-10-31")), expand = c(0,0)) +
  xlab("") +
  scale_y_percent() +
  ylab(" ") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5), legend.position = "bottom",
        legend.text = element_text(size = 9, face = "italic"), legend.title = element_text(face = "bold")) +
  ggtitle("Themenaufmerksamkeit im Zeitverlauf")







