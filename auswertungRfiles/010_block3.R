# block 3
# zentrierung auf die akteure

# zeitergänzungen
timePol_maximus$weekAsDate <- as.Date(cut(timePol_maximus$dateTime, breaks = "week"))
timePol_maximus$weeknumber <- lubridate::week(timePol_maximus$dateTime)
timePol_maximus$monthAsDate <- as.Date(cut(timePol_maximus$dateTime, breaks = "month"))
timePol_maximus$monthNumber <- lubridate::month(timePol_maximus$dateTime)

timeMedia_maximus$weekAsDate <- as.Date(cut(timeMedia_maximus$dateTime, breaks = "week"))
timeMedia_maximus$weeknumber <- lubridate::week(timeMedia_maximus$dateTime)
timeMedia_maximus$monthAsDate <- as.Date(cut(timeMedia_maximus$dateTime, breaks = "month"))
timeMedia_maximus$monthNumber <- lubridate::month(timeMedia_maximus$dateTime)

timeMedia_discPow_days$weekAsDate <- as.Date(cut(timeMedia_discPow_days$dateTime, breaks = "week"))
timeMedia_discPow_days$weeknumber <- lubridate::week(timeMedia_discPow_days$dateTime)
timeMedia_discPow_days$monthAsDate <- as.Date(cut(timeMedia_discPow_days$dateTime, breaks = "month"))
timeMedia_discPow_days$monthNumber <- lubridate::month(timeMedia_discPow_days$dateTime)

# ==============================================================================================================================================================
# DASHBOARD ZU AKTEUREN UND THEMEN | POLITIKER: überblick über thema covid-19 bei politikern -- kann F3 adressieren; u. U. F4, wenn selektion vorgenommen wird
# graphik a => area chart, behandlung des themas über den zeitraum hinweg, geschichtet nach parteizugehörigkeit der jeweiligen politiker
# graphik b => line chart, ebenfalls zeitraum abgedeckt, jedoch mit anderer lesbarkeit; könnte debattiert werden, ob der ausgetauscht werden soll: entweder a oder b
# graphik c => bar chart horizontal: aktivste einzel-politiker zum thema

## idee zum ersatz von entweder a oder b: lollipop chart zum vergleich ost vs. west, dann hat man gleich noch frage
# ========================================================================================================================================================

# followerzahlen politiker
mostfollowedPol <- as_tidytable(data.frame("user"=allePolitiker$user,
                                           "followerAmount"=allePolitiker$followerAmount,
                                           "bundesland"=allePolitiker$bundesland,
                                           "partei"=allePolitiker$partei))
mostfollowedPol <- mostfollowedPol %>% group_by(user) %>% distinct(.keep_all = T, user)
mostfollowedPol <- mostfollowedPol %>% arrange(desc(followerAmount))

# politiker meiste follower
mostfollowsPOL <- mostfollowedPol %>%
  head(20) %>%
  ggplot() +
  geom_bar(aes(fill=as.factor(partei), x=followerAmount, y=reorder(user, followerAmount)), stat="identity") +
  scale_fill_manual("Parteizugehörigkeit", values = parteifarben) +
  ggtitle("Konten mit meisten FollowerInnen") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = c(0,1100000), breaks = seq(0,1100000, 200000), labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 10), legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold"), axis.text.y = element_text(size = 9.5),
        title = element_text(size = 14))

# werte hiervon wurden mit visidata-funktion gegengecheckt und stimmen; tweets aller politiker OHNE nur klassifizierte zu nehmen
# einige hatten offensichtlich einen fehler beim scraping mit falschen zuordnungen, der aber nicht durch mich verursacht ist sondern höchstwahrscheinlich durchs twitter interface. Evtl. handelt es sich dabei auch immer mal wieder um zuvor bereits gelöschte tweets, die dann zwar in textform abgegriffen wurden aber dann nicht mehr mit nutzerinfos versehen werden können
write_csv(allePolitiker, file = "../../../allePolitiker_checkup.csv")

polTweets_meiste <- allePolitiker %>% group_by(user) %>%
  count(user) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  as_tidytable()

polTweetsVoll <- left_join(x = polTweets_meiste, y = timePol_userParty)

mostactivePOL <- polTweetsVoll %>%
  # arrange(desc(n)) %>%
  # head(20) %>%
  ggplot() +
  geom_bar(aes(y=reorder(user, n), x = n, fill = partei), stat = "identity") +
  scale_fill_manual(name="Parteizugehörigkeit", values = parteifarben) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 10), legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold"), axis.text.y = element_text(size = 9.5),
        title = element_text(size = 14)) +
  scale_x_continuous(limits = c(0,4000), breaks = seq(0,4000, 500), labels = scales::label_number(big.mark = ".")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Aktivste Konten")

mostfollowMostactivePOL <- ggarrange(nrow = 1, ncol = 2, mostactivePOL, mostfollowsPOL, common.legend = T, legend = "bottom", labels = c("1", "2"))
annotate_figure(mostfollowMostactivePOL,
                top = text_grob("Politische Akteure und deren Reichweiten im Überblick", face = "bold", size = 20),
                bottom = text_grob("Hinweis: Anzahl abgebildeter Akteure je Graphik wurde auf 20 festgelegt.", size = 8, x = 1, hjust = 1))

# anders herstellen: problem = keine zähloption
timePol_maximus <- timePol_maximus %>% mutate(regop = case_when(partei == "AfD" ~ "Opposition",
                                                                partei == "B90/Die Grünen" ~ "Regierung",
                                                                partei == "CDU" ~ "Opposition",
                                                                partei == "CSU" ~ "Opposition",
                                                                partei == "Die Linke" ~ "Opposition",
                                                                partei == "FDP" ~ "Regierung",
                                                                partei == "SPD" ~ "Regierung",
                                                                partei == "SSW" ~ "Opposition"))

# erstmal daten importieren
regierungsliste <- c("SPD", "FDP", "B90/Die Grünen")

# regierungsparteien
timePol_maximus_spd <- timePol_maximus %>% filter(partei == "SPD")
timePol_maximus_fdp <- timePol_maximus %>% filter(partei == "FDP")
timePol_maximus_grüne <- timePol_maximus %>% filter(partei == "B90/Die Grünen")

# oppositionsparteien
timePol_maximus_cdu <- timePol_maximus %>% filter(partei == "CDU")
timePol_maximus_csu <- timePol_maximus %>% filter(partei == "CSU")
timePol_maximus_afd <- timePol_maximus %>% filter(partei == "AfD")
timePol_maximus_linke <- timePol_maximus %>% filter(partei == "Die Linke")
timePol_maximus_ssw <- timePol_maximus %>% filter(partei == "SSW")

timePol_maximus_regierung <- timePol_maximus %>% filter(partei %in% regierungsliste)
timePol_maximus_opposition <- timePol_maximus %>% filter(partei %ni% regierungsliste)
sum(timePol_maximus_regierung$ukraine) / (sum(timePol_maximus_regierung$ukraine) + sum(timePol_maximus_opposition$ukraine))

# # anders
# timePol_reg <- timePol_maximus %>% filter(regop == "Regierung")
# timePol_op <- timePol_maximus %>% filter(regop == "Opposition")
regop <- full_join(timePol_maximus_regierung, timePol_maximus_opposition)

# ==============================================================================================================================================================
# covid thema
# ==============================================================================================================================================================

activeCovidUsers_politiker <- timePol_maximus %>% group_by(user) %>% filter(sum(covid) >= 100)

# regierungsparteien
activeCovidUsers_spd <- timePol_maximus_spd %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_grüne <- timePol_maximus_grüne %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_fdp <- timePol_maximus_fdp %>% group_by(user) %>% filter(sum(covid) >= 100)

# opposition
activeCovidUsers_cdu <- timePol_maximus_cdu %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_csu <- timePol_maximus_csu %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_afd <- timePol_maximus_afd %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_linke <- timePol_maximus_linke %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_ssw <- timePol_maximus_ssw %>% group_by(user) %>% filter(sum(covid) >= 100)

activeCovidUsers_reg <- timePol_maximus_regierung %>% group_by(user) %>% filter(sum(covid) >= 100)
activeCovidUsers_op <- timePol_maximus_opposition %>% group_by(user) %>% filter(sum(covid) >= 100)

# unique(activeCovidUsers$user)
# unique(activeCovidUsers_afd$user)
# unique(activeCovidUsers_spd$user)

###################################
highlightCOV_abstimmung <- data.frame(xmin = as.Date("2022-04-01"), xmax = as.Date("2022-04-11"), ymin = -Inf, ymax = Inf)


# area plot mit fill property
(areaplotCOV <- ggplot() +
  stat_summary(data = timePol_maximus, aes(weekAsDate, covid, fill=partei), color="white", position = "fill", geom = "area", fun = sum, na.rm = T) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  scale_y_percent() +
  #geom_vline(xintercept = as.Date("2022-04-04"), colour = "#ff6347aa", linewidth = 1.5) +
  geom_rect(data=highlightCOV_abstimmung, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="tomato3",
            fill="transparent",
            alpha=0.5,
            inherit.aes = FALSE) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5), legend.text = element_text(size = 9), legend.position = "right") +
  ggtitle(label = "Wöchentlicher Verlauf: Anteil an Tweets nach Parteien") +
  scale_fill_manual(name=" ", values = parteifarben) +
  xlab(" ") +
  ylab(" "))

# aktivste tweeter für regierung vs opposition als linechart
farbenRegop <- c("Regierung"="#b40040", "Opposition"="#10adf0")
# alternative farbe opposition: #40a241

(zeitreiheParteienCOV <- ggplot() +
  stat_summary(data = timePol_maximus_regierung, aes(weekAsDate, covid, colour="Regierung"), geom = "line", fun = sum) +
  stat_summary(data = timePol_maximus_opposition, aes(weekAsDate, covid, colour="Opposition"), geom = "line", fun = sum) +
  #geom_vline(xintercept = as.Date("2022-04-04"), colour = "#ff6347aa", linewidth = 1.5) +
  geom_rect(data=highlightCOV_abstimmung, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="tomato3",
            fill="transparent",
            alpha=0.5,
            inherit.aes = FALSE) +
  scale_colour_manual(name="", values = farbenRegop) +
  # stat_summary(data = activeCovidUsers_spd, aes(weekAsDate, covid), color="#e40006", geom = "line", fun = sum) + #effektiv karl lauterbach alleine
  # stat_summary(data = activeCovidUsers_fdp, aes(weekAsDate, covid), color="#ffee00", geom = "line", fun = sum) +
  # stat_summary(data = activeCovidUsers_grüne, aes(weekAsDate, covid), color="#19a329", geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Zeitreihen: Regierung und Opposition") +
  # scale_fill_manual(name="PolitikerInnen", values = sechzehnFarben) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9), legend.box.spacing = unit(0.2, "cm")) +
  xlab("") +
  ylab(""))


# graphik für politiker mit mind. 100 tweets zum thema COVID über untersuchungszeitraum
(politiker_aktivsteCOV <- timePol_userParty %>%
  filter(covid >= 100) %>%
  # arrange(desc(covid)) %>%
  # head(15) %>%
  mutate(user = fct_reorder(user, desc(-covid))) %>%
  ggplot(aes(y=user, x=covid, fill=partei)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name="Parteizugehörigkeit", values = parteifarben) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position = "none") +
  xlab(" ") +
  ylab(" ") +
  ggtitle(label = "Aktivste, einzelne PolitikerInnen auf Twitter"))

# (patchwork_politikerCOV <- areaplotCOV / (zeitreiheParteienCOV + politiker_aktivsteCOV))
# patchwork_politikerCOV + plot_annotation(tag_levels = '1',
#                                          title = "Überblicksgraphik zum Thema Covid",
#                                          caption = "Hinweis: Zeitreihendaten sind auf wöchentlicher Basis zu interpretieren.") &
#   theme(plot.tag = element_text(family = "TeX Gyre Heros", face = "plain", size = 12),
#         title = element_text(family = "TeX Gyre Heros", size = 18, face = "bold", hjust = 0.5),
#         plot.caption = element_text(family = "TeX Gyre Heros", size = 9, face = "plain"))

arrangement_politiker_covid <- ggarrange(areaplotCOV,
                                         ggarrange(zeitreiheParteienCOV, politiker_aktivsteCOV,
                                                             ncol = 2,
                                                             labels = c("2", "3")),
                                         nrow = 2, labels = "1")
annotate_figure(arrangement_politiker_covid,
                bottom=text_grob("Hinweis: Zeitreihendaten sind auf wöchentlicher Basis zu interpretieren.\nRote Markierung in 1 und 2: Abstimmung zur Impfpflicht.",
                                 face = "italic",
                                 size = 8,
                                 color = "black"),
                top = text_grob("Überblicksgraphik zum Thema Covid",
                                face = "bold",
                                size = 20,
                                color = "black"))

# appendix 9
activeCovidUsers_politiker %>%
  # arrange(desc(covid)) %>%
  # head(15) %>%
  mutate(user = fct_reorder(user, desc(-covid))) %>%
  ggplot() +
  stat_summary(fun = "sum", geom = "area", position = "fill", colour = "white", aes(x = weekAsDate, y = covid, fill = partei)) +
  scale_y_percent() +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W"), expand = c(0,5))+
  scale_fill_manual(name="Parteizugehörigkeit", values = parteifarben) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9), legend.position = "bottom", legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle(label = "Anteile an Tweets zum Thema Covid der aktivsten Akteure",
          subtitle = "Zeitebene: wöchentlich | Akteure mit min. 100 Tweets zum Thema")

# ========================================================================================================================================================
# ukraine thema
# ========================================================================================================================================================

activeUkraineUsers <- timePol_maximus %>% group_by(user) %>% filter(sum(ukraine) >= 100)
activeUkraineUsers_afd <- timePol_maximus_afd %>% group_by(user) %>% filter(sum(ukraine) >= 100)
activeUkraineUsers_spd <- timePol_maximus_spd %>% group_by(user) %>% filter(sum(ukraine) >= 100)
activeUkraineUsers_fdp <- timePol_maximus_fdp %>% group_by(user) %>% filter(sum(ukraine) >= 100)
activeUkraineUsers_grüne <- timePol_maximus_grüne %>% group_by(user) %>% filter(sum(ukraine) >= 100)
activeUkraineUsers_cdu <- timePol_maximus_cdu %>% group_by(user) %>% filter(sum(ukraine) >= 100)
activeUkraineUsers_csu <- timePol_maximus_csu %>% group_by(user) %>% filter(sum(ukraine) >= 100)
activeUkraineUsers_linke <- timePol_maximus_linke %>% group_by(user) %>% filter(sum(ukraine) >= 100)
activeUkraineUsers_ssw <- timePol_maximus_ssw %>% group_by(user) %>% filter(sum(ukraine) >= 100)


highlightUKR_invasion <- data.frame(xmin = as.Date("2022-02-14"), xmax = as.Date("2022-03-05"), ymin = -Inf, ymax = Inf)

# area plot mit fill property
(areaplotUKR <- ggplot() +
    stat_summary(data = timePol_maximus, aes(weekAsDate, ukraine, fill=partei), color="white", position = "fill", geom = "area", fun = sum, na.rm = T) +
    scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
    scale_y_percent() +
    #geom_vline(xintercept = as.Date("2022-02-21"), colour = "#ff6347aa", linewidth = 1.5) +
    geom_rect(data=highlightUKR_invasion, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color="tomato3",
              fill="transparent",
              alpha=0.5,
              inherit.aes = FALSE) +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.x = element_text(size = 9.5), legend.text = element_text(size = 9), legend.position = "right") +
    ggtitle(label = "Wöchentlicher Verlauf: Anteil an Tweets nach Parteien") +
    scale_fill_manual(name=" ", values = parteifarben) +
    xlab(" ") +
    ylab(" "))

# aktivste tweeter für bundesregierung als linechart
(zeitreiheParteienUKR <- ggplot() +
    stat_summary(data = timePol_maximus_regierung, aes(weekAsDate, ukraine, colour="Regierung"), geom = "line", fun = sum) +
    stat_summary(data = timePol_maximus_opposition, aes(weekAsDate, ukraine, colour="Opposition"), geom = "line", fun = sum) +
    #geom_vline(xintercept = as.Date("2022-02-21"), colour = "#ff6347aa", linewidth = 1.5) +
    geom_rect(data=highlightUKR_invasion, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color="tomato3",
              fill="transparent",
              alpha=0.5,
              inherit.aes = FALSE) +
    scale_colour_manual(name="", values = farbenRegop) +
    # stat_summary(data = activeCovidUsers_spd, aes(weekAsDate, covid), color="#e40006", geom = "line", fun = sum) + #effektiv karl lauterbach alleine
    # stat_summary(data = activeCovidUsers_fdp, aes(weekAsDate, covid), color="#ffee00", geom = "line", fun = sum) +
    # stat_summary(data = activeCovidUsers_grüne, aes(weekAsDate, covid), color="#19a329", geom = "line", fun = sum) +
    scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    ggtitle(label = "Zeitreihen: Regierung und Opposition") +
    # scale_fill_manual(name="PolitikerInnen", values = sechzehnFarben) +
    theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9), legend.box.spacing = unit(0.2, "cm")) +
    xlab("") +
    ylab(""))

# graphik für politiker mit mind. 80 tweets zum thema ukraine über untersuchungszeitraum
(politiker_aktivsteUKR <- timePol_userParty %>%
    filter(ukraine >= 100) %>%
    arrange(desc(ukraine)) %>%
    head(15) %>%
    mutate(user = fct_reorder(user, desc(-ukraine))) %>%
    ggplot(aes(y=user, x=ukraine, fill=partei)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_manual(name="", values = parteifarben) +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position = "none") +
    xlab(" ") +
    ylab(" ") +
    ggtitle(label = "Aktivste, einzelne PolitikerInnen auf Twitter"))

(arrangement_politiker_ukraine <- ggarrange(areaplotUKR,
                                           ggarrange(zeitreiheParteienUKR, politiker_aktivsteUKR,
                                                     ncol = 2,
                                                     labels = c("2", "3")),
                                           nrow = 2, labels = "1"))
annotate_figure(arrangement_politiker_ukraine,
                bottom=text_grob("Hinweis: Zeitreihendaten sind auf wöchentlicher Basis zu interpretieren.\nRote Markierung in 1 und 2: Zeitraum d. russischen Invasion.",
                                 face = "italic",
                                 size = 8,
                                 color = "black"),
                top = text_grob("Überblicksgraphik zum Thema Ukraine",
                                face = "bold",
                                size = 20,
                                color = "black"))

# ========================================================================================================================================================
# DASHBOARD ZU AKTEUREN UND THEMEN | MEDIEN:
# ========================================================================================================================================================

# datenherstellung: umbenennung der factors mit begriffen für etwaige legenden
timeMedia_discursivePower$geschäftsmodell <- factor(timeMedia_discursivePower$geschäftsmodell,
                                                    labels = c("Öffentlich", "Kommerziell", "Spenden"))
timeMedia_discursivePower$erreichbarkeit <- factor(timeMedia_discursivePower$erreichbarkeit,
                                                   labels = c("Etabliert (überregional)", "Etabliert (regional)", "Alternativ", "Individuell"))
timeMedia_discursivePower$normenWerte <- factor(timeMedia_discursivePower$normenWerte,
                                                labels = c("Objektiv-ausbalanciert", "Parteiisch", "Marktgetrieben"))

# active covid users für discursive power medien
activeCovidUsers_medien_mitDisc <- as_tidytable(timeMedia_discursivePower %>% filter(covid >= 500) %>% arrange(desc(covid)))
activeCovidUsers_medien_mitDisc$erreichbarkeit <- as.factor(activeCovidUsers_medien_mitDisc$erreichbarkeit)
activeCovidUsers_medien_mitDisc$normenWerte <- as.factor(activeCovidUsers_medien_mitDisc$normenWerte)
activeCovidUsers_medien_mitDisc$geschäftsmodell <- as.factor(activeCovidUsers_medien_mitDisc$geschäftsmodell)

activeCovidUsers_medien_ohneDisc <- as_tidytable(timeMedia_discPow_days %>% group_by(user) %>% filter(sum(covid) >= 500) %>% arrange(desc(covid)))

# datenherstellung für discursive power datensatz, INKLUSIVE dateTime
timeMedia_discPow_days$geschäftsmodell <- factor(timeMedia_discPow_days$geschäftsmodell,
                                                 labels = c("Öffentlich", "Kommerziell", "Spenden"))
timeMedia_discPow_days$erreichbarkeit <- factor(timeMedia_discPow_days$erreichbarkeit,
                                                labels = c("Etabliert (überregional)", "Etabliert (regional)", "Alternativ", "Individuell"))
timeMedia_discPow_days$normenWerte <- factor(timeMedia_discPow_days$normenWerte,
                                             labels = c("Objektiv-ausbalanciert", "Parteiisch", "Marktgetrieben"))

timeMedia_discPow_days_öffentlich <- timeMedia_discPow_days %>% filter(geschäftsmodell == "Öffentlich")
timeMedia_discPow_days_kommerz <- timeMedia_discPow_days %>% filter(geschäftsmodell == "Kommerziell")
timeMedia_discPow_days_spenden <- timeMedia_discPow_days %>% filter(geschäftsmodell == "Spenden")

timeMedia_discPow_days_etabUereg <- timeMedia_discPow_days %>% filter(erreichbarkeit == "Etabliert (überregional)")
timeMedia_discPow_days_etabReg <- timeMedia_discPow_days %>% filter(erreichbarkeit == "Etabliert (regional)")
timeMedia_discPow_days_alternativ <- timeMedia_discPow_days %>% filter(erreichbarkeit == "Alternativ")
timeMedia_discPow_days_individ <- timeMedia_discPow_days %>% filter(erreichbarkeit == "Individuell")

timeMedia_discPow_days_objektiv <- timeMedia_discPow_days %>% filter(normenWerte == "Objektiv-ausbalanciert")
timeMedia_discPow_days_parteiisch <- timeMedia_discPow_days %>% filter(normenWerte == "Parteiisch")
timeMedia_discPow_days_marktgetr <- timeMedia_discPow_days %>% filter(normenWerte == "Marktgetrieben")

# ## das dann minmax normalisieren
# discpow_öffentlich_minMax <- as_tidytable(apply(timeMedia_discPow_days_öffentlich[,-c(1:6, 22:25)],  MARGIN = 2, FUN = minMaxNorm))
# discpow_öffentlich_minMax$dateTime <- timeMedia_discPow_days_öffentlich$dateTime
# discpow_öffentlich_minMax$weekAsDate <- timeMedia_discPow_days_öffentlich$weekAsDate
# discpow_öffentlich_minMax$weeknumber <- timeMedia_discPow_days_öffentlich$weeknumber
# discpow_öffentlich_minMax$monthAsDate <- timeMedia_discPow_days_öffentlich$monthAsDate
# discpow_öffentlich_minMax$monthNumber <- timeMedia_discPow_days_öffentlich$monthNumber
#
# discpow_kommerz_minMax <- as_tidytable(apply(timeMedia_discPow_days_kommerz[,-c(1:6, 22:25)],  MARGIN = 2, FUN = minMaxNorm))
# discpow_kommerz_minMax$dateTime <- timeMedia_discPow_days_kommerz$dateTime
# discpow_kommerz_minMax$weekAsDate <- timeMedia_discPow_days_kommerz$weekAsDate
# discpow_kommerz_minMax$weeknumber <- timeMedia_discPow_days_kommerz$weeknumber
# discpow_kommerz_minMax$monthAsDate <- timeMedia_discPow_days_kommerz$monthAsDate
# discpow_kommerz_minMax$monthNumber <- timeMedia_discPow_days_kommerz$monthNumber
#
# discpow_spenden_minMax <- as_tidytable(apply(timeMedia_discPow_days_spenden[,-c(1:6, 22:25)],  MARGIN = 2, FUN = minMaxNorm))
# discpow_spenden_minMax$dateTime <- timeMedia_discPow_days_spenden$dateTime
# discpow_spenden_minMax$weekAsDate <- timeMedia_discPow_days_spenden$weekAsDate
# discpow_spenden_minMax$weeknumber <- timeMedia_discPow_days_spenden$weeknumber
# discpow_spenden_minMax$monthAsDate <- timeMedia_discPow_days_spenden$monthAsDate
# discpow_spenden_minMax$monthNumber <- timeMedia_discPow_days_spenden$monthNumber

# identisch für discpow, akteursebene
discPowAkteure_öffentlich <- timeMedia_discursivePower %>% filter(geschäftsmodell == "Öffentlich")
discPowAkteure_kommerziell <- timeMedia_discursivePower %>% filter(geschäftsmodell == "Kommerziell")
discPowAkteure_spenden <- timeMedia_discursivePower %>% filter(geschäftsmodell == "Spenden")

discPowAkteure_etabUreg <- timeMedia_discursivePower %>% filter(erreichbarkeit == "Etabliert (überregional)")
discPowAkteure_etabReg <- timeMedia_discursivePower %>% filter(erreichbarkeit == "Etabliert (regional)")
discPowAkteure_alternativ <- timeMedia_discursivePower %>% filter(erreichbarkeit == "Alternativ")
discPowAkteure_individ <- timeMedia_discursivePower %>% filter(erreichbarkeit == "Individuell")

discPowAkteure_objektiv <- timeMedia_discursivePower %>% filter(normenWerte == "Objektiv-ausbalanciert")
discPowAkteure_parteiisch <- timeMedia_discursivePower %>% filter(normenWerte == "Parteiisch")
discPowAkteure_marktgetr <- timeMedia_discursivePower %>% filter(normenWerte == "Marktgetrieben")

# ==============================================================================================================================================================

# evtl. appendix eine von diesen graphiken bringen
activeCovidUsers_medien_mitDisc %>%
  # filter(geschäftsmodell == "Spenden") %>%
  filter(erreichbarkeit == "Etabliert (überregional)") %>%
  # filter(normenWerte == "Parteiisch") %>%
  mutate(user = fct_reorder(user, desc(-covid))) %>%
  head(15) %>% #geht nur, wenn arrange vorher gemacht wurde, sonst x-beliebige beobachtungen!!!
  ggplot() +
  geom_bar(aes(y=user, x = covid, fill = normenWerte), stat = "identity", linewidth = 1.5, position = "dodge") +
  #scale_fill_manual(name="Bundesland", values = optimum) +
  scale_fill_manual(name = "Erreichbarkeit", values = coloursNormenwerte) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  scale_x_continuous(limits = c(0,3750), breaks = seq(0,3750,500), labels = scales::label_number(big.mark = ".")) +
  xlab("") + ylab("") +
  ggtitle("Aktivste mediale Akteure vom Typus 'Etablierter Akteur (Regional)' zum Thema Covid")

# ========================================================================================================================================================
# UKRAINE als thema
# facetten erreichbarkeit
ggplot() +
  stat_summary(data = timeMedia_discPow_days, aes(dateTime, ukraine, colour=erreichbarkeit), geom = "line", fun = sum) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1) +
  facet_wrap2(.~erreichbarkeit, nrow = 4, ncol = 1, scales = "free_y") +
  scale_colour_manual(name="Erreichbarkeit", values = coloursErreichbarkeit) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5),
      legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
      legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(size = 9, face = "bold"),
      plot.caption = element_text(size = 8)) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Ukraine",
          subtitle = "Sortierung: Erreichbarkeit nach der 'discursive power'-Theorie") +
  xlab("") + ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nZeitangaben auf Tagesbasis.")

# facetten normenWerte
ggplot() +
  stat_summary(data = timeMedia_discPow_days, aes(dateTime, ukraine, colour=normenWerte), geom = "line", fun = sum) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1) +
  facet_wrap2(.~normenWerte, nrow = 4, ncol = 1, scales = "free_y") +
  scale_colour_manual(name="Normen & Werte", values = coloursNormenwerte) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5),
        legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(size = 9, face = "bold"), legend.spacing = unit(1.0, "cm"),
        plot.caption = element_text(size = 8)) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Ukraine",
          subtitle = "Sortierung: Normen u. Werten nach der 'discursive power'-Theorie") +
  xlab("") + ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nZeitangaben auf Tagesbasis.")

# facetten geschäftsmodell
ggplot() +
  stat_summary(data = timeMedia_discPow_days, aes(dateTime, ukraine, colour=geschäftsmodell), geom = "line", fun = sum) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1) +
  facet_wrap2(.~geschäftsmodell, nrow = 3, ncol = 1, scales = "free_y") +
  scale_colour_manual(name="Geschäftsmodell", values = coloursGeschäftsmodell) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5),
        legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(size = 9, face = "bold"),
        plot.caption = element_text(size = 8)) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Ukraine",
          subtitle = "Sortierung nach Geschäftsmodell in der 'discursive power'-Theorie") +
  xlab("") + ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion. Zeitangaben auf Tagesbasis.")

# facetten erreichbarkeit mit farblicher absetzung normen und werte
ggplot() +
  stat_summary(data = timeMedia_discPow_days, aes(dateTime, ukraine, colour=normenWerte), geom = "line", fun = sum) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#ff6347aa", linewidth = 1) +
  facet_wrap2(.~erreichbarkeit, nrow = 4, ncol = 1, scales = "free_y") +
  scale_colour_manual(name="Erreichbarkeit", values = coloursNormenwerte) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5),
        legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(size = 9, face = "bold")) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Ukraine",
          subtitle = "Sortierung nach Erreichbarkeit in der 'discursive power'-Theorie") +
  xlab("") + ylab("")

# visualisierung zeitreihen nach erreichbarkeit
ggplot() +
  stat_summary(data = timeMedia_discPow_days_etabUereg, aes(weekAsDate, ukraine, colour="Etabliert (überregional)"), geom = "line", fun = sum) +
  stat_summary(data = timeMedia_discPow_days_etabReg, aes(weekAsDate, ukraine, colour="Etabliert (regional)"), geom = "line", fun = sum) +
  stat_summary(data = timeMedia_discPow_days_alternativ, aes(weekAsDate, ukraine, colour="Alternativ"), geom = "line", fun = sum) +
  stat_summary(data = timeMedia_discPow_days_individ, aes(weekAsDate, ukraine, colour="Individuell"), geom = "line", fun = sum) +
  # geom_vline(xintercept = as.Date("2022-02-21"), colour = "#ff6347aa", linewidth = 1.5) +
  scale_colour_manual(name="Erreichbarkeit", values = coloursErreichbarkeit) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9), legend.box.spacing = unit(0.2, "cm")) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Ukraine, sortiert nach Erreichbarkeit") +
  xlab("") + ylab("")

# visualisierung zeitreihen nach geschäftsmodell
ggplot() +
  stat_summary(data = timeMedia_discPow_days_öffentlich, aes(weekAsDate, ukraine, colour="Öffentlich"), geom = "line", fun = sum) +
  stat_summary(data = timeMedia_discPow_days_kommerz, aes(weekAsDate, ukraine, colour="Kommerziell"), geom = "line", fun = sum) +
  stat_summary(data = timeMedia_discPow_days_spenden, aes(weekAsDate, ukraine, colour="Spenden"), geom = "line", fun = sum) +
  # geom_vline(xintercept = as.Date("2022-02-21"), colour = "#ff6347aa", linewidth = 1.5) +
  scale_colour_manual(name="Finanzierungsmodell", values = coloursGeschäftsmodell) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9), legend.box.spacing = unit(0.2, "cm")) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Ukraine, sortiert nach Geschäftsmodell") +
  xlab("") + ylab("")

# visualisierung zeitreihen nach geschäftsmodell
ggplot() +
  stat_summary(data = timeMedia_discPow_days_objektiv, aes(weekAsDate, ukraine, colour="Objektiv-ausbalanciert"), geom = "line", fun = sum) +
  stat_summary(data = timeMedia_discPow_days_parteiisch, aes(weekAsDate, ukraine, colour="Parteiisch"), geom = "line", fun = sum) +
  stat_summary(data = timeMedia_discPow_days_marktgetr, aes(weekAsDate, ukraine, colour="Marktgetrieben"), geom = "line", fun = sum) +
  # geom_vline(xintercept = as.Date("2022-02-21"), colour = "#ff6347aa", linewidth = 1.5) +
  scale_colour_manual(name="Normen & Werte", values = coloursNormenwerte) +
  scale_x_date(breaks = breaks_pretty(n=10), labels = date_format(format = "%b, KW %V", locale = "de")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 10), legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"), legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold")) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Ukraine, sortiert nach Normen u. Werten") +
  xlab("") + ylab("")

timeMedia_discPow_days %>%
  group_by(user) %>%
  ggplot() +
  stat_summary(aes(weekAsDate, covid, fill = erreichbarkeit), geom = "bar", fun = sum, position = "fill") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b", locale = "de")) +
  facet_wrap2(. ~ normenWerte) +
  #geom_vline(xintercept = as.Date("2022-04-07"), colour = "#ff6347aa", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Thema Covid, Anteile der jeweiligen Mediengattungen",
          subtitle = "Farbliche Kennzeichnung der Anteile am Gesamtvolumen mittels der Erreichbarkeit") +
  scale_fill_manual("",values = coloursErreichbarkeit) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9), legend.box.spacing = unit(0.2, "cm")) +
  xlab("") +
  ylab("")

# datenherstellung: umbenennung der factors mit begriffen für etwaige legenden
alleMedien$geschäftsmodell <- factor(alleMedien$geschäftsmodell,
                                                    labels = c("Öffentlich", "Kommerziell", "Spenden"))
alleMedien$erreichbarkeit <- factor(alleMedien$erreichbarkeit,
                                                   labels = c("Etabliert (überregional)", "Etabliert (regional)", "Alternativ", "Individuell"))
alleMedien$normenWerte <- factor(alleMedien$normenWerte,
                                                labels = c("Objektiv-ausbalanciert", "Parteiisch", "Marktgetrieben"))

# followerzahlen medien
mostfollowedMedia <- as_tidytable(data.frame("user"=alleMedien$user,
                                             "followerAmount"=alleMedien$followerAmount,
                                             "geschäftsmodell"=alleMedien$geschäftsmodell,
                                             "normenWerte"=alleMedien$normenWerte,
                                             "erreichbarkeit"=alleMedien$erreichbarkeit))
mostfollowedMedia <- mostfollowedMedia %>% group_by(user) %>% distinct(.keep_all = T, user)
mostfollowedMedia <- mostfollowedMedia %>% arrange(desc(followerAmount))

mostfollows <- mostfollowedMedia %>%
  head(20) %>%
  ggplot() +
  geom_bar(aes(fill=as.factor(erreichbarkeit), x=followerAmount, y=reorder(user, followerAmount)), stat="identity") +
  scale_fill_manual("Erreichbarkeit", values = coloursErreichbarkeit) +
  ggtitle("Konten mit meisten FollowerInnen") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = c(0,4000000), breaks = seq(0,4000000, 500000), labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold"), axis.text.y = element_text(size = 9.5), title = element_text(size = 14))

timeMedia_discursivePower$SUMME <- 0
(timeMedia_discursivePower$SUMME <- rowSums(timeMedia_discursivePower[,-c(1:5)]))

# werte hiervon wurden mit visidata-funktion gegengecheckt und stimmen; tweets aller medien OHNE nur klassifizierte zu nehmen
medienTweets_meiste <- alleMedien %>% group_by(user) %>%
  count(user) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  as_tidytable()

medienTweetsVoll <- left_join(x = medienTweets_meiste, y = timeMedia_discursivePower)

mostactive <- medienTweetsVoll %>%
  # arrange(desc(n)) %>%
  # head(20) %>%
  ggplot() +
  geom_bar(aes(y=reorder(user, n), x = n, fill = erreichbarkeit), stat = "identity") +
  scale_fill_manual(name="Erreichbarkeit", values = coloursErreichbarkeit) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold"), axis.text.y = element_text(size = 9.5), title = element_text(size = 14)) +
  scale_x_continuous(limits = c(0,60000), breaks = seq(0,60000, 5000), labels = scales::label_number(big.mark = ".")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Aktivste Konten")

mostfollowMostactive <- ggarrange(nrow = 1, ncol = 2, mostactive, mostfollows, common.legend = T, legend = "bottom", labels = c("1", "2"))
annotate_figure(mostfollowMostactive,
                top = text_grob("Mediale Akteure im Bereich Twitter-Deutschlands", face = "bold", size = 20),
                bottom = text_grob("Hinweis: Anzahl abgebildeter Akteure je Graphik wurde auf 20 festgelegt.", size = 9))

# ========================================================================================================================================================

# covid als thema
# facetten erreichbarkeit
ggplot() +
  stat_summary(data = timeMedia_discPow_days, aes(dateTime, covid, colour=erreichbarkeit), geom = "line", fun = sum) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-04-07"), colour = "#ff6347aa", linewidth = 1) +
  facet_wrap2(.~erreichbarkeit, nrow = 4, ncol = 1, scales = "free_y") +
  scale_colour_manual(name="Erreichbarkeit", values = coloursErreichbarkeit) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5),
        legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(size = 9, face = "bold"),
        plot.caption = element_text(size = 8)) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Covid",
          subtitle = "Sortierung: Erreichbarkeit nach der 'discursive power'-Theorie") +
  xlab("") + ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nRote Linie = Abstimmung Impfpflicht.\nZeitangaben auf Tagesbasis.")

# facetten normenWerte
ggplot() +
  stat_summary(data = timeMedia_discPow_days, aes(dateTime, covid, colour=normenWerte), geom = "line", fun = sum) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-04-07"), colour = "#ff6347aa", linewidth = 1) +
  facet_wrap2(.~normenWerte, nrow = 4, ncol = 1, scales = "free_y") +
  scale_colour_manual(name="Normen & Werte", values = coloursNormenwerte) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5),
        legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(size = 9, face = "bold"), legend.spacing = unit(1.0, "cm"),
        plot.caption = element_text(size = 8)) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Covid",
          subtitle = "Sortierung: Normen u. Werten nach der 'discursive power'-Theorie") +
  xlab("") + ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nRote Linie = Abstimmung Impfpflicht.\nZeitangaben auf Tagesbasis.")

# facetten geschäftsmodell
ggplot() +
  stat_summary(data = timeMedia_discPow_days, aes(dateTime, covid, colour=geschäftsmodell), geom = "line", fun = sum) +
  geom_vline(xintercept = as.Date("2022-04-07"), colour = "#ff6347aa", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1) +
  facet_wrap2(.~geschäftsmodell, nrow = 3, ncol = 1, scales = "free_y") +
  scale_colour_manual(name="Geschäftsmodell", values = coloursGeschäftsmodell) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5),
        legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(size = 9, face = "bold"),
        plot.caption = element_text(size = 8)) +
  ggtitle(label = "Zeitreihen: Medien zum Thema Ukraine",
          subtitle = "Sortierung nach Geschäftsmodell in der 'discursive power'-Theorie") +
  xlab("") + ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nRote Linie = Abstimmung Impfpflicht.\nZeitangaben auf Tagesbasis.")

# =============================================================================================================================================================

# gruppenanteile an berichterstattungen nach ZWEI discpow variablen
timeMedia_discPow_days %>%
  group_by(user) %>%
  ggplot() +
  stat_summary(aes(weekAsDate, ukraine, fill = normenWerte), geom = "bar", fun = sum, position = "fill") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  facet_wrap2(. ~ erreichbarkeit) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Anteile an Tweet-Gesamtvolumen zum Thema Ukraine",
          subtitle = "Gruppiert nach Erreichbarkeit | Farblich abgesetzt: Normen und Werte") +
  scale_fill_manual("",values = coloursNormenwerte) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9),
        legend.box.spacing = unit(0.2, "cm"), plot.caption = element_text(size = 8)) +
  xlab("") +
  ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nZeitebene = wöchentlich.")

timeMedia_discPow_days %>%
  group_by(user) %>%
  ggplot() +
  stat_summary(aes(weekAsDate, covid, fill = normenWerte), geom = "bar", fun = sum, position = "fill") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  facet_wrap2(. ~ erreichbarkeit) +
  geom_vline(xintercept = as.Date("2022-04-07"), colour = "#ff6347aa", linewidth = 1.5) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Anteile an Tweet-Gesamtvolumen zum Thema Covid",
          subtitle = "Gruppiert nach Erreichbarkeit | Farblich abgesetzt: Normen und Werte") +
  scale_fill_manual("",values = coloursNormenwerte) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9), legend.box.spacing = unit(0.2, "cm")) +
  xlab("") +
  ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nRote Linie = Abstimmung Impfpflicht.\nZeitebene = wöchentlich.")

timeMedia_discPow_days %>%
  group_by(user) %>%
  ggplot() +
  stat_summary(aes(weekAsDate, ukraine, fill = normenWerte), geom = "bar", fun = sum, position = "fill") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  facet_wrap2(. ~ geschäftsmodell) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Anteile an Tweet-Gesamtvolumen zum Thema Ukraine",
          subtitle = "Gruppiert nach Geschäftsmodell | Farblich abgesetzt: Normen und Werte") +
  scale_fill_manual("",values = coloursNormenwerte) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9),
        legend.box.spacing = unit(0.2, "cm"), plot.caption = element_text(size = 8)) +
  xlab("") +
  ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nZeitebene = wöchentlich.")

timeMedia_discPow_days %>%
  group_by(user) %>%
  ggplot() +
  stat_summary(aes(weekAsDate, covid, fill = normenWerte), geom = "bar", fun = sum, position = "fill") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  facet_wrap2(. ~ geschäftsmodell) +
  geom_vline(xintercept = as.Date("2022-04-07"), colour = "#ff6347aa", linewidth = 1.5) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Anteile an Tweet-Gesamtvolumen zum Thema Covid",
          subtitle = "Gruppiert nach Geschäftsmodell | Farblich abgesetzt: Normen und Werte") +
  scale_fill_manual("",values = coloursNormenwerte) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9), legend.box.spacing = unit(0.2, "cm")) +
  xlab("") +
  ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nRote Linie = Abstimmung Impfpflicht.\nZeitebene = wöchentlich.")

timeMedia_discPow_days %>%
  group_by(user) %>%
  ggplot() +
  stat_summary(aes(weekAsDate, ukraine, fill = geschäftsmodell), geom = "bar", fun = sum, position = "fill") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  facet_wrap2(. ~ erreichbarkeit) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Anteile an Tweet-Gesamtvolumen zum Thema Ukraine",
          subtitle = "Gruppiert nach Erreichbarkeit | Farblich abgesetzt: Geschäftsmodell") +
  scale_fill_manual("",values = coloursGeschäftsmodell) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9),
        legend.box.spacing = unit(0.2, "cm"), plot.caption = element_text(size = 8)) +
  xlab("") +
  ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nZeitebene = wöchentlich.")

timeMedia_discPow_days %>%
  group_by(user) %>%
  ggplot() +
  stat_summary(aes(weekAsDate, covid, fill = geschäftsmodell), geom = "bar", fun = sum, position = "fill") +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b,\nKW %V", locale = "de")) +
  facet_wrap2(. ~ erreichbarkeit) +
  geom_vline(xintercept = as.Date("2022-04-07"), colour = "#ff6347aa", linewidth = 1.5) +
  geom_vline(xintercept = as.Date("2022-02-24"), colour = "#0000cdaa", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Anteile an Tweet-Gesamtvolumen zum Thema Covid",
          subtitle = "Gruppiert nach Erreichbarkeit | Farblich abgesetzt: Geschäftsmodell") +
  scale_fill_manual("",values = coloursGeschäftsmodell) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9), legend.box.spacing = unit(0.2, "cm")) +
  xlab("") +
  ylab("") +
  labs(caption = "Blaue Linie = Tag der russischen Invasion.\nRote Linie = Abstimmung Impfpflicht.\nZeitebene = wöchentlich.")

# appendix 15: alternative darstellung von abb. 18
mostfollowedMedia %>%
  head(20) %>%
  ggplot() +
  geom_bar(aes(fill=as.factor(geschäftsmodell), x=followerAmount, y=reorder(user, followerAmount)), stat="identity") +
  scale_fill_manual("Geschäftsmodell", values = coloursGeschäftsmodell) +
  ggtitle("Konten mit meisten FollowerInnen, inkl. Geschäftsmodell") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = c(0,4000000), breaks = seq(0,4000000, 500000), labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "bottom", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold"), axis.text.y = element_text(size = 9.5), title = element_text(size = 14))

# ========================================================================================================================================================
# ANALYSEN ZUR REGIONALITÄT
# ========================================================================================================================================================

# richtiger datensatz mit ost und west teilung erstellen
# in diesem fall muss überlegt werden, was mit denen passiert, die überregional sind
# am schlauesten ist wahrscheinlich wegzulassen
# überregional sagt ja auch aus: das ist nicht gebunden
ostdeutschlandListe <- c("Thüringen", "Sachsen", "Sachsen-Anhalt", "Brandenburg", "Mecklenburg-Vorpommern")
westdeutschland <- c("Baden-Württemberg", "Bayern", "Hessen", "Rheinland-Pfalz", "Nordrhein-Westfalen",
                     "Niedersachsen", "Schleswig-Holstein", "Saarland", "Bremen", "Hamburg", "Berlin")

# politiker und mediendaten je nach zuordnung zu ost und west
politikOst <- timePol_maximus %>% filter(bundesland %in% ostdeutschlandListe)
medienOst <- timeMedia_discursivePower %>% filter(bundesland %in% ostdeutschlandListe)
#
politikWest <- timePol_maximus %>% filter(bundesland %in% westdeutschland)
medienWest <- timeMedia_discursivePower %>% filter(bundesland %in% westdeutschland)

#===============================================================================

überblickPolOst <- as_tidytable(apply(X=politikOst[,c(5:19)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickPolOst <- überblickPolOst %>% rename(names = rn, anzahl = x)

überblickMedienOst <- as_tidytable(apply(X = medienOst[,c(6:20)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickMedienOst <- überblickMedienOst %>% rename(names = rn, anzahl = x)

überblickPolWest <- as_tidytable(apply(X = politikWest[,c(5:19)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickPolWest <- überblickPolWest %>% rename(names = rn, anzahl = x)

überblickMedienWest <- as_tidytable(apply(X = medienWest[,c(6:20)], MARGIN = 2, FUN = sum), .keep_rownames = T)
überblickMedienWest <- überblickMedienWest %>% rename(names = rn, anzahl = x)

###

sum(überblickMedienOst$anzahl)
sum(überblickPolOst$anzahl)
sum(überblickMedienWest$anzahl)
sum(überblickPolWest$anzahl)

#===============================================================================

# normalisierung auf anteilig 100%
proportionalOst_politiker <- as_tidytable(apply(überblickPolOst[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalOst_politiker$names <- überblickPolOst$names

proportionalWest_politiker <- as_tidytable(apply(überblickPolWest[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalWest_politiker$names <- überblickPolWest$names

proportionalOst_medien <- as_tidytable(apply(überblickMedienOst[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalOst_medien$names <- überblickMedienOst$names

proportionalWest_medien <- as_tidytable(apply(überblickMedienWest[,2], MARGIN = 2, FUN = formel_prozentualisierung))
proportionalWest_medien$names <- überblickMedienWest$names

#===============================================================================

# min max normalisierung
processPolOst <- preProcess(as_tidytable(überblickPolOst), method = "range")
überblickPolOst_minmax <- predict(processPolOst, as_tidytable(überblickPolOst))

processPolWest <- preProcess(as_tidytable(überblickPolWest), method = "range")
überblickPolWest_minmax <- predict(processPolWest, as_tidytable(überblickPolWest))

processMedienOst <- preProcess(as_tidytable(überblickMedienOst), method = "range")
überblickMedienOst_minmax <- predict(processMedienOst, as_tidytable(überblickMedienOst))

processMedienWest <- preProcess(as_tidytable(überblickMedienWest), method = "range")
überblickMedienWest_minmax <- predict(processMedienWest, as_tidytable(überblickMedienWest))

#===============================================================================

# datensätze für osten und westen, jeweils medien UND politiker
gemeinsamOst_absolut <- überblickPolOst %>% mutate(kennzeichnung = "Politiker Ost") %>%
  bind_rows(überblickMedienOst) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ost"))

gemeinsamOst_proportional <- proportionalOst_politiker %>% mutate(kennzeichnung = "Politiker Ost") %>%
  bind_rows(proportionalOst_medien) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ost"))

gemeinsamOst_minmax <- überblickPolOst_minmax %>% mutate(kennzeichnung = "Politiker Ost") %>%
  bind_rows(überblickMedienOst_minmax) %>%
  mutate(kennzeichnung = replace_na(kennzeichnung, "Medien Ost"))

ganzOsten <- as_tidytable(data.frame("names" = gemeinsamOst_absolut$names,
                                     "anzahlAbs" = gemeinsamOst_absolut$anzahl,
                                     "anzahlProporz" = gemeinsamOst_proportional$anzahl,
                                     "anzahlMinMax" = gemeinsamOst_minmax$anzahl,
                                     "kennzeichnung" = gemeinsamOst_absolut$kennzeichnung))

gemeinsamWest_absolut <- überblickPolWest %>% mutate(kennzeichnung = "Politiker West") %>%
bind_rows(überblickMedienWest) %>%
mutate(kennzeichnung = replace_na(kennzeichnung, "Medien West"))

gemeinsamWest_proportional <- proportionalWest_politiker %>% mutate(kennzeichnung = "Politiker West") %>%
bind_rows(proportionalWest_medien) %>%
mutate(kennzeichnung = replace_na(kennzeichnung, "Medien West"))

gemeinsamWest_minmax <- überblickPolWest_minmax %>% mutate(kennzeichnung = "Politiker West") %>%
bind_rows(überblickMedienWest_minmax) %>%
mutate(kennzeichnung = replace_na(kennzeichnung, "Medien West"))

ganzWesten <- as_tidytable(data.frame("names" = gemeinsamWest_absolut$names,
                                     "anzahlAbs" = gemeinsamWest_absolut$anzahl,
                                     "anzahlProporz" = gemeinsamWest_proportional$anzahl,
                                     "anzahlMinMax" = gemeinsamWest_minmax$anzahl,
                                     "kennzeichnung" = gemeinsamWest_absolut$kennzeichnung))

kompletto <- as_tidytable(data.frame("names" = ganzWesten$names,
                        "absolutWesten" = ganzWesten$anzahlAbs,
                        "proporzWesten" = ganzWesten$anzahlProporz,
                        "minmaxWesten" = ganzWesten$anzahlMinMax,
                        "absolutOsten" = ganzOsten$anzahlAbs,
                        "proporzOsten" = ganzOsten$anzahlProporz,
                        "minmaxOsten" = ganzOsten$anzahlMinMax,
                        "kennung" = ganzWesten$kennzeichnung))

kompletto$kennzeichnung[kompletto$kennung == "Politiker West"] <- "Politiker"
kompletto$kennzeichnung[kompletto$kennung == "Medien West"] <- "Medien"

kompletto$names <- c("Covid", "Ukraine", "Energie", "Soziales", "Verteidigungspolitik", "PolizistenmordKusel", "FlutAhrtal", "PolitikEuropa", "PolitikInternational", "Klima", "ProtesteIran", "Verkehr", "PluralismusMedien", "Zukunft", "Verfassungsfeindlich", "Covid", "Ukraine", "Energie", "Soziales", "Verteidigungspolitik", "PolizistenmordKusel", "FlutAhrtal", "PolitikEuropa", "PolitikInternational", "Klima", "ProtesteIran", "Verkehr", "PluralismusMedien", "Zukunft", "Verfassungsfeindlich")

ganzOsten <- ganzOsten %>% mutate(names = fct_reorder(names, desc(-anzahlAbs)))
ganzWesten <- ganzWesten %>% mutate(names = fct_reorder(names, desc(-anzahlAbs)))
kompletto <- kompletto %>% mutate(names = fct_reorder(names, desc(absolutWesten)))

#===============================================================================

# # medien und politiker auf 100% normalisiert, macht an sich gar nicht sehr viel sinn, nur nice to have
# proportionalWest_ganz<- as_tidytable(apply(gemeinsamWest[,2], MARGIN = 2, FUN = formel_prozentualisierung))
# proportionalWest_ganz$names <- gemeinsamWest$names
#
# proportionalOst_ganz<- as_tidytable(apply(gemeinsamOst[,2], MARGIN = 2, FUN = formel_prozentualisierung))
# proportionalOst_ganz$names <- gemeinsamOst$names

# norm ost vs. norm west: alle vier gruppen mit 2x pol und 2x medien sind jeweils in sich selbst genormt
# ANGLEICHUNG AN NEUE NAMENSGEBUNG NOTWENDIG
# gnormt <- data.frame(names = themenüberblickPolitikerNorm$names,
#                      politikerOst = proportionalOst_politiker$anzahl,
#                      politikerWest = proportionalWest_politiker$anzahl,
#                      medOst = proportionalOst_medien$anzahl,
#                      medWest = proportionalWest_medien$anzahl)

# factor order GNORMT
# SORT_gnormt_politiker <- gnormt %>% mutate(names = fct_reorder(names, desc(-politikerOst)))
# SORT_gnormt_medien <- gnormt %>% mutate(names = fct_reorder(names, desc(-medOst)))
# SORT_gnormt_alleGleich <- gnormt %>% mutate(names = fct_reorder(names, desc(-politikerOst)))



# vorsicht: hier wird gesamtdeutschland mit osten verglichen; gesamtdeutschland enthält hier nochmal osten, was ergebnis verzerrt
# es werden potentiell doppelte nennungen gemacht
# ggplot(data = gemeinsamOstNormUngleichAbsolut) +
#   geom_segment(aes(x=names, xend=names, y=anzahlPolitikerOstNorm, yend=anzahlPolitikerNorm), colour="white") +
#   geom_point(aes(x=names, y=anzahlPolitikerNorm, colour = "Politiker Gesamt")) +
#   geom_point(aes(x=names, y=anzahlPolitikerOstNorm, colour = "Politiker Ost")) +
#   theme_ft_rc() +
#   scale_colour_manual(name="Gruppe", values = paletteCategorical3) +
#   coord_flip() +
#   ggtitle(label = "Themen", subtitle = "") +
#   xlab("Themen") +
#   ylab("Anteil %")

#===============================================================================

# politiker ostdeutschland und westdeutschland im vergleich
politikerPalette <- c("West"="#10adf0", "Ost"="#bb0040")
medienPalette <- c("West"="blue3", "Ost"="goldenrod1")

komplettoPolitiker <- kompletto %>% filter(kennzeichnung == "Politiker")
komplettoMedien <- kompletto %>% filter(kennzeichnung == "Medien")

(politiker_ostwest <- ggplot(komplettoPolitiker) +
  geom_segment(aes(x=proporzOsten,
                   xend=proporzWesten,
                   y=reorder(names, komplettoPolitiker$absolutWesten),
                   yend=reorder(names, komplettoPolitiker$absolutWesten)),
               colour="grey50") +
  geom_point(aes(x=proporzWesten,
                 y=reorder(names, komplettoPolitiker$absolutWesten),
                 colour = "West")) +
  geom_point(aes(x=proporzOsten,
                 y=reorder(names, komplettoPolitiker$absolutWesten),
                 colour = "Ost")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom") +
  scale_x_percent(breaks = seq(0, 0.21, 0.02), limits = c(0, 0.21)) +
  scale_colour_manual(name="Herkunftsregion", values = politikerPalette) +
  #coord_flip() +
  ggtitle(label = "Themen PolitikerInnen") +
  xlab("Anteil %") +
  ylab(" "))

# ggplotly(politiker_ostwest)

# medien ostdeutschland und westdeutschland im vergleich
(medien_ostwest <- ggplot(komplettoMedien) +
  geom_segment(aes(x=proporzOsten,
                   xend=proporzWesten,
                   y=reorder(names, komplettoPolitiker$absolutWesten),
                   yend=reorder(names, komplettoPolitiker$absolutWesten)),
               colour="grey50") +
  geom_point(aes(x=proporzWesten,
                 y=reorder(names, komplettoPolitiker$absolutWesten),
                 colour = "West")) +
  geom_point(aes(x=proporzOsten,
                 y=reorder(names, komplettoPolitiker$absolutWesten),
                 colour = "Ost")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom") +
  scale_x_percent(breaks = seq(0, 0.27, 0.02), limits = c(0, 0.27)) +
  scale_colour_manual(name="Herkunftsregion", values = politikerPalette) +
  #coord_flip() +
  ggtitle(label = "Themen Medien") +
  xlab("Anteil %") +
  ylab(" "))

# ggplotly(medien_ostwest)

lollipop_vergleich_ostwest <- ggarrange(politiker_ostwest, medien_ostwest, nrow = 1, ncol = 2, labels = c("1", "2"), common.legend = T, legend = "bottom")
annotate_figure(lollipop_vergleich_ostwest, top = text_grob("Vergleich der Themenaufmerksamkeit zwischen Ost- und Westdeutschland\nFokus: geographische Regionen", size = 20, face = "bold"))

#===============================================================================

komplettoOsten <- as_tidytable(data.frame("names"=komplettoPolitiker$names,
                                          "absolutPolitiker"=komplettoPolitiker$absolutOsten,
                                          "proporzPolitiker"=komplettoPolitiker$proporzOsten,
                                          "absolutMedien" = komplettoMedien$absolutOsten,
                                          "proporzMedien" = komplettoMedien$proporzOsten,
                                          "herkunft"="Ostdeutschland"))
komplettoWesten <- as_tidytable(data.frame("names"=komplettoPolitiker$names,
                                           "absolutPolitiker"=komplettoPolitiker$absolutWesten,
                                           "proporzPolitiker"=komplettoPolitiker$proporzWesten,
                                           "absolutMedien" = komplettoMedien$absolutWesten,
                                           "proporzMedien" = komplettoMedien$proporzWesten,
                                           "herkunft"="Westdeutschland"))

# ========================================================================================================================================================
# APPENDIXMATERIAL
# ========================================================================================================================================================

#covid
timePol_maximus %>%
  group_by(partei) %>%
ggplot() +
  stat_summary(aes(weekAsDate, covid, fill = partei), geom = "bar", fun = sum, position = "stack") +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\n KW %W", locale = "de"), expand = c(0,5)) +
  scale_y_continuous(limits = c(0,950), breaks = seq(0, 950, 150)) +
  #geom_vline(xintercept = as.Date("2022-04-07"), colour = "#ff6347aa", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Thema Covid", subtitle = "Anteile der jeweiligen Politiker, sortiert nach Parteien (wöchentliche Angaben)") +
  scale_fill_manual("Parteizugehörigkeit",values = parteifarben) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold")) +
  xlab("") +
  ylab("")

#ukraine
timePol_maximus %>%
  group_by(partei) %>%
  ggplot() +
  stat_summary(aes(weekAsDate, ukraine, fill = partei), geom = "bar", fun = sum, position = "stack") +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de"), expand = c(0,5)) +
  scale_y_continuous(limits = c(0,2250), breaks = seq(0, 2250, 500), labels = scales::label_number(big.mark = ".")) +
  #geom_vline(xintercept = as.Date("2022-02-24"), colour = "blue", linewidth = 1.5) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Thema Ukraine", subtitle = "Anteile der jeweiligen Politiker, sortiert nach Parteien (wöchentliche Angaben)") +
  scale_fill_manual("Parteizugehörigkeit",values = parteifarben) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold")) +  xlab("") +
  ylab("")

# ========================================================================================================================================================

COVIDalt1 <- ggplot(regop) +
  stat_summary(aes(weekAsDate, covid, fill=regop), geom = "area", fun = sum, na.rm = T, position = "fill") +
  scale_fill_manual(name="", values = farbenRegop) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de"), expand = c(0,5)) +
  scale_y_percent() +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Anteil Themenaufmerksamkeit Covid: Regierung und Opposition") +
  # scale_fill_manual(name="PolitikerInnen", values = sechzehnFarben) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic", size = 8), legend.title = element_text(face = "bold")) +
  xlab("") +
  ylab("")

COVIDalt2 <- ggplot() +
  stat_summary(data = activeCovidUsers_spd, aes(weekAsDate, covid, colour = "SPD"), geom = "line", fun = sum) + #effektiv karl lauterbach alleine
  stat_summary(data = activeCovidUsers_fdp, aes(weekAsDate, covid, colour = "FDP"), geom = "line", fun = sum) +
  stat_summary(data = activeCovidUsers_grüne, aes(weekAsDate, covid, colour = "B90/Die Grünen"), geom = "line", fun = sum) +
  stat_summary(data = activeCovidUsers_afd, aes(weekAsDate, covid, colour = "AfD"), geom = "line", fun = sum) +
  scale_x_date(breaks = "2 weeks", labels = date_format(format = "%b\nKW %W", locale = "de"), expand = c(0,5)) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Covid: Tweets/Woche", subtitle = "Vergleich zwischen AfD und den Regierungsparteien") +
  scale_colour_manual(name="Parteizugehörigkeit", values = parteifarben) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic", size = 8), legend.title = element_text(face = "bold")) +
  xlab("") +
  ylab("")

ggarrange(nrow = 2, ncol = 1, common.legend = F,
          COVIDalt1, COVIDalt2, labels = c("1", "2"))

# ========================================================================================================================================================

# ukraine
UKRalt1 <- ggplot(regop) +
  stat_summary(aes(weekAsDate, ukraine, fill=regop), geom = "area", fun = sum, na.rm = T, position = "fill") +
  scale_fill_manual(name="", values = farbenRegop) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b\nWK %W", locale = "de"), expand = c(0,5)) +
  scale_y_percent() +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Anteil Themenaufmerksamkeit Ukraine: Regierung und Opposition") +
  # scale_fill_manual(name="PolitikerInnen", values = sechzehnFarben) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic", size = 8), legend.title = element_text(face = "bold")) +
  xlab("") +
  ylab("")

UKRalt2 <- ggplot() +
  stat_summary(data = activeUkraineUsers_spd, aes(weekAsDate, ukraine, colour = "SPD"), geom = "line", fun = sum) +
  stat_summary(data = activeUkraineUsers_fdp, aes(weekAsDate, ukraine, colour = "FDP"), geom = "line", fun = sum) +
  stat_summary(data = activeUkraineUsers_grüne, aes(weekAsDate, ukraine, colour = "B90/Die Grünen"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b\nWK %W", locale = "de"), expand = c(0,5)) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Regierung: Ukraine-Tweets pro Woche") +
  scale_colour_manual(name="Regierung", values = parteifarben) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic", size = 8), legend.title = element_text(face = "bold")) +
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0,225))

UKRalt3 <- ggplot() +
  stat_summary(data = activeUkraineUsers_linke, aes(weekAsDate, ukraine, colour = "Die Linke"), geom = "line", fun = sum) +
  stat_summary(data = activeUkraineUsers_afd, aes(weekAsDate, ukraine, colour = "AfD"), geom = "line", fun = sum) +
  stat_summary(data = activeUkraineUsers_ssw, aes(weekAsDate, ukraine, colour = "SSW"), geom = "line", fun = sum) +
  stat_summary(data = activeUkraineUsers_csu, aes(weekAsDate, ukraine, colour = "CSU"), geom = "line", fun = sum) +
  stat_summary(data = activeUkraineUsers_cdu, aes(weekAsDate, ukraine, colour = "CDU"), geom = "line", fun = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b\nWK %W", locale = "de"), expand = c(0,5)) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  ggtitle(label = "Opposition: Ukraine-Tweets pro Woche") +
  scale_colour_manual(name="Opposition", values = parteifarben) +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(face = "italic", size = 8), legend.title = element_text(face = "bold")) +
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0,225))

ggarrange(ncol = 2, common.legend = F,
          UKRalt2, UKRalt3, labels = c("1", "2"))

# ========================================================================================================================================================

# appendix 16
(appendix16A <- ggplot(komplettoOsten) +
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
   scale_x_percent(breaks = seq(0, 0.27, 0.02), limits = c(0, 0.27)) +
   scale_colour_manual(name=" ", values = farbenPolitikerMedien) +
   #coord_flip() +
   ggtitle(label = "Themen Ostdeutschland") +
   xlab("Anteil %") +
   ylab(" "))

# ggplotly(politiker_ostwest)

# medien ostdeutschland und westdeutschland im vergleich
(appendix16B <- ggplot(komplettoWesten) +
    geom_segment(aes(x=proporzPolitiker,
                     xend=proporzMedien,
                     y=reorder(names, komplettoOsten$absolutPolitiker),
                     yend=reorder(names, komplettoOsten$absolutPolitiker)),
                 colour="grey50") +
    geom_point(aes(x=proporzPolitiker,
                   y=reorder(names, komplettoOsten$absolutPolitiker),
                   colour = "PolitikerInnen")) +
    geom_point(aes(x=proporzMedien,
                   y=reorder(names, komplettoOsten$absolutPolitiker),
                   colour = "Medien")) +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
          legend.text = element_text(face = "italic"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom") +
    scale_x_percent(breaks = seq(0, 0.25, 0.02), limits = c(0, 0.25)) +
    scale_colour_manual(name=" ", values = farbenPolitikerMedien) +
    #coord_flip() +
    ggtitle(label = "Themen Westdeutschland") +
    xlab("Anteil %") +
    ylab(" "))

# ggplotly(medien_ostwest)

lollipopAppendix16 <- ggarrange(appendix16A, appendix16B, nrow = 1, ncol = 2, labels = c("1", "2"), common.legend = T, legend = "bottom")
annotate_figure(lollipopAppendix16, top = text_grob("Vergleich der Themenaufmerksamkeit zwischen den Akteurssphären\nFokus: Akteure in den Regionen",
                                                            size = 20, face = "bold"))





