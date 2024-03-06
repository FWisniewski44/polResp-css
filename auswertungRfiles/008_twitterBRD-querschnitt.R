#===============================================================================================================================================
#===============================================================================================================================================
#===============================================================================================================================================
# bezug auf politikerdaten zum überblick von twitter-deutschland

freq(allePolitiker$partei)
politikerAnalysedaten_Parteien$SUMME <- rowSums(politikerAnalysedaten_Parteien[,-1])

politikerAnalysedaten_Parteien
freq(politikerAnalysedaten_Parteien$partei)

overviewPolitiker <- data.frame("partei"=politikerAnalysedaten_Parteien$partei,
                                "absoluteTweets"=c(16010, 40195, 22030, 3298, 12824, 29186, 22862, 214),
                                "anteiligGes"=c(0.1092, 0.2742, 0.1503, 0.0224, 0.0875, 0.1991, 0.1559, 0.0015),
                                "klassifiziertAbs"=politikerAnalysedaten_Parteien$SUMME)
overviewPolitiker <- overviewPolitiker %>% as_tidytable()
overviewPolitiker <- overviewPolitiker %>% mutate(klassifiziertProzent = klassifiziertAbs / absoluteTweets)
overviewPolitiker$gruppe <- "Politiker"

# graphik für einführungsteil, variante 1
anteil_tweets_partei <- overviewPolitiker %>%
  mutate(partei = fct_reorder(partei, desc(absoluteTweets))) %>% ggplot() +
  geom_bar(stat = "identity", position = "fill", aes(x=gruppe, y=absoluteTweets, fill=partei)) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  scale_y_percent() +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_blank(), axis.title.x =  element_blank(), axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("") +
  ggtitle("Anteil der Parteien an Tweets im Datensatz", subtitle = "Basis: eigene Datenerhebung auf Twitter")

# graphik für einführungsteil, variante 2
overviewPolitiker %>%
  mutate(partei = fct_reorder(partei, desc(absoluteTweets))) %>% ggplot() +
  geom_bar(stat = "identity", aes(x=partei, y=absoluteTweets, fill=partei)) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  #scale_y_percent() +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9)) +
  xlab("") +
  ylab("")

# variante 3: doughnut plot
library(ggpol)
library(ggparliament)
#
# Compute the cumulative percentages (top of each rectangle)
overviewPolitiker$ymax <- cumsum(overviewPolitiker$anteiligGes)

# Compute the bottom of each rectangle
overviewPolitiker$ymin <-  c(0, head(overviewPolitiker$ymax, n=-1))

overviewPolitiker %>%
  mutate(partei = fct_reorder(partei, desc(ymax))) %>% ggplot() +
  geom_rect(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=partei)) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(name="Partei", values = parteifarben) +
  xlim(c(2,4))

anteil_tweets_partei_alternativ <- overviewPolitiker %>%
  mutate(partei = fct_reorder(partei, desc(anteiligGes))) %>%
  ggplot() +
  geom_arcbar(aes(fill=partei, shares=anteiligGes, r0=7, r1=10)) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  theme_ipsum() +
  ggtitle("Anteil der Parteien an Tweets im Datensatz", subtitle = "Basis: eigene Datenerhebung auf Twitter") +
  xlab("") +
  ylab("")

#####

# datensatz erstellen für bundestagsparteien basierend auf wahlergebnis 2021
bundestag <- data.frame("partei"=politikerAnalysedaten_Parteien$partei,
                        "sitze"=c(83, 118, 152, 45, 39, 92, 206, 1),
                        "prozentErreicht"=c(.103, .148, .189, .052, .049, .115, .257, .001),
                        "gruppe"="Politiker")

sitze_bundestag_alternativ <- bundestag %>%
  mutate(partei = fct_reorder(partei, desc(sitze))) %>%
  ggplot() +
  geom_arcbar(aes(fill=partei, shares=sitze, r0=7, r1=10)) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  theme_ipsum() +
  ggtitle("Sitzverteilung im Bundestag", subtitle = "Basis: amtliches Endergebnis der Bundestagswahl 2021") +
  xlab("") +
  ylab("")

sitze_bundestag <- bundestag %>%
  mutate(partei = fct_reorder(partei, desc(sitze))) %>%
  ggplot() +
  geom_bar(stat = "identity", position = "fill", aes(x=gruppe, y=sitze, fill=partei)) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  scale_y_percent() +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_blank(), axis.title.x =  element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("1) Sitzverteilung im Bundestag", subtitle = "Basis: amtliches Endergebnis der Bundestagswahl 2021") +
  xlab("") +
  ylab("")

# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank()

parteienTwitter <- data.frame("partei"=politikerAnalysedaten_Parteien$partei,
                              "vorkommen"=c(61, 109, 89, 26, 33, 84, 129, 1),
                              "vorkommenProzent"=c(.1147, .2049, .1673, .0489, .0620, .1579, .2425, .0019),
                              "gruppe"="Politiker")
parteienTwitter <- parteienTwitter %>% mutate(abgeordnete_mit_account_prozent = vorkommen / bundestag$sitze)

beschaffenheit_twitterdeutschland <- parteienTwitter %>%
  mutate(partei = fct_reorder(partei, desc(vorkommen))) %>%
  ggplot() +
  geom_bar(stat = "identity", position = "fill", aes(x=gruppe, y=vorkommen, fill=partei)) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  scale_y_percent() +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_blank(), axis.title.x =  element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("2) Anzahl Abgeordnete/Partei mit Twitter-Account", subtitle = "Basis: eigens erhobene Daten, Zeitraum Jan.-Okt. 2022") +
  xlab("") +
  ylab("")

# vergleich sitze im parlament mit tweets pro partei
ggarrange(nrow=1, ncol=2,
          sitze_bundestag, anteil_tweets_partei,
          legend="bottom", common.legend = T)

# vergleich sitze im parlament mit accounts auf twitter pro partei
ggarrange(nrow=1, ncol=2,
          sitze_bundestag, beschaffenheit_twitterdeutschland,
          legend="bottom", common.legend = T)

# alternative darstellungsform als halbe donuts
ggarrange(nrow = 2, ncol=1,
          sitze_bundestag_alternativ, anteil_tweets_partei_alternativ)

freq(politikerAnalysedaten_userParteien$partei)


twitterHorizontal <- parteienTwitter %>%
  mutate(partei = fct_reorder(partei, desc(-vorkommenProzent))) %>%
  ggplot() +
  geom_bar(stat = "identity", aes(x=partei, y=vorkommenProzent, fill=partei)) +
  coord_flip() +
  scale_fill_manual(name="Partei", values = parteifarben) +
  scale_y_percent(limits = c(0,.28), breaks = seq(0, .25, .05)) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(legend.position = "none") +
  ggtitle("Twitter-Accounts pro Partei", subtitle = "Anteil in % | Basis: eigens erhobene Daten") +
  xlab("") +
  ylab("")

bundestagHorizontal <- bundestag %>%
  mutate(partei = fct_reorder(partei, desc(-prozentErreicht))) %>%
  ggplot() +
  geom_bar(stat = "identity", aes(x=partei, y=prozentErreicht, fill=partei)) +
  coord_flip() +
  scale_fill_manual(name="Partei", values = parteifarben) +
  scale_y_percent(limits = c(0,.28), breaks = seq(0, .25, .05)) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(legend.position = "none") +
  ggtitle("Sitzverteilung im Bundestag", subtitle = "Anteil in % | Basis: amtliches Endergebnis der Bundestagswahl 2021") +
  xlab("") +
  ylab("")

tweetanteilHorizontal <- overviewPolitiker %>%
  mutate(partei = fct_reorder(partei, desc(anteiligGes))) %>%
  ggplot() +
  geom_bar(stat = "identity", position = "stack", aes(x=anteiligGes, y=gruppe, fill=partei), colour="white") +
  #annotate(geom = "label", x = c(0.161, 0.863, 0.30, 0.1, 0.0075, 0.625, 0.45, 0), y=1, label = overviewPolitiker$absoluteTweets, ) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  scale_x_percent(breaks = seq(0, 1.0, .10)) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.y = element_blank(), axis.title.y =  element_blank(), axis.ticks.y = element_blank(), legend.position = "none") +
  xlab("") +
  ylab("") +
  ggtitle("Anteil Tweets pro Partei am Gesamtvolumen", subtitle = "Anteil in % | Basis: eigens erhobene Daten")

ggarrange(ggarrange(bundestagHorizontal, twitterHorizontal, ncol = 2, labels = c("1)", "2)")),
          ggarrange(tweetanteilHorizontal), nrow = 2, labels = c("", "3)"))


tweetanteilHorizontal_alternativ <- overviewPolitiker %>%
  mutate(partei = fct_reorder(partei, desc(-anteiligGes))) %>%
  ggplot() +
  geom_bar(stat = "identity", aes(y=partei, x=anteiligGes, fill=partei)) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  scale_x_percent(limits = c(0,.30), breaks = seq(0, .30, .05)) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(legend.position = "none") +
  ggtitle("3) Anteil an der Gesamtzahl von Tweets seitens der Parteien", subtitle = "Prozentuale Angaben\nBasis: eigens erhobene Daten für den Untersuchungszeitraum") +
  xlab("") +
  ylab("")

bundestag %>%
  mutate(partei = fct_reorder(partei, desc(prozentErreicht))) %>%
  ggplot() +
  geom_bar(stat = "identity", position = "stack", aes(x=gruppe, y=prozentErreicht, fill=partei)) +
  scale_fill_manual(name="Partei", values = parteifarben) +
  scale_y_percent() +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_blank(), axis.title.x =  element_blank(), axis.ticks.x = element_blank()) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Anteil der Parteien an Tweets im Datensatz", subtitle = "Basis: eigene Datenerhebung auf Twitter")

ggarrange(legend = "none", ncol = 2, nrow = 2,
          bundestagHorizontal,
          twitterHorizontal,
          ggarrange(nrow = 1, tweetanteilHorizontal, legend = "right"))

ggplot(parteienTwitter) +
  geom_point(stat="identity",aes(x=partei,y=abgeordnete_mit_account_prozent, color=partei)) +
  geom_text(aes(x=partei, y=abgeordnete_mit_account_prozent, label=partei), nudge_y = -0.015) +
  scale_color_manual(values = parteifarben) +
  scale_y_percent(limits = c(0,1)) +
  geom_hline(yintercept = 0.776, color="red")


library(treemap)

treemap(overviewPolitiker,
        index = "partei",
        vSize = "anteiligGes",
        type = "index")

# treemap versuch: leserlichkeit?

# library(treemapify)
#
# ggplot(data = overviewPolitiker) +
#   geom_treemap(aes(fill = partei, area = anteiligGes), start = "topleft", color = "white") +
#   geom_treemap_text(aes(area=anteiligGes, label = partei), place = "topleft", colour="#fffff9aa", size = 35, start = "topleft") +
#   geom_treemap_text(aes(area=anteiligGes, label = absoluteTweets), place = "topright", colour = "#fffff9aa", size = 25, start = "topleft") +
#   scale_fill_manual(values = parteifarben) +
#   theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
#   theme(legend.position = "none") +
#   ggtitle("Anteile der Parteien an der Gesamtzahl der Tweets", subtitle = "Annotation: Parteinamen und Anzahl der Tweets pro Partei")

# datensatz: politiker, die im Untersuchungszeitraum am meisten getweetet haben
politikerAnalysedaten_userParteien$SUMME <- 0
politikerAnalysedaten_userParteien$SUMME <- rowSums(politikerAnalysedaten_userParteien[,-c(1,2)])

politikerAnalysedaten_userParteien <- politikerAnalysedaten_userParteien %>% arrange(desc(SUMME))

meistTweetende <- tidytable("name" = politikerAnalysedaten_userParteien$user,
                            "partei"=politikerAnalysedaten_userParteien$partei,
                            "tweetsGesamt" = politikerAnalysedaten_userParteien$SUMME)

head(meistTweetende, 15)

#===============================================================================================================================================
#===============================================================================================================================================
#===============================================================================================================================================
# bezug auf mediendaten

rowSums(medienAnalysedaten_userBundesland[,-c(1:2)])

# user pro bundesländer anschauen, die die meisten tweets haben; ALLE TWEETS, auch die die keine kategorie haben
alleMedien %>%
  group_by(bundesland) %>%
  freq(user, order = "freq")

freq(alleMedien$bundesland)
medienAnalysedaten_userBundesland$SUMME <- rowSums(medienAnalysedaten_userBundesland[,-c(1:2)])

medienAnalysedaten_userBundesland <- medienAnalysedaten_userBundesland %>% group_by(bundesland) %>% mutate(bundeslandsumme = sum(SUMME))
View(medienAnalysedaten_userBundesland)
medienAnalysedaten_userBundesland <- medienAnalysedaten_userBundesland %>% arrange(desc(SUMME))

medienAnalysedaten_userBundesland$PROZENTUAL <- 0
(medienAnalysedaten_userBundesland$PROZENTUAL <- formel_prozentualisierung(medienAnalysedaten_userBundesland$SUMME) %>% round(5))

medienAnalysedaten_userAggregiert
freq(medienAnalysedaten_userBundesland$bundesland, order = "freq")

rowsum(medienAnalysedaten_userBundesland$SUMME, group = medienAnalysedaten_userBundesland$bundesland)

(overviewMedien <- as_tidytable(data.frame("bundesland"=unique(medienAnalysedaten_userBundesland$bundesland),
                                          "anzahlAkteure"=c(28, 25, 15, 5, 4, 2, 12, 2, 31, 32, 6, 3, 7, 5, 7, 5, 94),
                                          "absoluteTweets"=c(124884, 123856, 104312, 16140, 13388, 54022, 48438, 6520,
                                                             146864, 175448, 19914, 9744, 25547, 31894, 19951, 19099, 432092),
                                          "klassifiziertAbs"=c(41476, 53538, 44091, 7722, 5720, 14273, 17455, 3387,
                                                               46799, 62618, 9165, 4673, 12087, 9016, 9689, 6993, 258668),
                                          "gruppe"="Medien")))

sum(overviewMedien$absoluteTweets)
sum(overviewMedien$klassifiziertAbs)

(overviewMedien <- overviewMedien %>% mutate(tweetsProAkteurDurchschnitt = absoluteTweets / anzahlAkteure,
                                            anteiligGes = absoluteTweets / sum(absoluteTweets),
                                            klassifiziertProzent = klassifiziertAbs / absoluteTweets))

# alleMedien %>% group_by(bundesland) %>% sum(unique(alleMedien$user))

## versuch der sortierung nach erreichbarkeit, geschäftsmodell, normenWerte

medienAnalysedaten_discursivePower
freq(medienAnalysedaten_discursivePower$erreichbarkeit)
freq(medienAnalysedaten_discursivePower$normenWerte)
freq(medienAnalysedaten_discursivePower$geschäftsmodell)

medienAnalysedaten_discursivePower %>%
  filter(erreichbarkeit == 4)

# which.max(medienAnalysedaten_discursivePower)

## darstellung der gängigsten kombinationen von discursive power merkmalen
medienAnalysedaten_discursivePower[,c(3:5)] %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  arrange(desc(count))

medienAnalysedaten_discursivePower %>%
  group_by(geschäftsmodell, normenWerte, erreichbarkeit) %>%
  freq()

# datensatz: politiker, die im Untersuchungszeitraum am meisten getweetet haben
medienAnalysedaten_discursivePower$SUMME <- 0
(medienAnalysedaten_discursivePower$SUMME <- rowSums(medienAnalysedaten_discursivePower[,-c(1:5)]))

medienAnalysedaten_discursivePower$PROZENTUAL <- 0
(medienAnalysedaten_discursivePower$PROZENTUAL <- formel_prozentualisierung(medienAnalysedaten_discursivePower$SUMME) %>% round(4))
sum(medienAnalysedaten_discursivePower$PROZENTUAL)

medienAnalysedaten_discursivePower <- medienAnalysedaten_discursivePower %>% arrange(desc(SUMME))

# appendix 4
(öffentlichFinanzierte <- medienAnalysedaten_discursivePower %>%
  filter(geschäftsmodell == 1) %>%
  mutate(user = fct_reorder(user, desc(-SUMME))) %>%
  head(15) %>% #geht nur, wenn arrange vorher gemacht wurde, sonst x-beliebige beobachtungen!!!
  ggplot() +
  geom_bar(aes(y=user, x = SUMME, fill = bundesland), color = "black", stat = "identity") +
  scale_fill_manual(name="Herkunft", values = bundesländer) +
  scale_x_continuous(limits = c(0,15000), breaks = seq(0,15000, 5000), labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  xlab("") +
  ylab("") +
  ggtitle("Top 15 öffentlich finanzierte Nachrichtenmedien") +
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom", legend.text = element_text(face = "italic")))

(kommerziellFinanzierte <- medienAnalysedaten_discursivePower %>%
  filter(geschäftsmodell == 2) %>%
  mutate(user = fct_reorder(user, desc(-SUMME))) %>%
  head(15) %>%
  ggplot() +
  geom_bar(aes(y=user, x = SUMME, fill = bundesland), color = "black", stat = "identity") +
  scale_fill_manual(name="Herkunft", values = bundesländer) +
  scale_x_continuous(limits = c(0,25000), breaks = seq(0,25000, 5000), labels = scales::label_number(big.mark = ".")) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  xlab("") +
  ylab("") +
  ggtitle("Top 15 kommerziell finanzierte Nachrichtenmedien") +
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom", legend.text = element_text(face = "italic")))

(spendenFinanzierte <- medienAnalysedaten_discursivePower %>%
  filter(geschäftsmodell == 3) %>%
  mutate(user = fct_reorder(user, desc(-SUMME))) %>%
  head(15) %>%
  ggplot() +
  geom_bar(aes(y=user, x = PROZENTUAL, fill = bundesland), color = "black", stat = "identity") +
  scale_fill_manual(name="Herkunft", values = bundesländer) +
  scale_x_percent(limits = c(0, .02), breaks = seq(0, .02, .01)) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  xlab("") +
  ylab("") +
  ggtitle("Top 15 der über Spenden/Geldgeber finanzierten Nachrichtenmedien") +
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom", legend.text = element_text(face = "italic")))

ggarrange(legend = "right", ncol = 3, nrow = 1,
          öffentlichFinanzierte,
          kommerziellFinanzierte,
          spendenFinanzierte, labels = c("1", "2", "3"))

# top 15 akteure mit meisten tweets insg.

medienAnalysedaten_userBundesland <- medienAnalysedaten_userBundesland %>%
  ungroup() %>% arrange(desc(PROZENTUAL))

medienAnalysedaten_userBundesland %>%
    mutate(user = fct_reorder(user, desc(-PROZENTUAL))) %>%
    head(15) %>%
    ggplot() +
    geom_bar(aes(y=user, x = PROZENTUAL, fill = bundesland), color = "black", stat = "identity") +
    scale_fill_manual(name="Bundesland", values = optimum) +
    scale_x_percent(limits = c(0, .04), breaks = seq(0, .04, .01)) +
    theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
    xlab("") +
    ylab("") +
    ggtitle("Top 15 der aktivsten deutschen Nachrichtenmedien auf Twitter",
            subtitle = "Untersuchungszeitraum: Jan. - Okt. 2022")


view(as_tidytable(data.frame("user"=head(medienAnalysedaten_userBundesland$user, 15),
           "tweetsproacc"=head(medienAnalysedaten_userBundesland$SUMME, 15),
           "tweetsproacc_prozent"=head(medienAnalysedaten_userBundesland$PROZENTUAL, 15))))


# facets für geschäftsmodell
# scales free und independent TRUE sind wichtig!
medienAnalysedaten_discursivePower %>%
  mutate(user = fct_reorder(user, desc(-SUMME))) %>%
  head(15) %>%
  ggplot() +
  geom_bar(aes(y=user, x = SUMME, fill = bundesland), size = 1, stat = "identity") +
  #facet_wrap2(.~geschäftsmodell, scales = "free_y", nrow = 3) +
  scale_fill_manual(name="Bundesland", values = optimum) +
  #scale_colour_manual(name="Geschäftsmodell", values = coloursGeschäftsmodell) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Twitter-Deutschland: 15 aktivste Konten medialer Akteure",
          subtitle = "Inklusive Zuordnung zu jeweiligem Geschäftsmodell sowie Herkunft")

data.frame("user"=medienTweetsVoll$user,
           "bundesland"=medienTweetsVoll$bundesland,
           "anzTweets"=medienTweetsVoll$n,
           "prozTweets"=medienTweetsVoll$n/1372113)

(abb11_mostactive <- medienTweetsVoll %>%
  # arrange(desc(n)) %>%
  # head(20) %>%
  ggplot() +
  geom_bar(aes(y=reorder(user, n), x = n, fill = bundesland), linewidth = 1, stat = "identity") +
  geom_text(aes(y=reorder(user, n), x = n, label=geschäftsmodell), hjust = 0, nudge_x = 100) +
  scale_fill_manual(name="Herkunft", values = bundesländer) +
  #scale_colour_manual(name="Geschäftsmodell", values = coloursGeschäftsmodellAlternativ) +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(size = 9.5), legend.position = "right", legend.text = element_text(size = 9, face = "italic"),
        legend.box.spacing = unit(0.2, "cm"), legend.title = element_text(face = "bold"), axis.text.y = element_text(size = 9.5), title = element_text(size = 14)) +
  scale_x_continuous(limits = c(0,60000), breaks = seq(0,60000, 5000), labels = scales::label_number(big.mark = ".")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Twitter-Deutschland: 15 aktivste Konten medialer Akteure"))

coloursGeschäftsmodellAlternativ <- c("Öffentlich"="orangered3", "Kommerziell"="steelblue", "Spenden"="green")
#
# medienAnalysedaten_discursivePower$geschäftsmodell <- factor(medienAnalysedaten_discursivePower$geschäftsmodell,
#                                                              labels = c("öffentlich", "kommerziell", "Spenden"))
# freq(medienAnalysedaten_discursivePower$geschäftsmodell)













