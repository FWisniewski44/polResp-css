################################################################################ libraries
library(tidytext)
library(readr)
library(summarytools)
library(rvest)
library(expss)
# library(hunspell)
library(lubridate)
# library(quanteda)
# library(tm)
# library(topicmodels)
# library(stopwords)
library(stringi)
#library(rvisidata)
library(qdap)
library(mgsub)
library(dirichletprocess)
library(igraph)
library(ggraph)
library(esquisse)
library(ggforce)
library(ggh4x)
library(plotly)
library(data.table)
library(tidytable)
library(gridExtra)
library(ggpubr)
library(scales)
library(TSstudio)
library(tidyverse)

################################################################################

library(knitr)
library(kableExtra)
library(DT)
# library(tm)
# library(topicmodels)
# library(reshape2)
# library(ggplot2)
# library(wordcloud)
# library(pals)
# library(SnowballC)
# library(lda)
# library(ldatuning)
library(flextable)
# activate klippy for copy-to-clipboard button
# klippy::klippy()

# emoji clipping
# library(emoji)
# library(textclean)

################################################################################

politiker_alles_days <- allePolitikerDay %>%
  reframe(.by = c(dateTime),
          mentionsCovid = sum(covid),
          mentionsUkraine = sum(ukraine),
          mentionsEnergie = sum(energie),
          mentionsSoziales = sum(soziales),mentionsVerteidigungspolitik = sum(verteidigungspolitik),
          mentionsPolitikNational = sum(politikNational),
          mentionsPolitikInternational = sum(politikInternational),
          mentionsPolitikEuropa = sum(politikEuropa),
          mentionsKlima = sum(klima),mentionsProtesteIran = sum(protesteIran),
          mentionsPolizistenmordKusel = sum(polizistenmordKusel),
          mentionsVerkehr = sum(verkehr),mentionsPluralismusMedien = sum(pluralismusMedien),
          mentionsZukunft = sum(zukunft),mentionsVerfassungsfeindlich = sum(verfassungsfeindlich),
          #partei = partei,
          #user = user,
          follower = followerAmount,
          einzug = einzug,
          replies = replies,
          retweets = retweets,
          likes = likes)

medien_alles_days <- alleMedienDay %>%
  reframe(.by = c(dateTime),
          mentionsCovid = sum(covid),
          mentionsUkraine = sum(ukraine),
          mentionsEnergie = sum(energie),
          mentionsSoziales = sum(soziales),mentionsVerteidigungspolitik = sum(verteidigungspolitik),
          mentionsPolitikNational = sum(politikNational),
          mentionsPolitikInternational = sum(politikInternational),
          mentionsPolitikEuropa = sum(politikEuropa),
          mentionsKlima = sum(klima),mentionsProtesteIran = sum(protesteIran),
          mentionsPolizistenmordKusel = sum(polizistenmordKusel),
          mentionsVerkehr = sum(verkehr),mentionsPluralismusMedien = sum(pluralismusMedien),
          mentionsZukunft = sum(zukunft),mentionsVerfassungsfeindlich = sum(verfassungsfeindlich),
          #partei = partei,
          user = user,
          follower = followerAmount,
          #einzug = einzug,
          replies = replies,
          retweets = retweets,
          likes = likes)


# alternativen datensatz erstellen zur abbildung der themen im zeitverlauf
alternativeMedien <- alleMedienDay %>% aggregate(covid ~ dateTime, sum) %>% as_tidytable(alternativeMedien)
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(ukraine ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(energie ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(soziales ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(verteidigungspolitik ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(polizistenmordKusel ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(politikEuropa ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(politikInternational ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(politikNational ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(klima ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(protesteIran ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(verkehr ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(pluralismusMedien ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(zukunft ~ dateTime, sum)))
alternativeMedien <- full_join(x = alternativeMedien, y = (alleMedienDay %>% aggregate(verfassungsfeindlich ~ dateTime, sum)))

alternativeMedien

# alternativen datensatz erstellen zur abbildung der themen im zeitverlauf
alternativePolitiker <- allePolitikerDay %>% aggregate(covid ~ dateTime, sum) %>% as_tidytable(alternativePolitiker)
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(ukraine ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(energie ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(soziales ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(verteidigungspolitik ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(polizistenmordKusel ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(politikEuropa ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(politikInternational ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(politikNational ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(klima ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(protesteIran ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(verkehr ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(pluralismusMedien ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(zukunft ~ dateTime, sum)))
alternativePolitiker <- full_join(x = alternativePolitiker, y = (allePolitikerDay %>% aggregate(verfassungsfeindlich ~ dateTime, sum)))

alternativePolitiker

# vars für wochen und monate schaffen
alternativePolitiker$weeks <- as.Date(cut(alternativePolitiker$dateTime, breaks = "week"))
alternativePolitiker$months <- as.Date(cut(alternativePolitiker$dateTime, breaks = "month"))

# plot wöchentlich
alternativePolitiker %>% ggplot() +
  stat_summary(aes(weeks, ukraine, colour = "Ukraine"), geom = "line", fun.y = sum) +
  stat_summary(aes(weeks, covid, colour = "Covid"), geom = "line", fun.y = sum) +
  stat_summary(aes(weeks, ukraine, colour = "Ukraine"), geom = "point", fun.y = sum) +
  stat_summary(aes(weeks, covid, colour = "Covid"), geom = "point", fun.y = sum) +
  scale_x_date(breaks = "1 week", labels = date_format(format = "KW %W", locale = "de")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(name="Thema", values = colours) +
  ggtitle(label = "Themenkonkurrenz, wöchentlich", subtitle = "Coronavirus vs. Ukrainekrieg") +
  xlab("Wochen") +
  ylab("Erwähnungen")

# plot monatlich
alternativePolitiker %>% ggplot() +
  stat_summary(aes(months, ukraine, colour = "Ukraine"), geom = "line", fun.y = sum) +
  stat_summary(aes(months, covid, colour = "Covid"), geom = "line", fun.y = sum) +
  stat_summary(aes(months, ukraine, colour = "Ukraine"), geom = "point", fun.y = sum) +
  stat_summary(aes(months, covid, colour = "Covid"), geom = "point", fun.y = sum) +
  scale_x_date(breaks = "1 month", labels = date_format(format = "%b %Y", locale = "de")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  scale_colour_manual(name="Thema", values = colours) +
  ggtitle(label = "Themenkonkurrenz, monatlich", subtitle = "Coronavirus vs. Ukrainekrieg") +
  xlab("Monate") +
  ylab("Erwähnungen")

# politiker vs medien bei covid und ukraine über die zeit (sowas ist auch wöchentlich/monatlich denkbar)
politikergraph <- politiker_alles_days %>% ggplot() +
  geom_point(aes(x=dateTime, y=mentionsCovid, colour="Covid")) +
  geom_line(aes(x=dateTime, y=mentionsCovid, colour="Covid")) +
  geom_point(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  geom_line(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, 50)) +
  ggtitle(label = "Alle Politiker, täglich gruppiert", subtitle = "Themen: Ukraine und Covid") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_colour_manual(name = "", values = colours)

mediengraph <- medien_alles_days %>% ggplot() +
  geom_point(aes(x=dateTime, y=mentionsCovid, colour="Covid")) +
  geom_line(aes(x=dateTime, y=mentionsCovid, colour="Covid")) +
  geom_point(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  geom_line(aes(x=dateTime, y=mentionsUkraine, colour="Ukraine")) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 200)) +
  ggtitle(label = "Alle Medien, täglich gruppiert", subtitle = "Themen: Ukraine und Covid") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_colour_manual(values = colours)

kombigraph <- ggarrange(politikergraph, mediengraph)
kombigraph








