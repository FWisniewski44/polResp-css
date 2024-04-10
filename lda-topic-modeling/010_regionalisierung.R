# ==============================================================================
# ============================ REGIONALE MEDIEN ================================
# ==============================================================================

# ==============================================================================
# ====================== Einteilung nach Bundesländern =========================
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

################################################################################

# if needed, set working directory
setwd(dir="~/Documents/uni/masterarbeit/methods/representative-speech/")

# komplette datensätze mit path angeglichen an wd
# orr <- read_csv("../../scraping/polResp-css/mergerDaten/orr.csv") %>% column_to_rownames(., var="...1")
# uregGes <- read_csv("../../scraping/polResp-css/mergerDaten/uregGes.csv") %>% column_to_rownames(., var="...1")
regioGes <- read_csv("../../scraping/polResp-css/mergerDaten/regioGes.csv") %>% column_to_rownames(., var="...1")
# digiGes <- read_csv("../../scraping/polResp-css/mergerDaten/digiGes.csv") %>% column_to_rownames(., var="...1")
# 
# gesamtMedien <- read_csv("../../scraping/polResp-css/mergerDaten/gesamtMedien.csv")

################################################################################

# Vorgehen => Liste mit Medien enthält Bundesland und Twitter-Acc.
# subset oder if verwenden: wenn Twitter-Acc. == BNN-BaNeuNa, dann BaWü

# Alternative: letzten Eintrag des letzten Mediums je Bundesland raussuchen
# Dann nach diesen rows filtern

################################################################################
######################## USERLISTEN NACH BUNDESLÄNDERN #########################
################################################################################

# baden-württemberg
userlistBaWü <- c("ab_nachrichten", "bkz_online", "BNN_BaNeuNa", "badischezeitung", "bzonline", "kreiszeitungbb", "ez_online",
                  "Gaeubote", "GT_Gmuend", "goodnews_stgt", "Pressehaus", "stimmeonline", "hz_nachrichten", "kanews",
                  "konradsblatt", "KontextWZ", "LKZ_Leonberg", "LKZ_online", "mainecho_de", "online_MM",
                  "MZ_nachrichten", "mittelbadische", "TagblattOnline", "pznews", "remszeitung", "geaonline",
                  "rn_nachrichten", "RNZonline", "SchwaePo", "Schwaebische", "Tagblatt", "schwarzwaelder",
                  "KN_Wochenblatt", "StN_News", "StZ_NEWS", "Suedkurier_News", "SWPde", "Der_Teckbote",
                  "wnoz", "zvw_redaktion", "ZAK_Redaktion")

# bayern
userlistBay <- c("Abendzeitung", "AZ_Allgaeu", "AZ_Augsburg", "GZ_AKTUELL", "BayerRundschau", "Staatszeitung", "bt_tagblatt",
                 "CTageblatt", "donaukurier", "ed_anzeiger", "EuroamSonntag", "Frankenpost", "fraenkischertag", "GermaniaPlus",
                 "pnp_grafenau", "hallo_muenchen", "heinrichsblatt", "HZOnline", "KreisboteOA", "mainpost", "MarktSpiegel",
                 "idowa", "MZ_Online", "mz_de", "merkur_de", "Wochenanzeiger", "NPCoburg", "onetz_de", "kurier_online",
                 "NN_Online", "NZ_Online", "heimatzeitungen", "ObermainTB", "pnp", "Pegnitz_Zeitung",
                 "pnp_plattling", "SaaleZeitung", "sonntagsblatt", "StaZ_Augsburg", "SuddtZeitung", "tzmuenchen",
                 "wochenblattnews")

# berlin
userlistBerlin <- c("BerlAbendblatt", "BERLINER_KURIER", "morgenpost", "berlinerwoche", "berlinerzeitung", "bzberlin",
                    "JuedischeOnline", "Junge_Freiheit", "Jungle_World", "ndaktuell", "pukzeitung", "tag_des_herrn",
                    "Tagesspiegel", "TspCheckpoint", "vorwaerts")

# brandenburg
userlistBrand <- c("lr_online", "maz_online", "mozde", "NCasnik", "PNN_de")

# bremen
userlistBremen <- c("nordseezeitung", "weserkurier", "Wuemme_Zeitung")

# hamburg
userlistHH <- c("bgzonline", "abendblatt", "mopo", "paz_ob")

# hessen
userlistHessen <- c("Kirchenzeitung", "boersenzeitung", "mittelhessende", "Echo_Online", "fnp_zeitung", "fuldaerzeitung",
                    "ANZEIGER_NEWS", "MitmachZeitung", "HA1725", "HNA_online", "KAgezwitscher", "Laredaktion", "mspnachrichten",
                    "opmarburg", "oberhessische", "Echo_Online", "Zwitschern_UA", "Werra_Rundschau", "WZ_Wetterau",
                    "wknachrichten")

# mecklenburg-vorpommern
userlistMP <- c("Nordkurier", "OZlive")

# niedersachsen
userlistNDS <- c("azgifhorn", "Harlinger1862", "BZRedaktion", "BorkumerZeitung", "BS_Zeitung", "cezett", "CN_Online", "Dewezet",
                 "Kreisblatt", "einmorgenpost", "ejzgezwitscher", "EmderZeitung", "ga_online", "goslarsche", "goetageblatt",
                 "GN_Nordhorn", "HAZ", "Die_Harke", "HarzKurier", "hinews", "Wochenblatt1791", "Kreiszeitung", "kzw_redaktion",
                 "lzgezwitscher", "neuepresse", "noz_de", "NEZ_Online", "nwzonline", "oz_online_de", "on_online_de", "pazpeine",
                 "RZ_Rheiderland", "Rundblick_NDS", "sn_online", "SZLZ1", "TAGEBLATTonline", "wz_net", "WZonline", "wazwolfsburg")

# nordrhein-westfalen
userlistNRW <- c("aachenerzeitung", "BA_Billerbeck", "blickaktuell", "BBV_Bocholt", "BorkenerZeitung", "DZ_Duelmen", "gabonn",
                 "DieGlocke", "handwerksblatt", "express24", "KreisblattHalle", "handelsblatt", "HellwegerNews", "ivz_aktuell",
                 "KevelaererBlatt", "KSTA", "KoelnischeR", "lzonline", "comeon_de", "MT_Online", "MZ_MUENSTER", "nwnews",
                 "rgaonline", "rponline", "RN_DORTMUND", "Sauerlandkurier", "SiegenerZeitung", "soesteranzeiger", "SGTageblatt",
                 "UnsereKirche", "UnsereZeit_UZ", "WAZ_Redaktion", "wznewsline", "westfalenblatt", "DasSauerland",
                 "WN_Redaktion", "WA_online", "WRundschau")

# rheinland-pfalz
userlistRLP <- c("aznachrichten", "PflzischerMerkur", "pz_online", "rheinpfalz", "RheinZeitung", "STagblatt",
                 "Volksfreund", "wznachrichten")

# saarland
userlistSaar <- c("szaktuell", "wo_regio_sb")

# sachsen
userlistSachsen <- c("dnn_online", "freie_presse", "LIZ_de", "LVZ", "saechsischeDE", "sonntagticker", "TorgauerZeitung")

# sachsen-anhalt
userlistSaAn <- c("AltmarkZeitung", "glaubeundheimat", "mzwebde", "Volksstimme", "volksstimme_md")

# schleswig-holstein
userlistSH <- c("BassesBlatt", "BoyensMedien", "Flensborg_Avis", "FTageblatt", "kn_online", "LN_Online", "der_reporter",
                "shz_de", "mywochenspiegel")

# thüringen
userlistThüringen <- c("freieswort", "OTZonline", "TAOnline", "TLZnews")

################################################################################
########################### OPERATIONEN DURCHFÜHREN ############################
################################################################################

# baden-württemberg
bawü <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistBaWü, "Baden-Württemberg", NA))
bawü <- bawü[complete.cases(bawü$bundesland), ]

### RVerkauf: Reichweite nach verkaufter Auflage
bawü <- bawü %>% mutate(RVerkauf = case_when(user == "BNN_BaNeuNa" ~ 99326,
                                             user == "badischezeitung" ~ 120183,
                                             user == "ez_online" ~ 22336,
                                             user == "Gaeubote" ~ 9967,
                                             user == "GT_Gmuend" ~ 10568,
                                             user == "Pressehaus" ~ 22258,
                                             user == "stimmeonline" ~ 68201,
                                             user == "kanews" ~ NA,
                                             user == "KontextWZ" ~ NA,
                                             user == "LKZ_Leonberg" ~ 16630,
                                             user == "LKZ_online" ~ 36033,
                                             user == "mainecho_de" ~ 58593,
                                             user == "online_MM" ~ 53697,
                                             user == "MZ_nachrichten" ~ 4395,
                                             user == "mittelbadische" ~ 36058,
                                             user == "pznews" ~ 28059,
                                             user == "remszeitung" ~ 11132,
                                             user == "RNZonline" ~ 68402,
                                             user == "SchwaePo" ~ 25614,
                                             user == "Schwaebische" ~ 135319,
                                             user == "Tagblatt" ~ 33907,
                                             user == "schwarzwaelder" ~ 98189,
                                             user == "StN_News" ~ 162861,
                                             user == "StZ_News" ~ 162861,
                                             user == "Suedkurier_News" ~ 101802,
                                             user == "SWPde" ~ 242191,
                                             user == "Der_Teckbote" ~ 12385,
                                             user == "wnoz" ~ 19053,
                                             user == "ZAK_Redaktion" ~ 18049))

### RVisits: Seitenaufrufe der Onlineauftritte
bawü <- bawü %>% mutate(RVisits = case_when(user == "BNN_BaNeuNa" ~ 2600000,
                                             user == "badischezeitung" ~ 4600000,
                                             user == "ez_online" ~ 515100,
                                             user == "Gaeubote" ~ 91800,
                                             user == "GT_Gmuend" ~ 162800,
                                             user == "Pressehaus" ~ 405400,
                                             user == "stimmeonline" ~ 1900000,
                                             user == "kanews" ~ 1200000,
                                             user == "KontextWZ" ~ 79800,
                                             user == "LKZ_Leonberg" ~ 77200,
                                             user == "LKZ_online" ~ 431400,
                                             user == "mainecho_de" ~ 1100000,
                                             user == "online_MM" ~ 1100000,
                                             user == "MZ_nachrichten" ~ 17300,
                                             user == "mittelbadische" ~ 693100,
                                             user == "pznews" ~ 1900000,
                                             user == "remszeitung" ~ 146100,
                                             user == "RNZonline" ~ 1200000,
                                             user == "SchwaePo" ~ 554800,
                                             user == "Schwaebische" ~ 3300000,
                                             user == "Tagblatt" ~ 1300000,
                                             user == "schwarzwaelder" ~ 3100000,
                                             user == "StN_News" ~ 7100000,
                                             user == "StZ_News" ~ 6200000,
                                             user == "Suedkurier_News" ~ 37000000,
                                             user == "SWPde" ~ 8500000,
                                             user == "Der_Teckbote" ~ 130800,
                                             user == "wnoz" ~ 101000,
                                             user == "ZAK_Redaktion" ~ 369200))

### Erreichbarkeit: Wertebereich für regionale Akteure => 2:4 (etablierte vs. alternative vs. individuelle)
bawü <- bawü %>% mutate(erreichbarkeit = case_when(user == "BNN_BaNeuNa" ~ 2,
                                            user == "badischezeitung" ~ 2,
                                            user == "ez_online" ~ 2,
                                            user == "Gaeubote" ~ 2,
                                            user == "GT_Gmuend" ~ 2,
                                            user == "Pressehaus" ~ 2,
                                            user == "stimmeonline" ~ 2,
                                            user == "kanews" ~ 3,
                                            user == "KontextWZ" ~ 3,
                                            user == "LKZ_Leonberg" ~ 2,
                                            user == "LKZ_online" ~ 2,
                                            user == "mainecho_de" ~ 2,
                                            user == "online_MM" ~ 2,
                                            user == "MZ_nachrichten" ~ 2,
                                            user == "mittelbadische" ~ 2,
                                            user == "pznews" ~ 2,
                                            user == "remszeitung" ~ 2,
                                            user == "RNZonline" ~ 2,
                                            user == "SchwaePo" ~ 2,
                                            user == "Schwaebische" ~ 2,
                                            user == "Tagblatt" ~ 2,
                                            user == "schwarzwaelder" ~ 2,
                                            user == "StN_News" ~ 2,
                                            user == "StZ_News" ~ 2,
                                            user == "Suedkurier_News" ~ 2,
                                            user == "SWPde" ~ 2,
                                            user == "Der_Teckbote" ~ 2,
                                            user == "wnoz" ~ 2,
                                            user == "ZAK_Redaktion" ~ 2))

### NormenWerte: Wertebereich => 1 = objektiv-ausbalanciert; 2 = parteiisch; 3 = marktgetrieben; 4 = NA
bawü <- bawü %>% mutate(normenWerte = case_when(user == "BNN_BaNeuNa" ~ 1,
                                                   user == "badischezeitung" ~ 1,
                                                   user == "ez_online" ~ 1,
                                                   user == "Gaeubote" ~ 1,
                                                   user == "GT_Gmuend" ~ 1,
                                                   user == "Pressehaus" ~ 1,
                                                   user == "stimmeonline" ~ 1,
                                                   user == "kanews" ~ 1,
                                                   user == "KontextWZ" ~ 2,
                                                   user == "LKZ_Leonberg" ~ 1,
                                                   user == "LKZ_online" ~ 1,
                                                   user == "mainecho_de" ~ 1,
                                                   user == "online_MM" ~ 1,
                                                   user == "MZ_nachrichten" ~ 1,
                                                   user == "mittelbadische" ~ 1,
                                                   user == "pznews" ~ 1,
                                                   user == "remszeitung" ~ 1,
                                                   user == "RNZonline" ~ 1,
                                                   user == "SchwaePo" ~ 1,
                                                   user == "Schwaebische" ~ 1,
                                                   user == "Tagblatt" ~ 1,
                                                   user == "schwarzwaelder" ~ 1,
                                                   user == "StN_News" ~ 1,
                                                   user == "StZ_News" ~ 1,
                                                   user == "Suedkurier_News" ~ 1,
                                                   user == "SWPde" ~ 1,
                                                   user == "Der_Teckbote" ~ 1,
                                                   user == "wnoz" ~ 1,
                                                   user == "ZAK_Redaktion" ~ 1))

### Geschäftsmodell: öffentlich (1) vs. kommerziell (2) vs. Spenden etc. (3)
bawü <- bawü %>% mutate(geschäftsmodell = case_when(user == "BNN_BaNeuNa" ~ 2,
                                                   user == "badischezeitung" ~ 2,
                                                   user == "ez_online" ~ 2,
                                                   user == "Gaeubote" ~ 2,
                                                   user == "GT_Gmuend" ~ 2,
                                                   user == "Pressehaus" ~ 2,
                                                   user == "stimmeonline" ~ 2,
                                                   user == "kanews" ~ 2,
                                                   user == "KontextWZ" ~ 3,
                                                   user == "LKZ_Leonberg" ~ 2,
                                                   user == "LKZ_online" ~ 2,
                                                   user == "mainecho_de" ~ 2,
                                                   user == "online_MM" ~ 2,
                                                   user == "MZ_nachrichten" ~ 2,
                                                   user == "mittelbadische" ~ 2,
                                                   user == "pznews" ~ 2,
                                                   user == "remszeitung" ~ 2,
                                                   user == "RNZonline" ~ 2,
                                                   user == "SchwaePo" ~ 2,
                                                   user == "Schwaebische" ~ 2,
                                                   user == "Tagblatt" ~ 2,
                                                   user == "schwarzwaelder" ~ 2,
                                                   user == "StN_News" ~ 2,
                                                   user == "StZ_News" ~ 2,
                                                   user == "Suedkurier_News" ~ 2,
                                                   user == "SWPde" ~ 2,
                                                   user == "Der_Teckbote" ~ 2,
                                                   user == "wnoz" ~ 2,
                                                   user == "ZAK_Redaktion" ~ 2))

################################################################################

# bayern
bayern <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistBay, "Bayern", NA))
bayern <- bayern[complete.cases(bayern$bundesland),]

### RVerkauf: Reichweite nach verkaufter Auflage // wenn keine angebbar, dann NA
bayern <- bayern %>% mutate(RVerkauf = case_when(user == "Abendzeitung" ~ 33022,
                                             user == "AZ_Allgaeu" ~ 83761,
                                             user == "AZ_Augsburg" ~ 211333,
                                             user == "GZ_AKTUELL" ~ 9665,
                                             user == "Staatszeitung" ~ 14363,
                                             user == "bt_tagblatt" ~ NA,
                                             user == "donaukurier" ~ 86858,
                                             user == "fraenkischertag" ~ 75601,
                                             user == "hallo_muenchen" ~ 654429,
                                             user == "HZOnline" ~ 5981,
                                             user == "mainpost" ~ 104807,
                                             user == "idowa" ~ 108516,
                                             user == "MZ_Online" ~ 12060,
                                             user == "mz_de" ~ 101560,
                                             user == "merkur_de" ~ 211857,
                                             user == "onetz_de" ~ 44050,
                                             user == "NN_Online" ~ 202620,
                                             user == "NZ_Online" ~ 162927,
                                             user == "heimatzeitungen" ~ 51988,
                                             user == "pnp" ~ 158805,
                                             user == "sonntagsblatt" ~ 2921,
                                             user == "tzmuenchen" ~ 81004,
                                             user == "wochenblattnews" ~ 993578))

### RVisits
bayern <- bayern %>% mutate(RVisits = case_when(user == "Abendzeitung" ~ 5300000,
                                                 user == "AZ_Allgaeu" ~ 1500000,
                                                 user == "AZ_Augsburg" ~ 6600000,
                                                 user == "GZ_AKTUELL" ~ 5000,
                                                 user == "Staatszeitung" ~ 34600,
                                                 user == "bt_tagblatt" ~ 292400,
                                                 user == "donaukurier" ~ 1500000,
                                                 user == "fraenkischertag" ~ 210500,
                                                 user == "hallo_muenchen" ~ 158100,
                                                 user == "HZOnline" ~ 1200000,
                                                 user == "mainpost" ~ 3000000,
                                                 user == "idowa" ~ 1600000,
                                                 user == "MZ_Online" ~ 6600000,
                                                 user == "mz_de" ~ 2400000,
                                                 user == "merkur_de" ~ 44100000,
                                                 user == "onetz_de" ~ 1700000,
                                                 user == "NN_Online" ~ 7800000,
                                                 user == "NZ_Online" ~ 7800000,
                                                 user == "heimatzeitungen" ~ 390900,
                                                 user == "pnp" ~ 4600000,
                                                 user == "sonntagsblatt" ~ 235000,
                                                 user == "tzmuenchen" ~ 24700000,
                                                 user == "wochenblattnews" ~ 351600))

### Erreichbarkeit => Wertebereich 2:4
bayern <- bayern %>% mutate(erreichbarkeit = case_when(user == "Abendzeitung" ~ 2,
                                                user == "AZ_Allgaeu" ~ 2,
                                                user == "AZ_Augsburg" ~ 2,
                                                user == "GZ_AKTUELL" ~ 3,
                                                user == "Staatszeitung" ~ 3,
                                                user == "bt_tagblatt" ~ 3,
                                                user == "donaukurier" ~ 2,
                                                user == "fraenkischertag" ~ 2,
                                                user == "hallo_muenchen" ~ 3,
                                                user == "HZOnline" ~ 2,
                                                user == "mainpost" ~ 2,
                                                user == "idowa" ~ 2,
                                                user == "MZ_Online" ~ 2,
                                                user == "mz_de" ~ 2,
                                                user == "merkur_de" ~ 2,
                                                user == "onetz_de" ~ 2,
                                                user == "NN_Online" ~ 2,
                                                user == "NZ_Online" ~ 2,
                                                user == "heimatzeitungen" ~ 2,
                                                user == "pnp" ~ 2,
                                                user == "sonntagsblatt" ~ 3,
                                                user == "tzmuenchen" ~ 2,
                                                user == "wochenblattnews" ~ 2))

# NormenWerte: objektiv (1) vs. parteiisch (2) vs. marktgetrieben (3)
bayern <- bayern %>% mutate(normenWerte = case_when(user == "Abendzeitung" ~ 3,
                                                       user == "AZ_Allgaeu" ~ 1,
                                                       user == "AZ_Augsburg" ~ 1,
                                                       user == "GZ_AKTUELL" ~ 2,
                                                       user == "Staatszeitung" ~ 2,
                                                       user == "bt_tagblatt" ~ 1,
                                                       user == "donaukurier" ~ 1,
                                                       user == "fraenkischertag" ~ 1,
                                                       user == "hallo_muenchen" ~ 3,
                                                       user == "HZOnline" ~ 1,
                                                       user == "mainpost" ~ 1,
                                                       user == "idowa" ~ 1,
                                                       user == "MZ_Online" ~ 1,
                                                       user == "mz_de" ~ 1,
                                                       user == "merkur_de" ~ 1,
                                                       user == "onetz_de" ~ 1,
                                                       user == "NN_Online" ~ 1,
                                                       user == "NZ_Online" ~ 1,
                                                       user == "heimatzeitungen" ~ 1,
                                                       user == "pnp" ~ 1,
                                                       user == "sonntagsblatt" ~ 2,
                                                       user == "tzmuenchen" ~ 3,
                                                       user == "wochenblattnews" ~ 1))

# Geschäftsmodell: öffentlich (1) vs. kommerziell (2) vs. Spenden etc. (3)
bayern <- bayern %>% mutate(geschäftsmodell = case_when(user == "Abendzeitung" ~ 2,
                                                    user == "AZ_Allgaeu" ~ 2,
                                                    user == "AZ_Augsburg" ~ 2,
                                                    user == "GZ_AKTUELL" ~ 2,
                                                    user == "Staatszeitung" ~ 1,
                                                    user == "bt_tagblatt" ~ 2,
                                                    user == "donaukurier" ~ 2,
                                                    user == "fraenkischertag" ~ 2,
                                                    user == "hallo_muenchen" ~ 2,
                                                    user == "HZOnline" ~ 2,
                                                    user == "mainpost" ~ 2,
                                                    user == "idowa" ~ 2,
                                                    user == "MZ_Online" ~ 2,
                                                    user == "mz_de" ~ 2,
                                                    user == "merkur_de" ~ 2,
                                                    user == "onetz_de" ~ 2,
                                                    user == "NN_Online" ~ 2,
                                                    user == "NZ_Online" ~ 2,
                                                    user == "heimatzeitungen" ~ 2,
                                                    user == "pnp" ~ 2,
                                                    user == "sonntagsblatt" ~ 2,
                                                    user == "tzmuenchen" ~ 2,
                                                    user == "wochenblattnews" ~ 2))

################################################################################

# berlin
berlin <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistBerlin, "Berlin", NA))
berlin <- berlin[complete.cases(berlin$bundesland),]

### RVerkauf: Reichweite nach verkaufter Auflage // wenn keine angebbar, dann NA
berlin <- berlin %>% mutate(RVerkauf = case_when(user == "BerlAbendblatt" ~ 1300070,
                                                 user == "BERLINER_KURIER" ~ 81202,
                                                 user == "morgenpost" ~ 42092,
                                                 user == "berlinerwoche" ~ 1140000,
                                                 user == "berlinerzeitung" ~ 103005,
                                                 user == "bzberlin" ~ 78933,
                                                 user == "JuedischeOnline" ~ 10700,
                                                 user == "Junge_Freiheit" ~ 26461,
                                                 user == "Jungle_World" ~ NA,
                                                 user == "ndaktuell" ~ 16608,
                                                 user == "pukzeitung" ~ 5000,
                                                 user == "tag_des_herrn" ~ 16657,
                                                 user == "Tagesspiegel" ~ 103652,
                                                 user == "TspCheckpoint" ~ NA,
                                                 user == "vorwaerts" ~ 330823))

# RVisits
berlin <- berlin %>% mutate(RVisits = case_when(user == "BerlAbendblatt" ~ 59500,
                                                 user == "BERLINER_KURIER" ~ 6300000,
                                                 user == "morgenpost" ~ 9900000,
                                                 user == "berlinerwoche" ~ 257700,
                                                 user == "berlinerzeitung" ~ 11900000,
                                                 user == "bzberlin" ~ 16200000,
                                                 user == "JuedischeOnline" ~ 347200,
                                                 user == "Junge_Freiheit" ~ 2900000,
                                                 user == "Jungle_World" ~ 183700,
                                                 user == "ndaktuell" ~ 866200,
                                                 user == "pukzeitung" ~ 5500,
                                                 user == "tag_des_herrn" ~ 25000,
                                                 user == "Tagesspiegel" ~ 24200000,
                                                 user == "TspCheckpoint" ~ NA,
                                                 user == "vorwaerts" ~ 100200))

# Erreichbarkeit: 2:4
berlin <- berlin %>% mutate(erreichbarkeit = case_when(user == "BerlAbendblatt" ~ 2,
                                                user == "BERLINER_KURIER" ~ 2,
                                                user == "morgenpost" ~ 2,
                                                user == "berlinerwoche" ~ 2,
                                                user == "berlinerzeitung" ~ 2,
                                                user == "bzberlin" ~ 2,
                                                user == "JuedischeOnline" ~ 2,
                                                user == "Junge_Freiheit" ~ 2,
                                                user == "Jungle_World" ~ 3,
                                                user == "ndaktuell" ~ 2,
                                                user == "pukzeitung" ~ 3,
                                                user == "tag_des_herrn" ~ 3,
                                                user == "Tagesspiegel" ~ 2,
                                                user == "TspCheckpoint" ~ 2,
                                                user == "vorwaerts" ~ 3))

# NormenWerte: objektiv (1) vs. parteiisch (2) vs. marktgetrieben (3)
berlin <- berlin %>% mutate(normenWerte = case_when(user == "BerlAbendblatt" ~ 2,
                                                       user == "BERLINER_KURIER" ~ 2,
                                                       user == "morgenpost" ~ 2,
                                                       user == "berlinerwoche" ~ 2,
                                                       user == "berlinerzeitung" ~ 2,
                                                       user == "bzberlin" ~ 2,
                                                       user == "JuedischeOnline" ~ 2,
                                                       user == "Junge_Freiheit" ~ 2,
                                                       user == "Jungle_World" ~ 3,
                                                       user == "ndaktuell" ~ 2,
                                                       user == "pukzeitung" ~ 3,
                                                       user == "tag_des_herrn" ~ 3,
                                                       user == "Tagesspiegel" ~ 2,
                                                       user == "TspCheckpoint" ~ 2,
                                                       user == "vorwaerts" ~ 3))


################################################################################

# brandenburg
brandenburg <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistBrand, "Brandenburg", NA))
brandenburg <- brandenburg[complete.cases(brandenburg$bundesland),]

################################################################################

# bremen
bremen <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistBremen, "Bremen", NA))
bremen <- bremen[complete.cases(bremen$bundesland),]

################################################################################

# hamburg
hamburg <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistHH, "Hamburg", NA))
hamburg <- hamburg[complete.cases(hamburg$bundesland),]

################################################################################

# hessen
hessen <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistHessen, "Hessen", NA))
hessen <- hessen[complete.cases(hessen$bundesland),]

################################################################################

# MeckPomm
meckPomm <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistMP, "Mecklenburg-Vorpommern", NA))
meckPomm <- meckPomm[complete.cases(meckPomm$bundesland),]

################################################################################

# niedersachsen
niedersachsen <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistNDS, "Niedersachsen", NA))
niedersachsen <- niedersachsen[complete.cases(niedersachsen$bundesland),]

################################################################################

# nordrhein-westfalen
nrw <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistNRW, "Nordrhein-Westfalen", NA))
nrw <- nrw[complete.cases(nrw$bundesland),]

################################################################################

# rheinland-pfalz
rlp <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistRLP, "Rheinland-Pfalz", NA))
rlp <- rlp[complete.cases(rlp$bundesland),]

################################################################################

# saarland
saarland <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistSaar, "Saarland", NA))
saarland <- saarland[complete.cases(saarland$bundesland),]

################################################################################

# sachsen
sachsen <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistSachsen, "Sachsen", NA))
sachsen <- sachsen[complete.cases(sachsen$bundesland),]

################################################################################

# sachsen-anhalt
sachsen_anhalt <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistSaAn, "Sachsen-Anhalt", NA))
sachsen_anhalt <- sachsen_anhalt[complete.cases(sachsen_anhalt$bundesland),]

################################################################################

# schleswig-holstein
schleswig <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistSH, "Schleswig-Holstein", NA))
schleswig <- schleswig[complete.cases(schleswig$bundesland),]

################################################################################

# thüringen
thüringen <- regioGes %>% mutate(bundesland = ifelse(user %in% userlistThüringen, "Thüringen", NA))
thüringen <- thüringen[complete.cases(thüringen$bundesland),]

################################################################################
############################# ABSPEICHERN DER DATEN ############################
################################################################################

# als RData
save(bawü, file = "regionalisierung/bawü.RData")
save(bayern, file = "regionalisierung/bayern.RData")
save(berlin, file = "regionalisierung/berlin.RData")
save(brandenburg, file = "regionalisierung/brandenburg.RData")
save(bremen, file = "regionalisierung/bremen.RData")
save(hamburg, file = "regionalisierung/hamburg.RData")
save(hessen, file = "regionalisierung/hessen.RData")
save(meckPomm, file = "regionalisierung/meckPomm.RData")
save(niedersachsen, file = "regionalisierung/niedersachsen.RData")
save(nrw, file = "regionalisierung/nrw.RData")
save(rlp, file = "regionalisierung/rlp.RData")
save(saarland, file = "regionalisierung/saarland.RData")
save(sachsen, file = "regionalisierung/sachsen.RData")
save(sachsen_anhalt, file = "regionalisierung/sachsen_anhalt.RData")
save(schleswig, file = "regionalisierung/schleswig.RData")
save(thüringen, file = "regionalisierung/thüringen.RData")

### als csv
write_csv(bawü, file = "regionalisierung/bawü.csv")
write_csv(bayern, file = "regionalisierung/bayern.csv")
write_csv(berlin, file = "regionalisierung/berlin.csv")
write_csv(brandenburg, file = "regionalisierung/brandenburg.csv")
write_csv(bremen, file = "regionalisierung/bremen.csv")
write_csv(hamburg, file = "regionalisierung/hamburg.csv")
write_csv(hessen, file = "regionalisierung/hessen.csv")
write_csv(meckPomm, file = "regionalisierung/meckPomm.csv")
write_csv(niedersachsen, file = "regionalisierung/niedersachsen.csv")
write_csv(nrw, file = "regionalisierung/nrw.csv")
write_csv(rlp, file = "regionalisierung/rlp.csv")
write_csv(saarland, file = "regionalisierung/saarland.csv")
write_csv(sachsen, file = "regionalisierung/sachsen.csv")
write_csv(sachsen_anhalt, file = "regionalisierung/sachsen_anhalt.csv")
write_csv(schleswig, file = "regionalisierung/schleswig.csv")
write_csv(thüringen, file = "regionalisierung/thüringen.csv")

################################################################################
