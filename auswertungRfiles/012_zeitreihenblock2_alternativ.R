(zeitreiheUKR <- politikerProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=ukraine, colour = "Ukraine"), fun = sum) +
  scale_color_manual(name="Thema", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent() +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  geom_rect(data=highlightUKR, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="black",
            fill="#0000cdaa",
            alpha=0.5,
            inherit.aes = FALSE) +
  annotate(geom="point", x=as.Date("2022-02-21"), y=0.125, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Russischer\nEinmarsch", x=as.Date("2022-02-04"), y=0.112, color = "grey15", size=3.5) +
  annotate(geom="point", x=as.Date("2022-02-27"), y=0.07, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Zeitenwende-Rede", x=as.Date("2022-03-16"), y=0.084, color = "grey15", size=3.5) +
  annotate(geom="point", x=as.Date("2022-04-03"), y=0.033, size=10, shape=21, fill="transparent", color="#0000cd") +
  annotate(geom="text", label = "Massaker von\nBucha", x=as.Date("2022-04-15"), y=0.054, color = "grey15", size=3.5))

(zeitreiheENER <- politikerProzentualisiert %>% ggplot() +
  stat_summary(geom = "line", aes(x=weekAsDate, y=energie, colour = "Energie"), fun = sum) +
  scale_color_manual(name="", values = topicsDistinctColours) +
  scale_x_date(breaks = "month", labels = date_format(format = "%b, KW %W", locale = "de")) +
  xlab("") +
  scale_y_percent() +
  ylab("Anteil % Gesamtvolumen") +
  theme_ipsum(base_family = "TeX Gyre Heros", base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
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
  annotate(geom="text", label = "Lieferengpässe\nNord-Stream-1", x=as.Date("2022-08-10"), y=0.085, color = "grey15", size=4) +
  annotate(geom="point", x=as.Date("2022-09-04"), y=0.05, size=10, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Entlastungspaket 3", x=as.Date("2022-09-04"), y=0.03, color = "grey15", size=3) +
  annotate(geom="point", x=as.Date("2022-09-26"), y=0.06, size=10, shape=21, fill="transparent", color="#00ff00") +
  annotate(geom="text", label = "Scholz'\nDoppel-Wumms", x=as.Date("2022-10-01"), y=0.095, color = "grey15", size=3.5))

ggarrange(zeitreiheUKR, zeitreiheENER,
          nrow = 2, ncol = 1)
