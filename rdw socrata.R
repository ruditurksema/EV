#RDW

#devtools::install_github("Chicago/RSocrata")
library(RSocrata)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(streamgraph)
library(ggridges)

options(scipen = 999)

RDW <- read.socrata("https://opendata.rdw.nl/resource/w4rt-e856.json")

# transformatie

RDW$datum_tenaamstelling_2 <- as.Date(RDW$datum_tenaamstelling, format = "%d/%m/%Y")
RDW$jaar <- as.factor(year(RDW$datum_tenaamstelling_2))
RDW$year <- year(RDW$datum_tenaamstelling_2)
RDW$prijs <- as.numeric(RDW$catalogusprijs)
RDW$vermogen <- as.numeric(RDW$vermogen_massarijklaar)
RDW$gewicht <- as.numeric(RDW$massa_rijklaar)
RDW$merk <- as.factor(RDW$merk)
RDW$kleur <- as.factor(RDW$eerste_kleur)
RDW$kwartaal <- quarter(RDW$datum_tenaamstelling_2, with_year = T)
RDW$maand <- month(RDW$datum_tenaamstelling_2)
RDW$maand_jaar <- as.factor(format(as.Date(RDW$datum_tenaamstelling_2), "%Y-%m"))

# recode

RDW$merk[RDW$merk=="TESLA MOTORS"] <- "TESLA"            

# density

RDW %>%
  filter(jaar > 2010) %>%
  ggplot(aes(datum_tenaamstelling_2)) + geom_density() + theme_minimal()

RDW %>%
  filter(jaar > 2010) %>%
  ggplot(aes(datum_tenaamstelling_2)) + geom_histogram() + theme_minimal()

# aantal

RDW %>%
  group_by(merk) %>%
  summarise(n = n()) %>%
  filter(n > 10) %>%
  ggplot(aes(merk, n)) + geom_col() + coord_flip() + theme_minimal()

# test

RDW_test <- RDW %>%
  group_by(merk) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) 

#prijs

RDW %>%
  group_by(merk) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  filter(gemiddelde_prijs > 10000 & aantal > 10) %>%
  ggplot(aes(gemiddelde_prijs, fct_reorder(merk, gemiddelde_prijs))) + geom_point(color = "#2ca25f", aes(size = aantal)) + theme_minimal() +
  ggtitle("Gemiddelde catalogusprijs nu rijdende elektrische voertuigen, 21 maart 2018 (data: RDW)") 

#prijs

RDW %>%
  group_by(merk) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  filter(gemiddelde_prijs > 10000) %>%
  ggplot(aes(gemiddelde_prijs, fct_reorder(merk, gemiddelde_prijs))) + geom_point(color = "red", aes(size = aantal)) + theme_minimal() +
    ggtitle("Gemiddelde catalogusprijs nu rijdende elektrische voertuigen, 21 maart 2018 (data: RDW)") + 
    geom_label_repel(aes(label = aantal), color = 'steelblue',
                     box.padding = 0.35, point.padding = 0.5,
                     segment.color = 'grey50')
      

#prijs & vermogen

RDW %>%
  filter(jaar %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017") & vermogen < 1) %>%
  ggplot(aes(prijs, vermogen)) + geom_point(aes(color = jaar, size = gewicht)) + theme_minimal()

# jaar en merk

RDW %>%
  filter(merk %in% c("TESLA", "BMW I")) %>%
  group_by(merk, jaar) %>%
  summarise(aantal = n()) %>%
  ggplot(aes(jaar, aantal, fill = merk)) + geom_col(position = "dodge") + theme_minimal() +
  ggtitle("Aantal tenaamgestelde Teslas en BMW I's, per jaar (tot 21 maart 2018) (data: RDW)") 

# verdeling prijs

RDW %>%
  filter(merk %in% c("TESLA") & handelsbenaming %in% c("MODEL S", "MODEL X")) %>%
  ggplot(aes(catalogusprijs)) + geom_density()

# jaar en tesla

RDW %>%
  filter(merk %in% c("TESLA") & handelsbenaming %in% c("MODEL S", "MODEL X")) %>%
  group_by(merk, handelsbenaming, jaar) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  ggplot(aes(jaar, gemiddelde_prijs, group = handelsbenaming)) + geom_line(aes(color = handelsbenaming)) + theme_minimal() +
    geom_point(aes(jaar, gemiddelde_prijs, color = handelsbenaming, size = aantal)) +
  ggtitle("Gemiddelde prijs tenaamgestelde Teslas, per jaar (tot 21 maart 2018) (data: RDW)") 

# datum en tesla

RDW %>%
  filter(merk %in% c("TESLA") & handelsbenaming %in% c("MODEL S", "MODEL X")) %>%
  group_by(merk, handelsbenaming, datum_tenaamstelling_2) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  ggplot(aes(datum_tenaamstelling_2, gemiddelde_prijs, group = handelsbenaming)) + theme_minimal() +
  geom_point(aes(datum_tenaamstelling_2, gemiddelde_prijs, color = handelsbenaming), alpha = .5) +
  ggtitle("Prijs tenaamgestelde Teslas, 2013 tot 21 maart 2018 (data: RDW)") 

# datum en tesla

RDW %>%
  filter(merk %in% c("TESLA") & handelsbenaming %in% c("MODEL S", "MODEL X")) %>%
  group_by(merk, handelsbenaming, datum_tenaamstelling_2) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  ggplot(aes(datum_tenaamstelling_2, gemiddelde_prijs, group = handelsbenaming)) + theme_minimal() +
  geom_point(aes(datum_tenaamstelling_2, gemiddelde_prijs, color = handelsbenaming), alpha = .5) +
  ggtitle("Prijs tenaamgestelde Teslas, 2013 tot 21 maart 2018 (data: RDW)") 

# datum en tesla

RDW %>%
  filter(merk %in% c("TESLA") & handelsbenaming %in% c("MODEL S", "MODEL X")) %>%
  group_by(merk, handelsbenaming, datum_tenaamstelling_2) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  ggplot(aes(gemiddelde_prijs, aantal)) + theme_minimal() +
  geom_jitter(aes(color = handelsbenaming), alpha = .5) +
  ggtitle("Prijs tenaamgestelde Teslas, 2013 tot 21 maart 2018 (data: RDW)") 

# stream

RDW %>%
  filter(year > 2010) %>%
  group_by(kleur, year) %>%
  summarise(aantal = n()) %>%
  streamgraph("kleur", "aantal", "year", offset="zero") %>% 
  sg_axis_x("year", "%Y") %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="kleur: ")

# stream

RDW %>%
  filter(year > 2010 & kleur != "DIVERSEN" & kleur != "Niet geregistreerd") %>%
  group_by(kleur, datum_tenaamstelling_2) %>%
  summarise(aantal = n()) %>%
  ggplot(aes(datum_tenaamstelling_2, fill = kleur)) + geom_area(stat = "bin", position = fill) + theme_minimal() +
  scale_fill_manual(values=c("beige", "steelblue", "brown", "yellow", "grey", "green", "orange", "purple", "red", "pink", "white", "black"))

RDW %>%
  filter(year > 2010 & kleur != "DIVERSEN" & kleur != "Niet geregistreerd") %>%
  group_by(kleur, year) %>%
  summarise(aantal = n()) %>%
  ggplot(aes(year, aantal, fill = kleur)) + geom_col(position = "fill") + theme_minimal() +
  scale_fill_manual(values=c("beige", "steelblue", "#8c510a", "yellow", "grey", "#4daf4a", "#d95f02", "purple", "#e41a1c", "#fb9a99", "white", "black"))

#prijs

RDW %>%
  group_by(merk, kwartaal) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  filter(gemiddelde_prijs > 10000 & aantal > 10) %>%
  mutate(highlight = if_else(merk == "TESLA", T, F)) %>%
  ggplot(aes(kwartaal, gemiddelde_prijs, group = merk)) + geom_line(aes(color = highlight), size = .8) + 
  scale_color_manual(values = c('grey', 'red')) + scale_alpha_manual(values = c(.7, 1)) + 
  labs(title = 'Schommelingen in prijs nu rijdende elektrische voertuigen, Tesla versus de rest',
       subtitle = str_c("Gemiddelde catalogusprijs op moment van tenaamstelling",
                        "\nstand per 23 maart 2018 (data: RDW)")) +
  theme(legend.position = 'none', text = element_text(color = '#3A3A3A', family = 'sans'),
        plot.title = element_text(margin = margin(b = 10), face = 'bold', size = 20), 
        axis.title = element_text(), plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma_format())

#aantal

RDW %>%
  filter(year > 2014) %>%
  group_by(merk, maand_jaar) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  filter(gemiddelde_prijs > 10000 & aantal > 10) %>%
  mutate(highlight = if_else(merk == "TESLA", T, F)) %>%
  ggplot(aes(maand_jaar, aantal, group = merk)) + geom_line(aes(color = highlight), size = .8) + 
  scale_color_manual(values = c('grey', 'red')) + scale_alpha_manual(values = c(.7, 1)) + 
  labs(title = 'Aantal elektrische voertuigen, Tesla versus de rest',
       subtitle = str_c("Aantal EV's op moment van tenaamstelling",
                        "\nstand per 23 maart 2018 (data: RDW)")) +
  theme(legend.position = 'none', text = element_text(color = '#3A3A3A', family = 'sans'),
        plot.title = element_text(margin = margin(b = 10), face = 'bold', size = 20), 
        axis.title = element_text(), plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma_format()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# per jaar

RDW %>%
  group_by(merk, jaar) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  filter(gemiddelde_prijs > 10000 & aantal > 10) %>%
  mutate(highlight = if_else(merk == "TESLA", T, F)) %>%
  ggplot(aes(jaar, aantal, group = merk)) + geom_line(aes(color = highlight), size = .8) + 
  scale_color_manual(values = c('grey', 'red')) + scale_alpha_manual(values = c(.7, 1)) + 
  labs(title = 'Aantal elektrische voertuigen, Tesla versus de rest',
       subtitle = str_c("Aantal EV's op moment van tenaamstelling",
                        "\nstand per 23 maart 2018 (data: RDW)")) +
  theme(legend.position = 'none', text = element_text(color = '#3A3A3A', family = 'sans'),
        plot.title = element_text(margin = margin(b = 10), face = 'bold', size = 20), 
        axis.title = element_text(), plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma_format())

# ridges

RDW %>%
  filter(year > 2013) %>%
  group_by(merk, jaar, maand) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  mutate(highlight = if_else(merk == "TESLA", T, F)) %>%
  ggplot(aes(x = maand, y = jaar)) + 
  geom_density_ridges(scale = 1.5, aes(fill=factor(highlight)), alpha=.6) + theme_ridges(font_size = 9) +
  ggtitle("Aantal elektrische voertuigen verkocht")

# prijs, aantal

RDW %>%
  group_by(merk, year) %>%
  summarise(gemiddelde_prijs = mean(prijs, na.rm = T),
            aantal = n()) %>%
  mutate(highlight = if_else(merk == "TESLA", T, F)) %>%
  filter(year > 2010 & aantal > 10) %>%
  ggplot(aes(gemiddelde_prijs, aantal)) + geom_point(aes(color = highlight)) + theme_minimal()
  
#cumsum

TESLA_aantal <- RDW %>%
  filter(year > 2012 & merk == "TESLA") %>%
  group_by(merk, maand_jaar) %>%
  summarise(aantal = n())

TESLA_cumsum <- cumsum(TESLA_aantal$aantal)
TESLA_cumsum_df <- as.data.frame(TESLA_cumsum)
TESLA_cumsum_totaal <- cbind.data.frame(TESLA_aantal, TESLA_cumsum_df)
TESLA_cumsum_totaal <- rename(TESLA_cumsum_totaal, cumsum = TESLA_cumsum)

BMW_aantal <- RDW %>%
  filter(year > 2012 & merk == "BMW I") %>%
  group_by(merk, maand_jaar) %>%
  summarise(aantal = n())

BMW_cumsum <- cumsum(BMW_aantal$aantal)
BMW_cumsum_df <- as.data.frame(BMW_cumsum)
BMW_cumsum_totaal <- cbind.data.frame(BMW_aantal, BMW_cumsum_df)
BMW_cumsum_totaal <- rename(BMW_cumsum_totaal, cumsum = BMW_cumsum)

totaal <- rbind.data.frame(TESLA_cumsum_totaal, BMW_cumsum_totaal)

totaal %>%
  ggplot(aes(maand_jaar, cumsum, group = merk)) + geom_line(aes(color = merk), size = 1) +
  scale_color_manual(values = c('grey', 'red')) + scale_alpha_manual(values = c(.7, 1)) + 
  labs(title = 'Cumulatief aantal verkochte elektrische voertuigen',
       subtitle = str_c("Tesla (rood) versus BMW. Stand per 27 maart 2018 (data: RDW)")) +
  theme(legend.position = 'none', text = element_text(color = '#3A3A3A', family = 'sans'),
        plot.title = element_text(margin = margin(b = 10), face = 'bold', size = 20), 
        axis.title = element_text(), plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma_format()) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
