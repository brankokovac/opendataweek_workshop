### Nedelja otvorenih podataka 2019 ###

### Kako unaprediti poslovanje koriscenjem otvorenih podataka ###
### Pokazni deo radionice analize podataka ###

### Branko Kovac, Managing Partner @ Logikka ###

#### 1. Ucitavanje biblioteka ####
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggmap)
library(leaflet)

#### 2. Ucitavanje podataka ####
saob_nezgode <- read.csv(file = "data/OPENDATA_SAOB_NEZGODE.csv", 
                         header = T, 
                         stringsAsFactors = F)

#### 3. Pocetna inspekcija podataka ####

# dimenzije tabele sa podacima
dim(saob_nezgode)


# osnovne informacije o podacima koji se nalaze u tabeli
summary(saob_nezgode)
glimpse(saob_nezgode)

# izbacivanje nepotrebnih kolona
saob_nezgode <- saob_nezgode %>% 
  select(-X_COL8, -X_COL9)

#### 4. Sta je moguce napraviti od podataka ####

# registracija na Google Maps radi pristupa eksternim podacima
register_google(key = "AIzaSyC7OxOIQPH4sQ7R2cqbtN8Do0qHag787gI")

# preuzimanje mape Beograda
bgd_map <- get_map(location = "Belgrade, Serbia", 
                   maptype = "roadmap")

# prostorna raspodela podataka
ggmap(bgd_map, extent = "device") + 
  geom_point(data = saob_nezgode,
             aes(x = WGS_X, y = WGS_Y), 
             size = .25, 
             color = "blue",
             alpha = .05) 

# preuzimanje mape za datu lokaciju - ulica Kneza Milosa 
kneza_milosa <- get_map(location = "Kneza Milosa, Belgrade",
                        maptype = "roadmap",
                        zoom = 16)

# prostorna raspodela podataka sa gustinom
ggmap(kneza_milosa, extent = "device") + 
  geom_density2d(data = saob_nezgode,
                 aes(x = WGS_X, y = WGS_Y),
                 size = .1) +
  stat_density2d(data = saob_nezgode,
                 aes(x = WGS_X, y = WGS_Y, 
                     fill = ..level.., 
                     alpha = ..level..), 
                 size = 0.01, 
                 bins = 16, 
                 geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "blue") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) + 
  theme(legend.position="none")

#### Kratka analiza pojedinih kolona ####

# koji su ishodi nezgoda?
saob_nezgode %>% 
  distinct(VRSTA_NEZ)

# ucestalost nezgoda prema ishodu
saob_nezgode %>% 
  group_by(VRSTA_NEZ) %>% 
  summarise(ukupno = n())

# ucestalost nezgoda prema ishodu - vizuelno
ggplot(saob_nezgode) + 
  aes(x = VRSTA_NEZ) + 
  geom_bar()

# koji su tipovi nezgoda?
saob_nezgode %>% 
  distinct(NAZIV_TIP)

saob_nezgode %>% 
  group_by(NAZIV_TIP) %>% 
  summarise(ukupno = n())

# ucestalost nezgoda prema ishodu i tipu
saob_nezgode %>% 
  group_by(VRSTA_NEZ, NAZIV_TIP) %>% 
  summarise(ukupno = n())

#### 5. Priprema podataka ####

# kreiranje posebnih kolona za datum i vreme
saob_nezgode <- separate(data = saob_nezgode,
                    col = VREME_NEZ,
                    into = c("datum", "vreme"),
                    sep = ",")

# kreiranje posebnih kolona za dan, mesec i godinu
saob_nezgode <- separate(data = saob_nezgode,
                    col = datum,
                    into = c("dan", "mesec", "godina"),
                    sep = "\\.")

# kreiranje posebnih kolona za sat i minut
saob_nezgode <- separate(data = saob_nezgode,
                    col = vreme,
                    into = c("sat", "minut"),
                    sep = ":")

# verifikacija podataka - izgleda da pojedini podaci imaju gresku! :)
saob_nezgode %>% 
  group_by(godina, mesec) %>% 
  summarise(ukupno = n())

# azuriranje podataka cije vrednosti nisu regularne
saob_nezgode <- saob_nezgode %>% 
  mutate(godina = "2015")

# kreiranje kategorickih varijabli za ishod i tip nezgode
saob_nezgode <- saob_nezgode %>% 
  mutate(ishod = factor(VRSTA_NEZ, labels = c("steta", "smrt", "povreda")), 
         tip_nezgode = factor(NAZIV_TIP, labels = c("nema", "dva_voz_skret_prelaz", 
                                                    "dva_voz_bez_skret", "jedno_voz", 
                                                    "park_voz", "pesak")))

# uklanjanje sada nepotrebih kolona
saob_nezgode <- saob_nezgode %>%
  select(-VRSTA_NEZ, -NAZIV_TIP)

# promena naziva odredjenih kolona
saob_nezgode <- saob_nezgode %>% 
  rename(lat = WGS_Y, lon = WGS_X)

# koordinate za Beograd - 44°49′N 20°28′E
range(saob_nezgode$lon)
range(saob_nezgode$lat)

# filtriranje podataka da sadrze samo one redove koji "pripadaju" Beogradu
# (prilicno amaterski pristup)
saob_nezgode <- saob_nezgode %>% 
  filter(lon < mean(lon) + 3*sd(lon), 
         lon > mean(lon) -3*sd(lon), 
         lat < mean(lat) + 3*sd(lat), 
         lat > mean(lat) - 3*sd(lat))

# sortiranje podataka prema kolonama
saob_nezgode <- saob_nezgode %>%
  arrange(dan, mesec, sat, minut)

#### 6. Eksplorativna analiza podataka ####


# manipulacija nad podacima se cesto nastavlja i tokom eksplorativne analize
# dodavanje kolone koja predstavlja opis nezgode
saob_nezgode <- saob_nezgode %>% 
  mutate(opis = paste(
    "<b>Datum: </b>", paste(dan, mesec, godina, sep = "-"), "<br>",
    "<b>Vreme: </b>", paste(sat, minut, sep = ":"), "<br>",
    "<b>Ishod: </b>", ishod,
    sep = ""))

# interaktivna mapa uz pomoc paketa {leaflet}
inter_mapa <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = saob_nezgode$lon,
             lat= saob_nezgode$lat,
             popup = saob_nezgode$opis, 
             clusterOptions = markerClusterOptions())

inter_mapa

# procenti nezgoda prema ishodu
saob_nezgode %>% 
  group_by(ishod) %>% 
  summarise(procenat = n()/nrow(saob_nezgode))

# broj nezgoda mesecno
nezg_mesecno <- saob_nezgode %>%
  group_by(mesec) %>%
  summarise(br_nezgoda = as.numeric(n()))

# broj nezgoda po mesecu i ishodu
ggplot(saob_nezgode) + 
  aes(x = as.factor(mesec), fill = ishod) + 
  geom_bar(position = "dodge")

# broj nezgoda prema tipu nezgode - vizuelno
saob_nezgode %>% 
  filter(tip_nezgode != "nema") %>% 
  ggplot(.) + 
  aes(x = tip_nezgode) + 
  geom_bar()

# broj nezgoda po satu
nezg_sat <- saob_nezgode %>%
  group_by(sat) %>%
  summarise(ukupno = n()) %>% 
  mutate(sat = as.numeric(sat))

# broj nezgoda po satu - vizuelno
ggplot(nezg_sat) +
  aes(y = ukupno, x = sat) + 
  geom_line(color = "blue", size = .5) + 
  geom_point(color = "blue", size = 1.5) + 
  geom_point(color = "white", size = 1) + 
  scale_x_continuous(breaks = nezg_sat$sat - 0.5,
                   labels = nezg_sat$sat) +
  ggtitle("Broj saobracajnih nezgoda po satu") + 
  xlab("Sat") + 
  ylab("Ukupno nezgoda")
  theme_bw() +
  theme(plot.title = element_text(size = 11))

# kada se desavaju nesrece sa smrtnim ishodom?
smrtne_nezgode <- saob_nezgode %>% 
  filter(ishod == "smrt") %>% 
  group_by(sat) %>% 
  summarise(ukupno = n())

smrtne_nezgode

# distribucija nezgoda po satu
nezg_sat_ishod <- saob_nezgode %>%
  group_by(sat, ishod) %>% 
  tally() %>%
  mutate(procenat = round(n/sum(n)*100, 2))

# procentualna distribucija nezgoda po satu - vizuelno
ggplot(nezg_sat_ishod) + 
  aes(x = sat, y = procenat, color = ishod, fill = ishod) +
  geom_bar(stat = "identity", position = "stack", width = .5) + 
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Blues") +
  theme_bw()

# procentualna distribucija nezgoda sa povredama po satu - vizuelno
nezg_sat_ishod %>%
  filter(ishod == "povreda") %>% 
  ggplot(.) + 
  aes(x = as.numeric(sat), y = procenat) +
  geom_path(color = "blue") +
  geom_point(size = 1.5, color = "blue") +
  geom_point(size = 1, color = "white") +
  xlab("Sat") + 
  ylim(20, 35) +
  ggtitle("Procenat nezgoda sa povredama po satu") +
  theme_bw()
