library(sf)
library(mapsf)
library(readr)
library(dplyr)
library(classInt)

# L. Jégou, 12/07/2021
#Inspiration : Victor Alexandre https://gist.github.com/Valexandre/b360be386663ede9b5e6bad222b5404a

# Fond de carte des EPCI en 2019, source : https://www.datajoule.fr/explore/dataset/intercommunalites-2019-france/export/?disjunctive.reg_name&disjunctive.dep_name&disjunctive.epci_name
fond<-st_read("~/Downloads/intercommunalites-2019-france.geojson")

# Données AMELI, téléchargées pour plus de rapidité,accès possible via l'API ci-dessous
data<-read_delim("~/Downloads/donnees-de-vaccination-par-epci.csv", delim=";", escape_double = FALSE, col_types = cols(epci = col_character()), trim_ws = TRUE)

#data<-read_delim("https://datavaccin-covid.ameli.fr/explore/dataset/donnees-de-vaccination-par-epci/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", delim=";", escape_double = FALSE, col_types = cols(epci = col_character()), trim_ws = TRUE)

#on trie les semaines
dernieredate<-sort(data$semaine_injection,decreasing = T)[1]
data<-data%>%
  filter(semaine_injection==dernieredate)

#On ne garde que les valeurs synthétisées pour l'ensemble de la pop.
data<-data%>%
  filter(classe_age=='TOUT_AGE')

#on joint avec le fond des EPCI
epcivacc<-left_join(fond,data %>% dplyr::select(epci, taux_cumu_1_inj, taux_cumu_termine),by=c("epci_code"="epci")) 

#Filtre sur la France métro.
fr_metro<-epcivacc%>%
  filter(epci_area_code == "FXX")

#Reprojection
fr_metro<-st_transform(fr_metro, crs = 2154)

# Carte du taux de 1ère vaccination
# Config. export fichier PNG
mf_export(fr_metro, export = "png",
          width = 1600,
          res = 200,
          filename = "~/Downloads/epci_1inj_0621.png")

# Cartographie
mf_init(x=fr_metro, theme = "agolalight")

# plot Tx vacc 1 inj cumulé
mf_map(
  x = fr_metro, 
  var = "taux_cumu_1_inj",
  type = "choro",
  breaks = "jenks",
  nbreaks = 5,
  pal = "Emrld",
  border = "white", 
  lwd = 0.01,
  leg_pos = "topright", 
  leg_title = "en %"
) 

# Habillage par les départements
depts<-st_read("~/Downloads/depts.geojson")
vide <- rgb(0, 0, 255, max = 255, alpha = 0, names = "vide")
mf_base(depts, col = vide, border="white", lwd=0.5, add=TRUE)

# layout
mf_layout(title = "Taux cumulé de première injection - 21/06/2021", 
          credits = paste0("Sources: AMELI, IGN, 2021\n",
                           "mapsf ", 
                           packageVersion("mapsf")),
          arrow = TRUE)

dev.off()

# Carte du taux de vaccination totale
# Config. export
mf_export(fr_metro, export = "png",
          width = 1600,
          res = 200,
          filename = "~/Downloads/epci_complete_0621.png")

# Cartographie
mf_init(x=fr_metro, theme = "agolalight")

# plot Tx vacc 1 inj cumulé
mf_map(
  x = fr_metro, 
  var = "taux_cumu_termine",
  type = "choro",
  breaks = "jenks",
  nbreaks = 5,
  pal = "Emrld",
  border = "white", 
  lwd = 0.01,
  leg_pos = "topright", 
  leg_title = "en %"
) 

# Habillage par les départements
mf_base(depts, col = vide, border="white", lwd=0.5, add=TRUE)

# layout
mf_layout(title = "Taux cumulé de vaccination complète - 21/06/2021", 
          credits = paste0("Sources: AMELI, IGN, 2021\n",
                           "mapsf ", 
                           packageVersion("mapsf")),
          arrow = TRUE)

dev.off()

# Carte de la médiane des revenus par UC, INSEE 2017
datarev<-read_delim("~/Downloads/revmed_epci_2017_insee.csv", delim=";", escape_double = FALSE, col_types = cols("Code" = col_character()), na = c("", "NA"), trim_ws = TRUE)

#on renomm la colonne pour plus de simplicité
names(datarev)[3] <- "medrev2017"

#on joint avec le spatial pour les EPCI
fr_metro<-left_join(fr_metro,datarev %>% dplyr::select("Code", "medrev2017"),by=c("epci_code"="Code"))

# Config. export
mf_export(fr_metro, export = "png",
          width = 1600,
          res = 200,
          filename = "~/Downloads/epci_revmed_2017.png")

# Cartographie
mf_init(x=fr_metro, theme = "agolalight")

# Discrétisation
medrev_n<-strtoi(na.omit(fr_metro$medrev2017))
brks<-classIntervals(medrev_n, "jenks", n=5)

# Palette inversée
invPal<-hcl.colors(5, palette = "Emrld", rev = TRUE)

# plot Tx vacc 1 inj cumulé
mf_map(
  x = fr_metro, 
  var = "medrev2017",
  type = "choro",
  breaks = brks$brks,
  pal = invPal,
  border = "white", 
  lwd = 0.01,
  leg_pos = "topright", 
  leg_title = "en %"
) 

# Habillage par les départements
mf_base(depts, col = vide, border="white", lwd=0.5, add=TRUE)

# layout
mf_layout(title = "Médiane du revenu disponible par UC 2017", 
          credits = paste0("Sources: INSEE, IGN, 2021\n",
                           "mapsf ", 
                           packageVersion("mapsf")),
          arrow = TRUE)

dev.off()