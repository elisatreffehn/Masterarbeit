# ICESHEET LGM
# Gowan, E.J., Zhang, X., Khosravi, S. et al. 
#A new global ice sheet reconstruction for the past 80 000 years. 
#Nat Commun 12, 1199 (2021). https://doi.org/10.1038/s41467-021-21469-w

# Icesheet reconstruction with "ICESHEET 2.0", data from Pangea:
# https://doi.org/10.1594/PANGAEA.905800
# possible to plot in R??




#install.packages("gmt")
library(gmt)
#install.packages("ncdf") for this version of R not available!
#library(ncdf)

#### check if NetCDF tools are installed
#############################################################################
library(here)




getwd()
library(rgdal)
library(ggplot2)

# read shapefile

wmap <- readOGR(dsn="data/icesheet", layer="ne_110m_land") 

# keine Angabe des Dateityps nötig (z.B. .shp)
# convert to dataframe
wmap_df <- fortify(wmap)

# create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="#e6e8ed"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

# plot map
ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon() + 
  labs(title="World map (longlat)") + 
  coord_equal() + 
  theme_opts
# reproject from longlat to robinson
wmap_robin <- spTransform(wmap, CRS("+proj=robin"))
wmap_df_robin <- fortify(wmap_robin)
ggplot(wmap_df_robin, aes(long,lat, group=group)) + 
  geom_polygon() + 
  labs(title="World map (robinson)") + 
  coord_equal() +
  theme_opts
# show hole of Caspian Sea:
ggplot(wmap_df_robin, aes(long,lat, group=group, fill=hole)) +
  geom_polygon() + 
  labs(title="World map (robin)") +
  coord_equal() + 
  theme_opts+
  scale_fill_manual(values=c("#262626", "#e6e8ed"), guide="none") # change colors & remove legend
# add graticule and bounding box (longlat)
grat <- readOGR("data/icesheet", layer="ne_110m_graticules_15") 
grat_df <- fortify(grat)

bbox <- readOGR("data/icesheet", layer="ne_110m_wgs84_bounding_box") 
bbox_df<- fortify(bbox)

ggplot(bbox_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_df, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="World map + graticule (longlat)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend
# graticule (Robin)
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

ggplot(bbox_robin_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_df_robin, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="World map (Robinson)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend
# add country borders:

countries <- readOGR("data/icesheet", layer="ne_110m_admin_0_countries")
grat <- readOGR("data/icesheet", layer="ne_110m_graticules_15") 
grat_df <- fortify(grat)

bbox <- readOGR("data/icesheet", layer="ne_110m_wgs84_bounding_box") 
bbox_df<- fortify(bbox)

########################
########################
##########################
# Winkel tripel projection ######

countries_wintri <- spTransform(countries, CRS("+proj=wintri"))
bbox_wintri <- spTransform(bbox, CRS("+proj=wintri"))
wmap_wintri <- spTransform(wmap, CRS("+proj=wintri"))
grat_wintri <- spTransform(grat, CRS("+proj=wintri"))

countries_wintri_df<-fortify(countries_wintri)
bbox_wintri_df <- fortify(bbox_wintri)
wmap_wintri_df <- fortify(wmap_wintri)
grat_wintri_df <- fortify(grat_wintri)


winkel.triple <- ggplot(bbox_wintri_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_wintri_df, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_wintri_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="World map (Winkel Triple)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none")
# without graticule:
ggplot(bbox_wintri_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_wintri_df, aes(long,lat, group=group, fill=hole)) + 
  labs(title="World map (Winkel Triple)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none")
# without bounding box:
ggplot(wmap_wintri_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_wintri_df, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_wintri_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="World map (Winkel Triple)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none")


ggsave("winkel.triple.withoutgrat.png", width=12.5, height=8.25, dpi=72)

# add data points:
library(tidyverse)
primigenius <- read.csv("data/Mammuthus_primigenius.csv", sep = ";")
columbi <-  read.csv("data/Mammuthus_columbi.csv", sep = ";")
meridionalis <-  read.csv("data/Mammuthus_meridionalis.csv", sep = ";")
trogontherii <-  read.csv("data/Mammuthus_trogontherii.csv", sep = ";")
recki <-  read.csv("data/Palaeoloxodon_recki.csv", sep = ";")
namadicus <-  read.csv("data/Palaeoloxodon_namadicus.csv", sep = ";")
antiquus <-  read.csv("data/Palaeoloxodon_antiquus_Charlotte.csv", sep = ";")

all.species <- bind_rows(primigenius,columbi,meridionalis,trogontherii,
                         recki, namadicus, antiquus) 

all.species %>% 
  select(SPECIES, LONG, LAT) %>%
  head() 

world_map <- map_data("world")
ggplot() +
  geom_map(
    data = world_map, map = world_map,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = all.species,
    aes(LONG, LAT, color=SPECIES),
    alpha = 0.7
  ) +
  scale_colour_brewer(palette = "Set1")+
  labs(title="All fossil locations of all seven species")+
  theme(legend.title = element_blank(), legend.position = c(0.13,0.35),
        legend.text = element_text(face="italic"))
#all.species.df <- fortify(all.species)

ggplot(
  bbox_wintri_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_wintri_df, aes(long,lat, group=group, fill=hole)) +
  geom_point(
    data = all.species,
    aes(LONG, LAT, color=SPECIES,group=NULL, fill=NULL),
    alpha = 0.7
  )+
  scale_colour_brewer(palette = "Set1")+
  labs(title="All fossil data of all species (Winkel Triple)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none")


