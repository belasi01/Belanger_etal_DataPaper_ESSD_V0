#
# This code was  written outside the github project and doesn't work properly 
# The code needs adjustment to make it work. 
#


library(ggplot2)
library(raster)
library(ggpubr)
library(lubridate)
library(tidyverse)
library(glue)
library(dbplyr)
library(dplyr)
library(DBI)
library(RSQLite)
library(assertr)
library(ggpmthemes) 
#devtools::install_github("PMassicotte/ggpmthemes")
library(data.table)
library(sf)
library(MBA)
library(ggisoband) # 
#devtools::install_github("clauswilke/ggisoband")
library(patchwork)
library(readxl)
library(rnaturalearth)

##### Set default ggplot2 font size and font family  ####
theme_set(theme_poppins(base_size = 10))
sf_use_s2(FALSE) 

##### Get shapefiles in ####

# https://www.diva-gis.org/datadown
Can_coast <-
  st_read("./data/CAN_adm/CAN_adm0.shp") %>%
  st_crop(c(
    xmin = -71,
    xmax = -61,
    ymin = 45,
    ymax = 51
  ))


ne_land <-
  ne_download(
    category = "physical",
    type = "land",
    returnclass = "sf",
    scale = "large"
  )

ne_river <-
  ne_download(
    category = "physical",
    type = "rivers_lake_centerlines",
    returnclass = "sf",
    scale = "large"
  )

arrow <- tibble(
  x = -68.2,
  xend = -68.5232,
  y = 48.3,
  yend = 48.4525
)

river_coords <- tibble(lon = c(-69), lat = c(46))



##### River network #####-----------------------------------------------------------

# https://open.canada.ca/data/en/dataset/448ec403-6635-456b-8ced-d3ac24143add
river_network <-
  st_read("./data/ghy_000c11a_e/ghy_000c11a_e.shp") %>%
  st_crop(c(
    xmin = -71,
    xmax = -61,
    ymin = 45,
    ymax = 51
  ))

##### Prepare bathymetry data #####-------------------------------------------------

bathy <- raster::raster(
  "./data/GEBCO_2020_16_Jun_2020_7b4c3b0d75c3/gebco_2020_n53.0_s43.0_w-71.5_e-55.5.tif"
) %>%
  # raster::sampleRegular(size = 1e4, asRaster = TRUE) %>%
  raster::rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3) %>%
  filter(between(y, 46, 50.5))

bathy_interpolated <- bathy %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z))

range(bathy_interpolated$xyz.est.z)

##### Read Station PMZA-Riki ####
watercolumn  <- read.csv("./data/watercolumn_synthesis_aquatel.csv") 
IML4 <- watercolumn %>% filter(project == "pmza", Rrs_inwater_unit == "cops")
station <- data.frame(station = "PMZA-RIKI",
                      longitude = mean(IML4$lon),
                      latitude = mean(IML4$lat),
                      Rrs.inwater.above = "in-water")

##### PLOT EGSL ####

p1 <- ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 20, color = NA
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-450, 0),
    oob = scales::squish,
    guide = guide_legend(
      label.position = "top",
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8),
      label.theme = element_text(size = 6),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(0.75, "cm"),
      byrow = TRUE,
      nrow = 1
    ),
    breaks = -seq(0, 450, by = 50)
  ) +
#  geom_sf(data = ne_land, size = 0.15) +
#  geom_sf(data = river_network, size = 0.01, color = "gray75") +
#  coord_sf(xlim = c(-69.8, -64), ylim = c(47.5, 50.5)) +
  geom_point(data = station, aes(
    x = longitude,
    y = latitude),    color = "#d8731f", show.legend = FALSE
  ) +
  geom_curve(
    data = arrow,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = -0.3,
    size = 0.25,
    arrow = arrow(length = unit(0.05, "inch"))
  ) +
  ggrepel::geom_text_repel(
    data = station,
    aes(x = longitude, y = latitude, label = station),
    size = 2.5,
    segment.size = 0.25
  ) +
  annotate(
    "text",
    y = 48.3,
    x = -67.8,
    label = "Rimouski",
    size = 5
  ) +
  annotate(
    "text",
    y = 49.5,
    x = -68,
    label = "Baie-Comeau",
    size = 5
  ) +
  geom_curve(
    aes(
      x = -68,
      xend = -68.1442,
      y = 49.5,
      yend = 49.2446,
    ),
    curvature = -0.3,
    size = 0.25,
    arrow = arrow(length = unit(0.05, "inch"))
  ) +
  annotate(
    "text",
    y = 50.4,
    x = -66,
    label = "Sept-Îles",
    size = 5
  ) +
  geom_curve(
    aes(
      x = -66,
      xend = -66.3901,
      y = 50.38,
      yend =  50.2309,
    ),
    curvature = -0.3,
    size = 0.25,
    arrow = arrow(length = unit(0.05, "inch"))
  ) +
  annotate(
    "text",
    y = 49.8,
    x = -65.5,
    label = "Gulf of\nSt Lawrence",
    size = 7,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 49,
    x = -67.75,
    label = "Lower estuary",
    angle=25,
    size = 5,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 47.85,
    x = -69.65,
    label = "Upper estu.",
    angle=50,
    size = 5,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 48.5,
    x = -65.8,
    label = "Gaspésie",
    size = 6,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 48.3,
    x = -69.72,
    label = "Saguenay\nriver",
    size = 3,
    fontface = "plain"
  ) +
  # annotate(
  #   "text",
  #   y = 50.5,
  #   x = -69.9,
  #   label = "a)",
  #   size = 5,
  #   fontface = "plain"
  # ) +
  annotate(
    "text",
    y = 48.8,
    x = -69.2,
    label = "b)",
    size = 4,
    fontface = "plain"
  ) +
  annotate( # Forestville range
    "rect",
    xmin = -69.15,
    xmax = -68.888,
    ymin = 48.6,
    ymax = 48.82,
    alpha = .3
  ) +
  annotate(
    "text",
    y = 49.25,
    x = -68.7,
    label = "c)",
    size = 4,
    fontface = "plain"
  ) +
  annotate( # WISE range
    "rect",
    xmin = -68.61,
    xmax = -68.01,
    ymin = 48.93,
    ymax = 49.29,
    alpha = .3
  ) +
  annotate(
    "text",
    y = 50.25,
    x = -66.75,
    label = "d)",
    size = 4,
    fontface = "plain"
  ) +
  annotate( # Chone-2 range
    "rect",
    xmin = -66.65,
    xmax = -66.,
    ymin = 50,
    ymax = 50.28,
    alpha = .3
  ) +
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.25,
    height = unit(0.1, "cm")
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.direction = "horizontal",
    legend.position = c(0.1, 0.08),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "#B9DDF1")
  ) +
  paletteer::scale_color_paletteer_d("ggsci::default_nejm")





#### Generate Inset maps ####

##### Forestville #####

# refine Bathymetry
bathy <- raster::raster(
  "/data/GEBCO_2020_16_Jun_2020_7b4c3b0d75c3/gebco_2020_n53.0_s43.0_w-71.5_e-55.5.tif"
) %>%
  # raster::sampleRegular(size = 1e4, asRaster = TRUE) %>%
  raster::rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3) %>%
  filter(between(y, 48.5, 48.9), between(x,-69.3, -68.))

bathy_interpolated <- bathy %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z))

FV.station <- fread("~/OneDrive - UQAR/data/MicroCASI_project/log/FV.station.csv")

##### plot  ####
p2 <- ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 20, color = NA
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-100, 0),
    oob = scales::squish,
    guide = guide_legend(
      label.position = "top",
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8),
      label.theme = element_text(size = 6),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(0.40, "cm"),
      byrow = TRUE,
      nrow = 1
    ),
    breaks = -seq(0, 100, by = 10)
  ) +
  geom_sf(data = Can_coast, size = 0.15) +
  geom_sf(data = river_network, size = 0.01, color = "gray75") +
  coord_sf(xlim = c(-69.15, -68.88), ylim = c(48.6, 48.82)) +
  geom_point(data = FV.station, aes(
  x = longitude,
  y = latitude,
  color = factor(Rrs.inwater.above)
  ), show.legend = FALSE) +
  ggspatial::annotation_scale(
  location = "br",
  width_hint = 0.25,
  height = unit(0.1, "cm")
  ) +
  annotate(
    "text",
    y = 48.72, 
    x = -69.11,
    label = "Forestville",
    size = 4,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 48.62, 
    x = -69.13,
    label = "Portneuf-\nsur-Mer",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 48.77, 
    x = -69.06,
    label = "Baie\nLaval",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 48.828, 
    x = -68.9,
    label = "Colombier",
    size = 3,
    fontface = "plain"
  ) +
  ggrepel::geom_text_repel(
    data = FV.station,
    aes(x = longitude, y = latitude, label = station),
    size = 2.5,
    segment.size = 0.25
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.direction = "horizontal",
    legend.position = c(0.05, 0.85),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "#B9DDF1")
  ) +
  paletteer::scale_color_paletteer_d("ggsci::default_nejm")

##### WISE-Man Map ####

wise.station <- data.table::fread("~/OneDrive - UQAR/data/WISEMan/Log/Stations_Rrs_info.csv")

# refine Bathymetry
bathy <- raster::raster(
  "/data/GEBCO_2020_16_Jun_2020_7b4c3b0d75c3/gebco_2020_n53.0_s43.0_w-71.5_e-55.5.tif"
) %>%
  # raster::sampleRegular(size = 1e4, asRaster = TRUE) %>%
  raster::rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3) %>%
  filter(between(y, 48.8, 49.35), between(x,-69., -67.9))

bathy_interpolated <- bathy %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z))


##### plot ####
p3 <- ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 20, color = NA
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-100, 0),
    oob = scales::squish,
    guide = guide_legend(
      label.position = "top",
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8),
      label.theme = element_text(size = 6),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(0.75, "cm"),
      byrow = TRUE,
      nrow = 1
    ),
    breaks = -seq(0, 100, by = 10)
  ) +
  geom_sf(data = Can_coast, size = 0.2) +
  geom_sf(data = river_network, size = 0.1, color = "grey75", fill = "transparent") +
  coord_sf(xlim = c(-68.7, -67.95), ylim = c(48.92, 49.26)) +
  geom_point(data = wise.station, aes(
    x = longitude,
    y = latitude,
    color = factor(Rrs.inwater.above)
  ), show.legend = FALSE) +
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.25,
    height = unit(0.1, "cm")
  ) +
   ggrepel::geom_text_repel(
     data = wise.station,
     aes(x = longitude, y = latitude, label = station),
     size = 2.5,
     segment.size = 0.25
   ) +
  annotate(
    "text",
    y = 49.24,
    x = -68.18,
    label = "Baie-\nComeau",
    size = 4,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 49.22,
    x = -68.35,
    label = "Manicouagan\nRiver",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 49.18,
    x = -68.45,
    label = "Aux-Outardes\nRiver",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 48.93,
    x = -68.68,
    label = "Betsiamite\nRiver",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 49.16,
    x = -68.31,
    label = "Manicouagan\nPeninsula",
    size = 4,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 49.09,
    x = -68.53,
    label = "Ragueneau",
    size = 4,
    fontface = "plain"
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.direction = "horizontal",
    legend.position = c(0.05, 0.9),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "#B9DDF1")
  ) +
  paletteer::scale_color_paletteer_d("ggsci::default_nejm")


##### CHONe-2 map ####

chone.station <- fread("~/OneDrive - UQAR/data/CHOne-2-BSI/L2/csv_database/file02.station.csv")
#names(chone.station) <- c("station", "longitude", "latitude", "depth")
# remove stations without optics
chone.station <- chone.station[!is.na(chone.station$Rrs.instrument),]
ix  <- which(str_detect(chone.station$station, "FR-"))
chone.station <- chone.station[-ix,]
  
  


# refine Bathymetry
bathy <- raster::raster(
  "/data/GEBCO_2020_16_Jun_2020_7b4c3b0d75c3/gebco_2020_n53.0_s43.0_w-71.5_e-55.5.tif"
) %>%
  # raster::sampleRegular(size = 1e4, asRaster = TRUE) %>%
  raster::rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3) %>%
  filter(between(y, 49.9, 50.3), between(x,-67, -65.5))

bathy_interpolated <- bathy %>%
  mba.surf(no.X = 500, no.Y = 500, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z))


##### plot #####

p4 <- ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 20, color = NA
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-200, 0),
    oob = scales::squish,
    guide = guide_legend(
      label.position = "top",
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8),
      label.theme = element_text(size = 6),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(0.75, "cm"),
      byrow = TRUE,
      nrow = 1
    ),
    breaks = -seq(0, 200, by = 20)
  ) +
  geom_sf(data = Can_coast, size = 0.2) +
  geom_sf(data = river_network, size = 0.1, color = "grey75", fill = "lightblue") +
  coord_sf(xlim = c(-66.65, -66), ylim = c(50, 50.28)) +
   geom_point(data = chone.station, aes(
     x = longitude,
     y = latitude,
     color = factor(Rrs.inwater.above)
   ), show.legend = FALSE)+#TRUE) + labs(colour = "AOPs type")+
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.25,
    height = unit(0.1, "cm")
  ) +
   ggrepel::geom_text_repel(
     data = chone.station,
     aes(x = longitude, y = latitude, label = station),
     size = 2.5,
     segment.size = 0.25
   ) +
  annotate(
    "text",
    y = 50.22,
    x = -66.35,
    label = "Sept-\nÎles",
    size = 4,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.275,
    x = -66.45,
    label = "Rapid riv.",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.222,
    x = -66.1,
    label = "Moisie\nRiver",
    size = 4,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.17,
    x = -66.62,
    label = "Ste-Margerite\nRiver",
    size = 4,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.22,
    x = -66.55,
    label = "Hall r.",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.25,
    x = -66.38,
    label = "Foins r.\n\nPoste r.",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.15,
    x = -66.46,
    label = "Pointe\nnoire",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.107,
    x = -66.4,
    label = "Manowin",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.09,
    x = -66.39,
    label = "Corrosol",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.16,
    x = -66.36,
    label = "G.\nBasque",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.135,
    x = -66.355,
    label = "P.\nBasque",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.145,
    x = -66.28,
    label = "G. Boule",
    size = 3,
    fontface = "plain"
  ) +
  annotate(
    "text",
    y = 50.166,
    x = -66.3,
    label = "P.\nBoule",
    size = 3,
    fontface = "plain"
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.direction = "horizontal",
    legend.position = c(0.4, 0.9),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "#B9DDF1")
  ) +
  paletteer::scale_color_paletteer_d("ggsci::default_nejm")

##### Save maps in png ####
ggarrange(p1,p2,labels=c("(a)", "(b)"), ncol = 2, widths = c(3,2))
ggsave("~/Google Drive/Papier/InPreparation/OpticalDataPaper/Figures/Fig1a-b.png", width = 25, height = 15, units = "cm")

ggarrange(p3,labels=c("(c)"))
ggsave("~/Google Drive/Papier/InPreparation/OpticalDataPaper/Figures/Fig1c.png", width = 25, height = 20, units = "cm")

ggarrange(p4,labels=c("(d)"))
ggsave("~/Google Drive/Papier/InPreparation/OpticalDataPaper/Figures/Fig1d.png", width = 25, height = 20, units = "cm")
