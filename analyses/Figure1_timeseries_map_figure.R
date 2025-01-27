#Joshua G. Smith; jossmith@mbayaq.org

rm(list=ls())

######
######
#required packages
librarian::shelf(tidyverse, sf, raster, shiny, tmap, terra, tidyterra, RColorBrewer, ggplot2)

#set directories 
basedir <- "/Volumes/seaotterdb$/kelp_recovery/data"
localdir <- "/Users/jossmith/Documents/Data/landsat"
output <- here::here("output")

#read timeseries
timeseries <- read_csv(file.path(output, "timeseries_data.csv")) 

#read state
ca_counties_orig <- st_read(file.path(basedir, "gis_data/raw/ca_county_boundaries/s7vc7n.shp")) 

#read landsat raw
cen_landsat <- st_read(file.path(localdir, "/processed/ca_climate/landsat_central_coast.geojson"))
islands_landsat <- st_read(file.path(localdir, "/processed/ca_climate/landsat_north_islands.geojson"))


# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")

# Filter for California counties
ca_counties <- usa[usa$name == "California", ]

################################################################################
# prep rasters for plotting

#--------------------------prep central coast 2013-----------------------------#
# transform landsat data to Teale Albers
cen_rast_build1 <- st_transform(cen_landsat, crs = 3310) %>% 
  mutate(area = ifelse(area == 0, NA, 1))  %>% filter (year == 2013 & quarter ==3) 

#define blank raster
r1 <- rast(cen_rast_build1, res=30)
cen_rast_2013 <- rasterize(cen_rast_build1, r1, field = "area", fun = mean)

#--------------------------prep central coast 2023-----------------------------#
# transform landsat data to Teale Albers
cen_rast_build2 <- st_transform(cen_landsat, crs = 3310) %>% 
  mutate(area = ifelse(area == 0, NA, 1))  %>% filter (year == 2023 & quarter ==3) 

#define blank raster
r2 <- rast(cen_rast_build2, res=30)
cen_rast_2023 <- rasterize(cen_rast_build2, r2, field = "area", fun = mean)

#--------------------------prep islands 2013-----------------------------#
# transform landsat data to Teale Albers
islands_rast_build1 <- st_transform(islands_landsat, crs = 3310) %>% 
  mutate(area = ifelse(area == 0, NA, 1))  %>% filter (year == 2013 & quarter ==3) 

#define blank raster
r3 <- rast(islands_rast_build1, res=30)
islands_rast_2013 <- rasterize(islands_rast_build1, r3, field = "area", fun = mean)

#--------------------------prep central coast 2023-----------------------------#
# transform landsat data to Teale Albers
islands_rast_build2 <- st_transform(islands_landsat, crs = 3310) %>% 
  mutate(area = ifelse(area == 0, NA, 1))  %>% filter (year == 2023 & quarter ==3) 

#define blank raster
r4 <- rast(islands_rast_build2, res=30)
islands_rast_2023 <- rasterize(islands_rast_build2, r4, field = "area", fun = mean)

################################################################################
#define max kelp area


################################################################################
# plot timeseries

base_theme <-  theme(axis.text=element_text(size=10, color = "black"),
                     axis.title=element_text(size=12,color = "black"),
                     legend.text=element_text(size=8,color = "black"),
                     legend.title=element_text(size=9,color = "black"),
                     plot.tag=element_text(size=9,color = "black"),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=9, face = "bold",color = "black", hjust=0),
                     strip.background = element_blank())

timeseries <- timeseries %>%
  mutate(area = recode(area, 
                       "central" = "Mainland", 
                       "n_islands" = "North islands"))

# Define a custom color palette
custom_colors <- c("Mainland" = "forestgreen", "North islands" = "#ff7f0e")

# Plot with modifications
A <- ggplot(timeseries, aes(x = year, y = deviation, color = area)) +
  geom_line(aes(color = area), linewidth = 1) +   # Ensure color is applied to lines
  geom_point(aes(color = area)) +  # Ensure color is applied to points
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +  # Horizontal dashed line at 0
  labs(
    title = "",
    x = "Year",
    y = "Standard deviations from \n1990-2013 average",
    color = "Area"
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for areas
  theme_bw() + base_theme +
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.key = element_rect(fill = alpha('blue', 0))  # Adjust legend key
  )
A

################################################################################
# plot maps

m1 <- ggplot() +
  #add historic kelp extent
  #tidyterra::geom_spatraster(data = kelp_na, na.rm = TRUE) +
  #scale_fill_gradient(low = alpha("#7286AC",0.4),
  #                    high = alpha("#7286AC",0.4),
  #                    na.value = NA)+
  #guides(fill = guide_legend(override.aes = list(size = 3),
  #                           label.theme = element_text(color = "white"))) +
  #labs(fill = "Max kelp \nextent")+
  #add observed landsat for 2022 Q3
  ggnewscale::new_scale_fill()+
  tidyterra::geom_spatraster(data = cen_rast_2013, na.rm = TRUE) +
  scale_fill_gradient2(low = "navyblue",mid="#1B9C00",high = "#FFFF79",
                       na.value = NA)+
  #scale_fill_viridis_c(na.value = NA)+
  labs(fill = expression("Kelp area" ~(Kg~"/"~"30m"^{-2})))+
  #add land
  geom_sf(data = ca_counties_orig, fill = "gray", color = "gray80") +
  coord_sf(xlim = c(-122.369951, -121.022622), ylim = c(35.794874, 37.118691), crs = 4326)+
  labs(title = "2008 Q3")+
  theme_bw() + base_theme +theme(
    plot.tag.position = c(-0.03, 1),
    axis.title=element_blank(),
    panel.background = element_rect(fill = "white"))
m1




