#Joshua G. Smith, jossmith@mbayaq.org
# nc processing 

# Data Source on EDI
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-sbc&identifier=74&revision=23


rm(list=ls())

#set memory #### USE R_MAX_VSIZE=100Gb; 
## BE SURE TO SAVE .Renviron
usethis::edit_r_environ()

#restart R 
.rs.restartR()

# install.packages("librarian")
librarian::shelf(tidyverse, tidync, sf, rnaturalearth, usethis, raster, here)

#set directories 
datin <- "/Users/jossmith/Documents/Data/landsat" #use local directory to save server memory
#datin <- "/Volumes/seaotterdb$/kelp_recovery/data/kelp_landsat/raw"
datout <- "/Users/jossmith/Documents/Data/landsat/processed/ca_climate"
figdir <- here::here("analyses","figures")

landsat_dat <- "LandsatKelpBiomass_2024_Q3_withmetadata.nc"

# read the data in
landsat_raw <- tidync(file.path(datin, landsat_dat)) 

# Transform the biomass grid into a data frame
landsat_df <- landsat_raw %>% 
  activate() %>%
  hyper_tibble(force = TRUE) 

# Transform the time, year, qtr grid into a data frame
## Note: time is unique identifier
kelp_year <- landsat_raw %>%
  activate("D1") %>%
  hyper_tibble() 


# Transform the latitude grid into a data frame
kelp_lat <- landsat_raw %>%
  activate("latitude") %>%
  hyper_tibble()

# Transform the longitude grid into a data frame
kelp_lon <- landsat_raw %>%
  activate("longitude") %>%
  hyper_tibble()

# Join lat and lon
kelp_latlon <- left_join(kelp_lat, kelp_lon, by = "station") %>%
  relocate(station, .before=everything())

# Join landsat df with kelp latlong
kelp_df_latlon <- left_join(landsat_df, kelp_latlon, by = "station") %>%
  relocate(station, .before=everything())

#check join
head(kelp_df_latlon)

# Join df with time
kelp_processed <- left_join(kelp_df_latlon, kelp_year, by = "time") %>%
  relocate(station, time, year, quarter, .before=everything())

#check join
head(kelp_processed)
nrow(kelp_processed)

# transform it to sf object
kelp_landsat_sf <- kelp_processed %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

#free memory
rm(kelp_processed)
rm(kelp_df_latlon)
rm(kelp_latlon)
rm(kelp_lon)
rm(kelp_lat)
rm(landsat_df)

################################################################################
#inspect
kelp_2008 <- kelp_landsat_sf %>% filter(year == 2008 & quarter == 3) %>%
  mutate(biomass = ifelse(biomass > 0,1,0))

#MPEN
ggplot() +
  # Plot land
  #geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  # geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot kelp
  geom_sf(data=kelp_landsat_sf %>% filter(year == 2008 & quarter == 3), aes(color = area), size = 1) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-122.08, -121.835531), ylim = c(36.510140, 36.670574)) +
  # Theme
  theme_bw()




################################################################################
#export processed data by focal regions

#central coast region
kelp_landsat_export_cen <- kelp_landsat_sf %>%
  #filter Monterey Peninsula
  filter(
    #Malpaso creek = southernmost point
    latitude >= 33.696665 &
      #Marina SB = northernmost point
      latitude <= 37.496309)


################################################################################
#Export
st_write(kelp_landsat_export_cen, file.path(datout, "landsat_cen_1984_2024_points_withNAs.shp"))


