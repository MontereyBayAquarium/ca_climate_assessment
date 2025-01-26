

#Joshua G. Smith; jossmith@mbayaq.org

rm(list=ls())

######
######
#required packages
librarian::shelf(tidyverse, sf, raster, shiny, tmap, terra, tidyterra, RColorBrewer)

#set directories 
#basedir <- "/Volumes/seaotterdb$/kelp_recovery/data"
localdir <- "/Users/jossmith/Documents/Data/landsat"
output <- here::here("analyses","output")

#read state
ca_counties_orig <- st_read(file.path(basedir, "gis_data/raw/ca_county_boundaries/s7vc7n.shp")) 

#read landsat raw
landsat_orig <- st_read(file.path(localdir, "/processed/ca_climate/landsat_cen_1984_2024_points_withNAs.shp"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")

# Filter for California counties
ca_counties <- usa[usa$name == "California", ]

################################################################################
# filter to central coast region and north channel islands

#Santa Cruz Ano Nuevo is 37.107295, -122.293032
#rincon point is 34.372820, -119.477869

cen_main <- landsat_orig %>% filter(latitude <= 37.107295 &
                                      latitude >= 34.372820)

islands <- landsat_orig %>% filter(latitude <= 34.149371 & latitude >=33.837119) %>%
                            filter(longitude <= -119.297081 & longitude >= -120.535060)


################################################################################
# summarize

# Group by year and quarter, and summarize the biomass and area values
# We want the total annual bioamss for each cluster
summarized_cen <- cen_main %>%
  #filter to year 1990 and beyond for Q3 only
  filter(year >= 1990 & quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
            )

summarized_islands <- islands %>%
  #filter to year 1990 and beyond for Q3 only
  filter(year >= 1990 & quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
  )

#determine baseline average kelp area
baseline_average <- summarized_data %>%
  filter(year < 2014) %>%
  filter(quarter == 3)%>% #filter to quarter 3 
  group_by(site_num, site_name) %>%
  summarize(baseline_avg = mean(total_area, na.rm=TRUE),
            baseline_sd = sd(total_area, na.rm=TRUE)) 

#inspect
#View(baseline_average)

# calculate departures in standard deviation from baseline avg
final_data <- summarized_data %>%
  st_join(baseline_average) %>%
  rename(site_num = site_num.x,
         site_name = site_name.x) %>%
  dplyr::select(-site_name.y, -site_num.y) %>%
  mutate(deviation = (total_biomass - baseline_avg) / baseline_sd)



file_to_delete <- file.path(output, "landsat_cluster_area.geojson")
if (file.exists(file_to_delete)) {
  file.remove(file_to_delete)
}


st_write(final_data,file_to_delete, file.path(output, "landsat_cluster_area.geojson")) #last write 16 Feb 2024







