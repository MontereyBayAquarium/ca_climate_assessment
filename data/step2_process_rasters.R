

#Joshua G. Smith; jossmith@mbayaq.org

rm(list=ls())

######
######
#required packages
librarian::shelf(tidyverse, sf, raster, shiny, tmap, terra, tidyterra, RColorBrewer)

#set directories 
#basedir <- "/Volumes/seaotterdb$/kelp_recovery/data"
localdir <- "/Users/jossmith/Documents/Data/landsat"
output <- here::here("output")

#read landsat raw
landsat_orig <- st_read(file.path(localdir, "/processed/ca_climate/landsat_cen_1984_2024_points_withNAs.shp"))

################################################################################
# filter to central coast region and north channel islands

#Santa Cruz Ano Nuevo is 37.107295, -122.293032
#rincon point is 34.372820, -119.477869

cen_main <- landsat_orig %>% filter(latitude <= 37.107295 &
                                      latitude >= 34.372820)

islands <- landsat_orig %>% filter(latitude <= 34.149371 & latitude >=33.837119) %>%
                            filter(longitude <= -119.297081 & longitude >= -120.535060)

rm(landsat_orig)

################################################################################
# summarize

# Group by year and quarter, and summarize the biomass and area values
# We want the total annual bioamss for each cluster
summarized_cen <- cen_main %>%
  st_drop_geometry()%>%
  #filter to year 1990 and beyond for Q3 only
  filter(year >= 1990 & quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
            )

summarized_islands <- islands %>%
  st_drop_geometry()%>%
  #filter to year 1990 and beyond for Q3 only
  filter(year >= 1990 & quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
  )

#determine baseline average kelp area
cen_baseline_average <- summarized_cen %>%
  ungroup()%>%
  data.frame()%>%
  filter(year < 2014) %>%
  filter(quarter == 3)%>% #filter to quarter 3 
  summarize(baseline_avg = mean(total_area, na.rm=TRUE),
            baseline_sd = sd(total_area, na.rm=TRUE)) 

islands_baseline_average <- summarized_islands %>%
  ungroup()%>%
  data.frame()%>%
  filter(year < 2014) %>%
  filter(quarter == 3)%>% #filter to quarter 3 
  summarize(baseline_avg = mean(total_area, na.rm=TRUE),
            baseline_sd = sd(total_area, na.rm=TRUE)) 

#inspect
#View(baseline_average)

# calculate departures in standard deviation from baseline avg
cen_anom <- summarized_cen %>%
  cross_join(cen_baseline_average) %>%
  mutate(deviation = (total_area - baseline_avg) / baseline_sd) %>%
  mutate(area = "central")

islands_anom <- summarized_islands %>%
  cross_join(islands_baseline_average) %>%
  mutate(deviation = (total_area - baseline_avg) / baseline_sd) %>%
  mutate(area = "n_islands")



timeseries_data <- rbind(cen_anom, islands_anom)

################################################################################
# check file size before export

#print(object.size(cen_main))
#print(object.size(islands))
#print(object.size(timeseries_data))

write_csv(timeseries_data, file.path(output, "timeseries_data.csv")) 

st_write(cen_main, file.path(localdir, "/processed/ca_climate/landsat_central_coast.geojson")) #last write 27 Jan 2025
st_write(islands, file.path(localdir, "/processed/ca_climate/landsat_north_islands.geojson")) #last write 27 Jan 2025






