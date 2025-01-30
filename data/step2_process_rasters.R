

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

sc_county <- landsat_orig %>% filter(latitude <= 37.107295 &
                                       latitude >= 36.850509)

mty_county <- landsat_orig %>% filter(latitude <= 36.850509 &
                                      latitude >= 35.795190)

slo_county <- landsat_orig %>% filter(latitude <= 35.795190 &
                                        latitude >= 34.974713)

sb_county <- landsat_orig %>% filter(latitude <= 34.974713 &
                                        latitude >= 34.373312)

islands <- landsat_orig %>% filter(latitude <= 34.149371 & latitude >=33.837119) %>%
                            filter(longitude <= -119.297081 & longitude >= -120.535060)

rm(landsat_orig)

################################################################################
# summarize

# Group by year and quarter, and summarize the biomass and area values
# We want the total annual bioamss for each county
sum_sc <- sc_county %>%
  st_drop_geometry()%>%
  #filter to year 1990 and beyond for Q3 only
  filter(quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
            )

sum_mty <- mty_county %>%
  st_drop_geometry()%>%
  #filter to year 1990 and beyond for Q3 only
  filter(quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
  )

sum_slo <- slo_county %>%
  st_drop_geometry()%>%
  #filter to year 1990 and beyond for Q3 only
  filter(quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
  )


sum_sb <- sb_county %>%
  st_drop_geometry()%>%
  #filter to year 1990 and beyond for Q3 only
  filter(quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
  )


sum_islands <- islands %>%
  st_drop_geometry()%>%
  #filter to year 1990 and beyond for Q3 only
  filter(quarter == 3)%>%
  group_by(year, quarter) %>%
  summarize(total_biomass = sum(biomass, na.rm = TRUE),
            total_area = sum(area, na.rm = TRUE),
  )


################################################################################
# calculate baselines

#determine baseline average kelp area
sc_baseline <- sum_sc %>%
  ungroup()%>%
  data.frame()%>%
  filter(year < 2014) %>%
  filter(quarter == 3)%>% #filter to quarter 3 
  summarize(baseline_avg = mean(total_area, na.rm=TRUE),
            baseline_sd = sd(total_area, na.rm=TRUE)) 

mty_baseline <- sum_mty %>%
  ungroup()%>%
  data.frame()%>%
  filter(year < 2014) %>%
  filter(quarter == 3)%>% #filter to quarter 3 
  summarize(baseline_avg = mean(total_area, na.rm=TRUE),
            baseline_sd = sd(total_area, na.rm=TRUE)) 

slo_baseline <- sum_slo %>%
  ungroup()%>%
  data.frame()%>%
  filter(year < 2014) %>%
  filter(quarter == 3)%>% #filter to quarter 3 
  summarize(baseline_avg = mean(total_area, na.rm=TRUE),
            baseline_sd = sd(total_area, na.rm=TRUE)) 

sb_baseline <- sum_sb %>%
  ungroup()%>%
  data.frame()%>%
  filter(year < 2014) %>%
  filter(quarter == 3)%>% #filter to quarter 3 
  summarize(baseline_avg = mean(total_area, na.rm=TRUE),
            baseline_sd = sd(total_area, na.rm=TRUE)) 

islands_baseline <- sum_islands %>%
  ungroup()%>%
  data.frame()%>%
  filter(year < 2014) %>%
  filter(quarter == 3)%>% #filter to quarter 3 
  summarize(baseline_avg = mean(total_area, na.rm=TRUE),
            baseline_sd = sd(total_area, na.rm=TRUE)) 

#inspect
#View(baseline_average)

################################################################################
# calculate standard deviations

# calculate departures in standard deviation from baseline avg
sc_anom <- sum_sc %>%
  cross_join(sc_baseline) %>%
  mutate(deviation = (total_area - baseline_avg) / baseline_sd) %>%
  mutate(couty = "Santa Cruz")

mty_anom <- sum_mty %>%
  cross_join(mty_baseline) %>%
  mutate(deviation = (total_area - baseline_avg) / baseline_sd) %>%
  mutate(couty = "Monterey")

slo_anom <- sum_slo %>%
  cross_join(slo_baseline) %>%
  mutate(deviation = (total_area - baseline_avg) / baseline_sd) %>%
  mutate(couty = "San Luis Obispo")

sb_anom <- sum_sb %>%
  cross_join(sb_baseline) %>%
  mutate(deviation = (total_area - baseline_avg) / baseline_sd) %>%
  mutate(couty = "Santa Barbara")

islands_anom <- sum_islands %>%
  cross_join(islands_baseline) %>%
  mutate(deviation = (total_area - baseline_avg) / baseline_sd) %>%
  mutate(couty = "North Channel Islands")


timeseries_data <- rbind(sc_anom, mty_anom, slo_anom, sb_anom, islands_anom)

################################################################################
# check file size before export

#print(object.size(cen_main))
#print(object.size(islands))
#print(object.size(timeseries_data))

write_csv(timeseries_data, file.path(output, "timeseries_data.csv")) 




