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
timeseries <- read_csv(file.path(output, "timeseries_data.csv")) %>%
                  rename('county'=couty)




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



# Define county order
county_order <- c("Santa Cruz", "Monterey", "San Luis Obispo", "Santa Barbara", "North Channel Islands")

# Ensure the county column is a factor with the specified order
timeseries$county <- factor(timeseries$county, levels = county_order)

# Determine the x-axis range
x_min <- 1985  # Force starting year at 1985
x_max <- max(timeseries$year)

# Plot with Dark2 color theme, ordered legend, and 5-year tick marks starting at 1985
A <- ggplot(timeseries, aes(x = year, y = deviation, color = county)) +
  geom_line(linewidth = 0.5) +  # Ensure color is applied to lines
  geom_point(size=0.8) +  # Ensure color is applied to points
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +  # Horizontal dashed line at 0
  labs(
    title = "",
    x = "Year",
    y = "",
    color = "Area",
    tag= "A"
  ) +
  scale_color_brewer(palette = "Dark2", limits = county_order) +  # Apply Dark2 and set order
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = 5),  # Tick marks every 5 years, starting at 1985
    expand = c(0, 0)  # Prevent extra space at the edges
  ) +
  #add heatwaves
  # 1982-83 
  annotate(geom="rect", xmin=1996.5, xmax=1997.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  # 1982-83 
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  #add 1.5 sd region
  # Add solid horizontal lines at ±1.5 SD
  geom_hline(yintercept = 1, linetype = "solid", color = "gray20", linewidth = 0.6) +
  geom_hline(yintercept = -1, linetype = "solid", color = "gray20", linewidth = 0.6) +
  annotate("rect", xmin = 1984, xmax = x_max, ymin = -1, ymax = 1, 
           fill = "gray80", alpha = 0.4)+
  theme_bw() + base_theme +
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.key = element_rect(fill = alpha('blue', 0)),  # Adjust legend key
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = unit(c(1,1,0,0), 'lines')
  )

A


B <- ggplot(timeseries %>% filter(year > 2013), aes(x = year, y = deviation, color = county)) +
  geom_line(linewidth = 0.5) +  # Ensure color is applied to lines
  geom_point(size=0.8) +  # Ensure color is applied to points
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +  # Horizontal dashed line at 0
  labs(
    title = "",
    x = "Year",
    y = "",
    color = "Area",
    tag = "B"
  ) +
  scale_color_brewer(palette = "Dark2", limits = county_order) +  # Apply Dark2 and set order
  scale_x_continuous(
    breaks = seq(2014, x_max, by = 2),  # Tick marks every 5 years, starting at 1985
    expand = c(0, 0)  # Prevent extra space at the edges
  ) +
  #add heatwaves
  # 1982-83 
 # annotate(geom="rect", xmin=1996.5, xmax=1997.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  # 1982-83 
  annotate(geom="rect", xmin=2014, xmax=2016, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  #add 1.5 sd region
  # Add solid horizontal lines at ±1.5 SD
  geom_hline(yintercept = 1, linetype = "solid", color = "gray20", linewidth = 0.6) +
  geom_hline(yintercept = -1, linetype = "solid", color = "gray20", linewidth = 0.6) +
  annotate("rect", xmin = 2014, xmax = x_max, ymin = -1, ymax = 1, 
           fill = "gray80", alpha = 0.4)+
  theme_bw() + base_theme +
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.key = element_rect(fill = alpha('blue', 0)),  # Adjust legend key
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = unit(c(0,1,1,0), 'lines')
  )

B


p <- ggpubr::ggarrange(A, B, nrow=2)
p

# Annotate the figure with a shared y-axis label
p_annotated <- ggpubr::annotate_figure(
  p,
  left = grid::textGrob("Standard deviations from \n1984-2013 average", rot = 90, vjust = 0.5, gp = grid::gpar(fontsize = 12)),
  bottom = grid::textGrob("Year", vjust = 0, hjust=-0.5, gp = grid::gpar(fontsize = 12))
)

p_annotated


ggsave(
  filename = "~/Downloads/kelp_timeseries_plot.png",  
  plot = A,  
  width = 8,  
  height = 3,  
  units = "in",
  dpi = 600   
)

ggsave(
  filename = "~/Downloads/kelp_timeseries_combined_plot.png",  
  plot = p_annotated,  
  width = 5,  
  height = 4,  
  units = "in",
  dpi = 600,
  bg = "white"
)
