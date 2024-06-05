getwd()
setwd("~/Documents/FYP_WP/code")

library(tidyverse)
library(readr)
library(Hmisc)
library(broom)
library(dplyr)

#IMPORT DATA
data <- read_csv("../data/DJB_BEF_DATA_COMBINED_standardised_w_continentsv2.csv", 
                 col_types = cols(Biodiversity_value_x = col_number(), 
                                  Biodiversiy_value_SD = col_number(), 
                                  Ecosystem_function_value_y = col_number()))

#REMOVE NAs
data = subset(data, !is.na(Ecosystem_function_value_y))
data = subset(data, !is.na(Biodiversity_value_x))

data$Biodiversity_value_x <- data$Biodiversity_value_x + 0.0000001

data_pollination <- filter(data, Ecosystem_function_metric %in% "Pollination_and_dispersal_of_seeds_and_other_propagules")

# raster
install.packages("raster")
library(raster)
library(sp)

# sf (alternative + new to rgdal)
install.packages("sf")
library(sf)

# load raster in an R object - different climate RCP scenarios
rcp2.6_raster <- raster("../data/sensitive/bii-ssp1_rcp2.6_image-2050.tif")
rcp4.5_raster <- raster("../data/sensitive/bii-ssp2_rcp4.5_message-globiom-2050.tif")
rcp6.0_raster <- raster("../data/sensitive/bii-ssp4_rcp6.0_gcam-2050.tif")
rcp7.0_raster <- raster("../data/sensitive/bii-ssp3_rcp7.0_aim-2050.tif")
rcp8.5_raster <- raster("../data/sensitive/bii-ssp5_rcp8.5_remind-magpie-2050.tif")


# min and max values of biodiversity from data_pollination dataset
bio_min_value <- min(data_pollination$Biodiversity_value_x) # 1e-07
bio_max_value <- max(data_pollination$Biodiversity_value_x) # 5.175753

# scaling PREDICTS data to match original BEF data
rescale_predicts <- function(x) { ((x/bio_max_value)*(bio_max_value-bio_min_value))+ bio_min_value }

scaled_rcp2.6_raster <- rescale_predicts(rcp2.6_raster)
scaled_rcp4.5_raster <- rescale_predicts(rcp4.5_raster)
scaled_rcp6.0_raster <- rescale_predicts(rcp6.0_raster)
scaled_rcp7.0_raster <- rescale_predicts(rcp7.0_raster)
scaled_rcp8.5_raster <- rescale_predicts(rcp8.5_raster)

#scaled_biodiversity_value_x_slope <- rescale_predicts(0.2)
#scaled_biodiversity_value_x_c <- rescale_predicts(1.4)

# uses slope and intercept from bayes model
scaled_predicted_rcp2.6_raster <- ((scaled_rcp4.5_raster*0.2) + 1.4)
scaled_predicted_rcp4.5_raster <- ((scaled_rcp4.5_raster*0.2) + 1.4)
scaled_predicted_rcp6.0_raster <- ((scaled_rcp6.0_raster*0.2) + 1.4)
scaled_predicted_rcp7.0_raster <- ((scaled_rcp7.0_raster*0.2) + 1.4)
scaled_predicted_rcp8.5_raster <- ((scaled_rcp8.5_raster*0.2) + 1.4)

# eg for rcp 2.6
plot(scaled_predicted_rcp2.6_raster, 
     main = "A. Prediction of biodiversity for scenario RCP 2.6")

scaled_predicted_raster_stack <- stack()

# RCP 2.6
rcp2.6_map <- rasterToPoints(scaled_predicted_rcp2.6_raster)
df_rcp2.6 <- data.frame(rcp2.6_map)
colnames(df_rcp2.6) <- c('x', 'y', 'Amount_of_biodiversity') # needs to be the same as aes(fill=Scale)

(rcp2.6_min_value <- min(df_rcp2.6$Amount_of_biodiversity)) # 1.429423
(rcp2.6_max_value <- max(df_rcp2.6$Amount_of_biodiversity)) # 1.644928

rcp2.6_map_plot <- ggplot(data=df_rcp2.6, aes(y=y, x=x)) +
  geom_raster(aes(fill=Amount_of_biodiversity)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient(name = "Amount of Pollination", low = "red3", high = "lightyellow", 'Scale', limits=c(1.429423, 1.644928))

(rcp2.6_map_plot2 <- rcp2.6_map_plot + ggtitle("A") + theme(
  axis.title = element_blank(),      # Remove axis titles
  axis.text = element_blank(),       # Remove axis text (ticks labels)
  axis.ticks = element_blank(),      # Remove axis ticks
  axis.line = element_blank(),        # Remove axis lines
  panel.grid.major = element_blank(), # Remove major grid lines
  panel.grid.minor = element_blank(), 
  plot.title = element_text(hjust = 0.5),        # Center the title
  plot.subtitle = element_text(hjust = 1),       # Right-align the subtitle
  plot.caption = element_text(hjust = 0)))         # Left-align the caption

# RCP 4.5
rcp4.5_map <- rasterToPoints(scaled_predicted_rcp4.5_raster)
df_rcp4.5 <- data.frame(rcp4.5_map)
colnames(df_rcp4.5) <- c('x', 'y', 'Amount_of_biodiversity') # needs to be the same as aes(fill=Scale)

(rcp4.5_min_value <- min(df_rcp4.5$Amount_of_biodiversity)) # 1.429423
(rcp4.5_max_value <- max(df_rcp4.5$Amount_of_biodiversity)) # 1.644928

rcp4.5_map_plot <- ggplot(data=df_rcp4.5, aes(y=y, x=x)) +
  geom_raster(aes(fill=Amount_of_biodiversity)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient(name = "Amount of Pollination", low = "red3", high = "lightyellow", limits=c(1.429423, 1.644928))

(rcp4.5_map_plot2 <- rcp4.5_map_plot + ggtitle("B") + theme(
  axis.title = element_blank(),      # Remove axis titles
  axis.text = element_blank(),       # Remove axis text (ticks labels)
  axis.ticks = element_blank(),      # Remove axis ticks
  axis.line = element_blank(),        # Remove axis lines
  panel.grid.major = element_blank(), # Remove major grid lines
  panel.grid.minor = element_blank(), # Remove minor grid lines
  plot.title = element_text(hjust = 0.5),        # Center the title
  plot.subtitle = element_text(hjust = 1),       # Right-align the subtitle
  plot.caption = element_text(hjust = 0)))         # Left-align the caption

# RCP 6.0
rcp6.0_map <- rasterToPoints(scaled_predicted_rcp6.0_raster)
df_rcp6.0 <- data.frame(rcp6.0_map)
colnames(df_rcp6.0) <- c('x', 'y', 'Amount_of_biodiversity') # needs to be the same as aes(fill=Scale)

(rcp6.0_min_value <- min(df_rcp6.0$Amount_of_biodiversity)) # 1.42949
(rcp6.0_max_value <- max(df_rcp6.0$Amount_of_biodiversity)) # 1.646094

rcp6.0_map_plot <- ggplot(data=df_rcp6.0, aes(y=y, x=x)) +
  geom_raster(aes(fill=Amount_of_biodiversity)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient(name = "Amount of Pollination", low = "red3", high = "lightyellow", limits=c(1.42949, 1.646094)) 

(rcp6.0_map_plot2 <- rcp6.0_map_plot + ggtitle("C") + theme(
  axis.title = element_blank(),      # Remove axis titles
  axis.text = element_blank(),       # Remove axis text (ticks labels)
  axis.ticks = element_blank(),      # Remove axis ticks
  axis.line = element_blank(),        # Remove axis lines
  panel.grid.major = element_blank(), # Remove major grid lines
  panel.grid.minor = element_blank(), # Remove minor grid lines
  plot.title = element_text(hjust = 0.5),        # Center the title
  plot.subtitle = element_text(hjust = 1),       # Right-align the subtitle
  plot.caption = element_text(hjust = 0)))         # Left-align the caption

# RCP 7.0
rcp7.0_map <- rasterToPoints(scaled_predicted_rcp7.0_raster)
df_rcp7.0 <- data.frame(rcp7.0_map)
colnames(df_rcp7.0) <- c('x', 'y', 'Amount_of_biodiversity') # needs to be the same as aes(fill=Scale)

(rcp7.0_min_value <- min(df_rcp7.0$Amount_of_biodiversity)) # 1.432459
(rcp7.0_max_value <- max(df_rcp7.0$Amount_of_biodiversity)) # 1.639049

rcp7.0_map_plot <- ggplot(data=df_rcp7.0, aes(y=y, x=x)) +
  geom_raster(aes(fill=Amount_of_biodiversity)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient(name = "Amount of Pollination", low = "red3", high = "lightyellow", limits=c(1.432459, 1.639049))

(rcp7.0_map_plot2 <- rcp7.0_map_plot + ggtitle("D") + theme(
  axis.title = element_blank(),      # Remove axis titles
  axis.text = element_blank(),       # Remove axis text (ticks labels)
  axis.ticks = element_blank(),      # Remove axis ticks
  axis.line = element_blank(),        # Remove axis lines
  panel.grid.major = element_blank(), # Remove major grid lines
  panel.grid.minor = element_blank(), # Remove minor grid lines
  plot.title = element_text(hjust = 0.5),        # Center the title
  plot.subtitle = element_text(hjust = 1),       # Right-align the subtitle
  plot.caption = element_text(hjust = 0)))         # Left-align the caption

# RCP 8.5
rcp8.5_map <- rasterToPoints(scaled_predicted_rcp8.5_raster)
df_rcp8.5 <- data.frame(rcp8.5_map)
colnames(df_rcp8.5) <- c('x', 'y', 'Amount_of_biodiversity') # needs to be the same as aes(fill=Scale)

(rcp8.5_min_value <- min(df_rcp8.5$Amount_of_biodiversity)) # 1.42928
(rcp8.5_max_value <- max(df_rcp8.5$Amount_of_biodiversity)) # 1.647608

rcp8.5_map_plot <- ggplot(data=df_rcp8.5, aes(y=y, x=x)) +
  geom_raster(aes(fill=Amount_of_biodiversity)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient(name = "Amount of Pollination", low = "red3", high = "lightyellow", limits=c(1.42928, 1.647608))

(rcp8.5_map_plot2 <- rcp8.5_map_plot + ggtitle("E") + theme(
  axis.title = element_blank(),      # Remove axis titles
  axis.text = element_blank(),       # Remove axis text (ticks labels)
  axis.ticks = element_blank(),      # Remove axis ticks
  axis.line = element_blank(),        # Remove axis lines
  panel.grid.major = element_blank(), # Remove major grid lines
  panel.grid.minor = element_blank(), # Remove minor grid lines
  plot.title = element_text(hjust = 0.5),        # Center the title
  plot.subtitle = element_text(hjust = 1),       # Right-align the subtitle
  plot.caption = element_text(hjust = 0)))         # Left-align the caption

install.packages("cowplot")
library(cowplot)

install.packages("gridExtra")
library(gridExtra)

layout_matrix <- rbind(c(1, 2),
                       c(3, 4),
                       c(5, 6))

# Combine the plots with the custom layout
(combined_plot <- grid.arrange(rcp2.6_map_plot2 + theme(legend.position = "none"),
                               rcp4.5_map_plot2 + theme(legend.position = "none"), 
                               rcp6.0_map_plot2 + theme(legend.position = "none"),
                               rcp7.0_map_plot2 + theme(legend.position = "none"),
                               rcp8.5_map_plot2 + theme(legend.position = "none"), 
                               layout_matrix = layout_matrix))

# Extract the legend from one of the plots
(legend <- get_legend(rcp7.0_map_plot2 + theme(legend.position = "right")))

# Combine the plots and the legend
(final_plot <- plot_grid(combined_plot, legend, ncol=1, rel_widths = c(1, .1)))
