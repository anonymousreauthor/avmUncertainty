#***************************************************************************************************
#
#  Script for Creating the Sales Location Map
#
#***************************************************************************************************

## MAKE SURE YOU'VE TESTED YOUR SYSTEM WITH THE SANDBOX.R FILE FIRST!

### Setup ------------------------------------------------------------------------------------------

  # Load Libraries
  library(kingCoData) #devtools::install_github('anonymousREAuthor/kingCoData')
  library(hpiR)
  library(tidyverse)
  library(sf)
  library(tigris)

  # Create directory for results if it doesn't exist
  setwd(file.path(getwd(), 'results'))

 ## Data Prep

  # Load filtered data previous scripts
  filter_df <- readRDS(file.path(getwd(), 'filtered_data.rds'))

  # Convert to Simple Features object and update CRS
  filter_sf <- sf::st_as_sf(filter_df, coords = c('longitude', 'latitude'))
  sf::st_crs(filter_sf) <- 4269

  # Create 2019 only data for faster plotting
  sales2019_sf <- filter_sf %>%
    dplyr::filter(substr(sale_date, 1, 4) == '2019')

  # Load all state counties with tigris package
  wa_sf <- tigris::counties(state = 'WA', cb = TRUE, resolution = "500k")

  # Convert to Simple Features object and update CRS
  wa_sf <- sf::st_as_sf(wa_sp)
  sf::st_crs(wa_sf) <- 4269

  # Extract all counties that border King County
  region_sf <- wa_sf %>%
    dplyr::filter(NAME %in% c('Island', 'Kitsap', 'Pierce', 'Snohomish', 'King',
                              'Kittitas', 'Chelan', 'Yakima'))

  # Extract King County
  king_sf <- wa_sf %>%
    dplyr::filter(NAME == 'King')

 ## Create Map

  sales_map <-
    ggplot() +
      geom_sf(data = region_sf, fill = 'gray95') +
      geom_sf(data = king_sf, fill = 'gray85') +
      geom_sf(data = sales2019_sf, color = 'black', alpha = .2, shape = 15, size = .2) +
      coord_sf(xlim = c(-122.55, -121.1),
               ylim = c(47.1, 47.8)) +
      theme(panel.background = element_rect(fill = "lightblue",
                                           color = "lightblue"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
      labs(title = 'Location of Home Sales',
           subtitle = 'King County, WA, USA')

  # Save to PNG

  png(file=file.path(getwd(), 'map1.png') ,width=550, height=400)
    sales_map
  dev.off()

#***************************************************************************************************
#***************************************************************************************************

