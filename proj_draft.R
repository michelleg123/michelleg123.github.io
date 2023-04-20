# DANL Final Project : 

# - 
invisible(library(tidyverse))
invisible(library(ggthemes))
invisible(library(Hmisc))
invisible(library(viridisLite))

us_states <- map_data('county')

cali_dat <- read.csv('california_housing.csv')


la <- c('LA', -118.15, 34.03)
sf <- c('San Francisco', -122.24, 37.46)

locs <- as.data.frame(matrix(c(la, sf), nrow = 2, byrow = T))
colnames(locs) <- c('place','long','lat')

ggplot(cali_dat) +
  geom_polygon(data = filter(us_states, region == 'california'),
               mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(aes(x = longitude, y = latitude, color = medianHouseValue, size = population), alpha = .5) +
  coord_fixed() +
  theme_map() +
  theme(legend.position = "right") +
  scale_color_continuous(type = 'viridis') +
  labs(color = 'Median House Value', size = 'Population', title = 'Median House Prices and Population in California') +
  ggrepel::geom_text_repel(data = locs, aes(x = as.numeric(long), y = as.numeric(lat), label = place), hjust = 3, 
                           vjust = 3)


