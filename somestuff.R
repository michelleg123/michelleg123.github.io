

cali_dat$pred <- predict(mod, newdata = cali_dat)

us_states <- map_data('county')


# Map of California with Median House Value : 
la <- c('LA', -118.15, 34.03)
sf <- c('San Francisco', -122.24, 37.46)
sd <- c('San Diego',-117.16, 32.72 )
sb <- c('Santa Barbara', -119.70, 34.42)

locs <- as.data.frame(matrix(c(la, sf, sd, sb), nrow = 4, byrow = T))
colnames(locs) <- c('place','long','lat')


# Median House Predicted Prices : 

ggplot(cali_dat) +
  geom_polygon(data = filter(us_states, region == 'california'),
               mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(aes(x = longitude, y = latitude, color =  pred, size = population), alpha = .5) +
  coord_fixed() +
  theme_map() +
  theme(legend.position = "right") +
  scale_color_continuous(type = 'viridis') +
  labs(color = 'Predicted House Value', size = 'Population', title = 'Predicted House Prices and Population in California') +
  ggrepel::geom_text_repel(data = locs, aes(x = as.numeric(long), y = as.numeric(lat), label = place), hjust = 3, 
                           vjust = 3)

