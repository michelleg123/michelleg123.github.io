library(tidyverse)
library(skimr)
library(ggthemes)
library(socviz)

climate_opinion_long <- read_csv(
  'https://bcdanl.github.io/data/climate_opinion_2021.csv')
 

# First : Transform the Data 
climate_opinion_long <- climate_opinion_long %>% 
filter(belief == 'human') # only include the "human" entries 
county_map <- socviz::county_map
county_map$id <- as.numeric(county_map$id) # change to numerical values 
county_full <- left_join(county_map, climate_opinion_long) # join the two by "id"


p <- ggplot(data = county_full,
       mapping = aes(x = long, y = lat,
                     fill = perc, 
                     group = group))
p1 <- p + geom_polygon(color = "gray70", size = 0.05) + coord_equal()
p1

p2 <- p1 + scale_fill_gradient2( 
  low = '#2E74C0',  # from party_colors for DEM
  mid = '#FFFFFF',  # transparent white
  high = '#CB454A',  # from party_colors for GOP
  na.value = "grey50", # missing values 
  midpoint = median(climate_opinion_long$perc))
p2 + labs(title = "US Climate Opinion",
              fill = "Percent\nBelief") +
  theme_map() + theme(legend.position = "bottom")
