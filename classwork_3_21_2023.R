# March 21 2023 

library(socviz)
library(ggthemes)
library(dplyr)
library(tidyverse)
election %>% select(state, total_vote,
                    r_points, pct_trump, party, census) %>%
  sample_n(5)


party_colors <- c("#2E74C0", "#CB454A")  # Hex color codes for Dem Blue and Rep Red
p0 <- ggplot(data = filter(election, st != "DC"),
             mapping = aes(x = r_points,
                           y = reorder(state, r_points),
                           color = party))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") +
  geom_point(size = 2)
p1

p2 <- p1 + scale_color_manual(values = party_colors)
p2


# Do some breaks on the x axis and use labels to describe Trump vs Clinton 
p3 <- p2 + scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30, 40),
                              labels = c("30\n (Clinton)", "20", "10", "0",
                                         "10", "20", "30", "40\n(Trump)"))
p3

# Break it up into sections based on geographical region 

p3 + facet_wrap(~ census, ncol=1, scales="free_y") +
  guides(color = "none") + labs(x = "Point Margin", y = "") +
  theme(axis.text=element_text(size=8))


# Make a map of the United States 

us_states <- map_data("state") # from the 'maps' package
us_states
view(us_states)

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))
p + geom_polygon(fill = "white", color = "black") # manual set the colors 


# Different colors for different states 
p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = "none")


# Transorm the map : 

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)


# Using this data frame, we can draw an election map 
election <- election
election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election)





# Use our party colors to fill 
p0 <- ggplot(data = us_states_elec,
             aes(x = long, y = lat,
                 group = group, fill = party))
p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# Because of theme_map function, we don't see the lines anymore: 
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2016", fill = NULL)
p2 + theme_map()


# The lighter the color, the more percentage of Trump 
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat, group = group, fill = pct_trump))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p1 + labs(title = "Trump vote") + theme_map() + labs(fill = "Percent")

# Fix the color gradient bc Trump is Rep
p2 <- p1 + scale_fill_gradient(low = "white", high = "#CB454A") +
  labs(title = "Trump vote") 
p2 + theme_map() + labs(fill = "Percent")


# Some diverging colors
# For election results, we might prefer a gradient that diverges from a midpoint
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat, group = group, fill = d_points))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_gradient2() + labs(title = "Winning margins") 
p2 + theme_map() + labs(fill = "Percent")

# From the scale_*_gradient2() function, 
# we can also re-specify the mid-level color along with the high and low colors.

p3 <- p1 + scale_fill_gradient2(low = "red", 
                                mid = scales::muted("purple"),
                                high = "blue", 
                                breaks = c(-25, 0, 25, 50, 75)) 
p3 + theme_map() + labs(fill = "Percent", title = "Winning margins")



# Omit outlier "district of columbia" 
# Remove observations with extreme values ! 
p0 <- ggplot(data = filter(us_states_elec,
                           region != "district of columbia"),
             aes(x = long, y = lat, group = group, fill = d_points))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p3 <- p1 + scale_fill_gradient2(low = "red", 
                                mid = scales::muted("purple"),
                                high = "blue", 
                                breaks = c(-25, 0, 25, 50, 75)) 
p3 + theme_map() + labs(fill = "Percent", title = "Winning margins", caption = "DC is omitted.")

# Chloropeth Maps : 
county_map <- socviz::county_map
county_data <- socviz::county_data

county_map
county_data %>%
  select(id, name, state, pop_dens, pct_black) %>%
  sample_n(5)
county_full <- 
  left_join(county_map, county_data, by = "id")


# Plot alaska and hawaii seperately 
p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill = pop_dens, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05)
p1

p1 + coord_equal() # Better aspect ratio 



# choosing apprpriate color palette matters 
p2 <- p1 + scale_fill_brewer(
  palette = "Blues",
  labels = c("0-10", "10-50", "50-100", "100-500",
             "500-1,000", "1,000-5,000", ">5,000"))
p2

# Details about labels:
p2 + labs(fill = "Population per\nsquare mile") +
  theme_map() +
  guides(fill = guide_legend(nrow = 1)) + # specifies how the legend is displayed
  theme(legend.position = "bottom")


# Percent of black in country : 

p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat, fill = pct_black, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2 <- p1 + scale_fill_brewer(palette="Greens")
p2 + labs(fill = "US Population, Percent Black") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() + theme(legend.position = "bottom")


# Color palettes:
orange_pal <- RColorBrewer::brewer.pal(n = 6, name = "Oranges")
orange_pal
orange_rev <- rev(orange_pal)
orange_rev

pop_p <- ggplot(data = county_full,
                mapping = aes(x = long, y = lat,
                              fill = pop_dens6, 
                              group = group))
pop_p1 <- pop_p + geom_polygon(color = "gray90", size = 0.05) +
  coord_equal()
pop_p2 <- pop_p1 + scale_fill_manual(values = orange_pal)
pop_p2 + labs(title = "Population Density",
              fill = "People per square mile") +
  theme_map() + theme(legend.position = "bottom")

pop_p2_rev <- pop_p1 + scale_fill_manual(values = orange_rev) # we can use our own color palette
pop_p2_rev + labs(title = "Reverse-coded Population Density",
                  fill = "People per square mile") +
  theme_map() + theme(legend.position = "bottom")



# choosing colors 
gop_p <- ggplot(data = county_full,
                mapping = aes(x = long, y = lat,
                              fill = per_gop_2016, 
                              group = group))
gop_p1 <- gop_p + geom_polygon(color = "gray70", size = 0.05) + coord_equal()
gop_p1

gop_p2 <- gop_p1 + scale_fill_gradient2( 
  low = '#2E74C0',  # from party_colors for DEM
  mid = '#FFFFFF',  # transparent white
  high = '#CB454A',  # from party_colors for GOP
  na.value = "grey50", # missing values 
  midpoint = .5)
gop_p2 + labs(title = "US Presidential Election 2016",
              fill = "Trump vote share") +
  theme_map() + theme(legend.position = "bottom")

# NY state map ; 
NY_socioecon_geo_poverty <- read_csv(
  'https://bcdanl.github.io/data/NY_socioecon_geo_poverty.csv'
)

library(viridis)

# WE want "plasma"
p <- ggplot(data = NY_socioecon_geo_poverty,
            mapping = aes(x = long, y = lat, group = group, 
                          fill = c04_058 ))
p1 <- p + geom_polygon(color = "grey", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_viridis_c(option = "plasma") + theme_map() 
p2


# How geographic pattern changes over time 
table(NY_socioecon_geo_poverty)

p2 + facet_wrap(~ year, ncol = 3) +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Poverty rate in NY (%)",
       title = "Poverty rate for the male population 25 years 
       and over \nfor whom the highest educational attainment is bachelor's degree")
# It looks like poverty rate has decreased. 

# Statebins: 
# install.packages("statebins")
library(statebins)  
p <- ggplot(election, aes( state = state, fill = pct_trump ) )
p1 <- p +  geom_statebins(lbl_size = 5,
                          border_col = "grey90", border_size = 1)
p2 <- p1 + labs(fill = "Percent Trump") +
  coord_equal() +
  theme_statebins( legend_position = c(.45, 1) ) +
  theme( legend.direction="horizontal" )
p2

p2 + scale_fill_gradient2( 
  low = '#2E74C0',  # from party_colors for DEM
  mid = '#FFFFFF',  # transparent white
  high = '#CB454A',  # from party_colors for GOP
  na.value = "grey50",
  midpoint = 50)   # set the midpoint value


# Remove Washington DC 
p <- ggplot(data = filter(election, st != "DC")  , 
            mapping = aes(state = state, fill = pct_clinton)) 
p1 <- p + geom_statebins(lbl_size = 5,
                         border_col = "grey90",
                         border_size = 1)

p <- ggplot(data = election  , 
            mapping = aes(state = state, fill = party)) 
p1 <- p + geom_statebins(lbl_size = 5,
                         border_col = "grey90",
                         border_size = 1)

p2 <- p1 + labs(fill = "Winner") +
  coord_equal() +
  theme_statebins( legend_position = c(.25, 1) ) +
  theme( legend.direction="horizontal",
         legend.title = element_text(size=30),
         legend.text = element_text(size=30) )
p2 + scale_fill_manual( values = c(Republican = "darkred", 
                                   Democratic = "royalblue"))


# Break down pct_trmup variable into 5 sections 

p <- ggplot(data = election  , 
            mapping = aes(state = state, fill=pct_trump)) 
p1 <- p + geom_statebins(lbl_size = 5,
                         border_col = "grey90",
                         border_size = 1)
p2 <- p1 + labs(fill = "Percent Trump") +
  coord_equal() +
  theme_statebins( legend_position = c(.2, 1) ) +
  theme( legend.direction="horizontal")
p2 + scale_fill_gradient(breaks = c(5, 21, 41, 48, 57),
                         labels = c("< 5", "5-21", 
                                    "21-41", "41-58", "> 57"),
                         low = "#f9ecec", high = "#CB454A") +
  guides(fill = guide_legend())
