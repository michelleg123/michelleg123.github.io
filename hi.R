library(lubridate)
gapminder <- gapminder


# Q1a 
q1a <- gapminder %>% 
  filter(year == 2007, continent != "Oceania") %>% 
  group_by(continent, country, lifeExp) %>% 
  summarize() %>% 
  mutate(country = fct_reorder(country, lifeExp, na.rm = T))
  
p <- ggplot(data = q1a, aes(x = lifeExp, y = country))
p + geom_point(color = "#0072B2") + labs(
                                         title = "Life expectancy in 2007") + 
  theme_minimal() +  theme(plot.title = element_text(face = "bold") + 
  facet_grid(continent~ . ) 

#Q1b 

# Africa generally has the lowest life expectancy. Asia and Europe have the 
# highest reports of life expectancy. 

  
  
  
  
# Question 2 : 


n_tweets_long <- read_csv(
  'https://bcdanl.github.io/data/n_tweets_long.csv')


q2 <- n_tweets_long
p1 <- ggplot(data = q2, aes(x = year, y = n))
p2 <- p1 + geom_bar(data = filter(type %in% c("n_ot_us", "n_ot_wrld"))) + 
  geom_line(data = filter(type %in% c("n_rt_lk_us", "n_rt_lk_wrld")))





# 2b : 
# By 2017, the number of retweets/likes in the US matched the number of tweets 
# made worldwide. 


# Question 3 


# Q3a 
nyc_dog_license <- read_csv(
  'https://bcdanl.github.io/data/nyc_dog_license.csv')
nyc_zips_coord <- read_csv(
  'https://bcdanl.github.io/data/nyc_zips_coord.csv')
nyc_zips_df <- read_csv(
  'https://bcdanl.github.io/data/nyc_zips_df.csv')


q3a <- left_join(nyc_zips_coord, nyc_zips_df, by = "objectid")
q3a_1 <- nyc_dog_license %>% 
  group_by(breed_rc, zipcode, ) %>% 
  summarize(perc = sum(breed_rc == "PitBull or Mix") / sum(breed_rc, na.rm= T) ) %>% 
  filter(!is.na(breed_rc))


p1 <- ggplot(data = q3a_1) + 
  geom_polygon(mapping = aes(x = X, y = Y,
                             group = group, 
                             fill = perc),
               color = "grey60", 
               linewidth = 0.1) 
p2 <- p1 + scale_fill_gradient(low = "black", high = "yellow")

coord_map(projection = "albers", lat0 = 39, lat1 = 45)


#Q3b : I would need the proper data frame to get this answer. 



# Question 4 : 

# 4a

# I don't have working code but these are my ideas: 
stock <- read_csv('https://bcdanl.github.io/data/stocks2013_2023.csv')

q4a <- stock %>% 
  filter(
    Date >= ymd("2019-01-01")) %>% 
  mutate(Volume = log(Volume)) %>% 
  mutate(year = c(2019, 2020, 2021, 2022)) %>% 
  group_by(company, year)


colors <- c("red", "yellow", "green", "lightblue", "blue", "purple", "pink")

p <- ggplot(data = q4a,aes(x = Volume, y = Close)) + labs(x = "Volume(in log)", y = "Close(in log)")
 facet_wrap(year~company) 
p2 <- p + 
 geom_point(alpha = 0.5, size = 0.5) + 
  geom_smooth()

# Q4b 

#  NFLX seems the most insensitive. 
