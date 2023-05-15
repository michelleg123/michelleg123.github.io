library(tidyverse)
library(ggthemes)
library(Hmisc)
library(viridisLite)
dat <- read.csv('home_sales_nyc.csv')

ggplot(dat) +
  geom_histogram(aes(sale.price))

ggplot(dat) +
  geom_histogram(aes(log(sale.price))) 


shapiro.test(log(sample_n(dat, 10000)[,12]))
ks.test(log(dat[,12]), 'pnorm')





us_states <- map_data('county')

cali_dat <- read.csv('california_housing.csv')

ggplot(cali_dat) +
  geom_histogram(aes(medianHouseValue))

# Log-transformed data : 
ggplot(cali_dat) +
  geom_histogram(aes(log(medianHouseValue))) 


# Map of California with Median House Value : 
la <- c('LA', -118.15, 34.03)
sf <- c('San Francisco', -122.24, 37.46)
sd <- c('San Diego',-117.16, 32.72 )
sb <- c('Santa Barbara', -119.70, 34.42)

locs <- as.data.frame(matrix(c(la, sf, sd, sb), nrow = 4, byrow = T))
colnames(locs) <- c('place','long','lat')

# Median House Prices : 

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

# Median Income 

ggplot(cali_dat) +
  geom_polygon(data = filter(us_states, region == 'california'),
               mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(aes(x = longitude, y = latitude, color = medianIncome, size = perc_br), alpha = .5) +
  coord_fixed() +
  theme_map() +
  theme(legend.position = "right") +
  scale_color_continuous(type = 'viridis') +
  labs(color = 'Median Income', size = 'Percentage of Bedrooms', title = 'Median Income and Percentage of Bedrooms') +
  ggrepel::geom_text_repel(data = locs, aes(x = as.numeric(long), y = as.numeric(lat), label = place), hjust = 3, 
                           vjust = 3)



# Hypothesis: More money, big houses ! 



#+
#  scale_color_gradient2(breaks = c(100000,200000,300000,400000,500000),
#                        labels = c('$100,000','$200,000','$300,000','$400,000','$500,000'))






hist.data.frame(cali_dat)


## Coast map stuff ----------


coasts <- read.csv('Coastal Borders_Migrated Data.csv')

ggplot(coasts) +
  geom_polygon(aes(x = Longitude, y = Latitude, group = Sub.Polygon.Id), fill = 'white', color = 'black' ) +
  coord_fixed() +
  theme_map()

ggplot(coasts) +
  geom_point(aes(x = Longitude, y = Latitude), size = .3) +
  theme_classic()




## Function for distance to coast ------
distance <- function(lat, long){
  dist <- sqrt( ( (lat - coasts$Latitude)*69 )^2 + ( (long - coasts$Longitude)*54.6 )^2)
  return(min(dist, na.rm = T))
}


lon <- -120

lat <- 40

distance(lat, lon) # should get roughly 156




cali_dat <- read.csv('california_housing.csv')

## Creates distance entry
cali_dat$Distance <- 0

for(i in 1:nrow(cali_dat)){
  cali_dat$Distance[i] <- distance(cali_dat$latitude[i], cali_dat$longitude[i])
}

# Plotting distance to the coast and medianHouseValue 

hist(cali_dat$Distance)


plot(cali_dat$Distance, cali_dat$medianHouseValue)



plot(cali_dat$Distance, log(cali_dat$medianHouseValue))


cali_dat <- cali_dat %>% 
  mutate(perc_br = totalBedrooms/totalRooms)

## Models and testing/training data -------

set.seed(84392)

d <- runif(nrow(cali_dat), 0, 1)

dtrain <- filter(cali_dat, d <= 0.3) 
dtest <- filter(cali_dat, d > 0.3)


mod <- lm( medianHouseValue ~ Distance + medianIncome + population + households + totalBedrooms + totalRooms + housingMedianAge,
           data = dtrain)

summary(mod)

# Look at the complete residual plot : 
plot(mod)



dtest$pred <- predict(mod, newdata = dtest)

# Individual Variables : 

p <- ggplot(dtest, aes(y = medianHouseValue - pred))


p + geom_point(aes(x = Distance))

p + geom_point(aes(x = totalRooms))

p + geom_point(aes(x = housingMedianAge))

p + geom_point(aes(x = totalBedrooms))

p + geom_point(aes(x = population))

p + geom_point(aes(x = households))

p + geom_point(aes(x = pred))


summary(mod_loc)


# new models

mod_loc <- lm( log(medianHouseValue) ~ log(medianIncome) * ( log(Distance) + 
             log(population) + log(households) + log(totalBedrooms) + 
               log(totalRooms) + log(housingMedianAge) +
             longitude + latitude),
           data = dtrain)

plot(mod_loc)


# models : 
# Save for later : 

dtrain <- dtrain %>% 
  mutate(alt_dist = 1/Distance^2) 
dtest <- dtest %>% 
  mutate(alt_dist = 1/Distance^2)

ggplot(dtrain) +
  geom_point(aes(x = alt_dist, y = medianHouseValue))

mod1 <- lm( medianHouseValue ~ log(alt_dist),
           data = dtrain)

plot(mod1, which = 1 )

summary(mod1)




# Linear Regression Models: 

mod1 <- lm( medianHouseValue ~ Distance + medianIncome, 
             data = dtrain)
# plot(mod1, which = 1)
summary(mod1)

ggplot(mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot for Model 1', x='Fitted Values', y='Residuals')

mod2 <- lm( medianHouseValue ~ Distance + medianIncome + population + households, 
            data = dtrain) 
# plot(mod2, which = 1)
summary(mod2)


ggplot(mod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot for Model 2', x='Fitted Values', y='Residuals')

mod3 <- lm( medianHouseValue ~ Distance + medianIncome + population + households + totalBedrooms + totalRooms,
           data = dtrain)
# plot(mod3, which = 1)
summary(mod3)


ggplot(mod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot for Model 3', x='Fitted Values', y='Residuals')

mod <- lm( medianHouseValue ~ Distance + medianIncome + population + households + perc_br + housingMedianAge,
           data = dtrain)

summary(mod)

ggplot(mod, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot for Final Model', x='Fitted Values', y='Residuals')





dtrain$pred <- predict(mod, newdata = dtrain)

sqrt(dtrain$pred - dtrain$)


# Relationship between Distance to the Coast: 
# Clearly, further away = less expensive 


p <- ggplot(data = cali_dat, aes(x = Distance, y = medianHouseValue)) + 
  geom_point()
p2 <- p + labs(x = "Distance to the Coast(miles)", y = "Median House Value(US Dollars)", 
               title = "Relationship Between Distance to the Coast and Median House Value")
p2


p3 <- p2 + scale_y_continuous(labels=scales::percent) +
  labs(color = "County flipped to ... ",
       x = "County Population (log scale)",
       y = "Percent Black Population",
       title = "Flipped counties, 2016",
       caption = "Counties in gray did not flip.")
p3



install.packages("stargazer")
library(stargazer)


stargazer(mod1, mod2, mod3, mod)
