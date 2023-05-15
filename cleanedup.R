# Necessary Code : 

cali_dat <- read.csv('california_housing.csv')

coasts <- read.csv('Coastal Borders_Migrated Data.csv')

distance <- function(lat, long){
  dist <- sqrt( ( (lat - coasts$Latitude)*69 )^2 + ( (long - coasts$Longitude)*54.6 )^2)
  return(min(dist, na.rm = T))
}

cali_dat$Distance <- 0

for(i in 1:nrow(cali_dat)){
  cali_dat$Distance[i] <- distance(cali_dat$latitude[i], cali_dat$longitude[i])
}

cali_dat <- cali_dat %>% 
  mutate(perc_br = totalBedrooms/totalRooms)


set.seed(84392)

d <- runif(nrow(cali_dat), 0, 1)


dtrain <- filter(cali_dat, d <= 0.3) 
dtest <- filter(cali_dat, d > 0.3)

# dtest$pred <- predict(mod, newdata = dtest) 


mod1 <- lm( medianHouseValue ~ Distance + medianIncome, 
            data = dtrain)

mod2 <- lm( medianHouseValue ~ Distance + medianIncome + population + households, 
            data = dtrain) 

mod3 <- lm( medianHouseValue ~ Distance + medianIncome + population + households + totalBedrooms + totalRooms,
            data = dtrain)

mod <- lm( medianHouseValue ~ Distance + medianIncome + population + households + perc_br + housingMedianAge,
           data = dtrain)

stargazer(mod1, mod2, mod3, mod)


{r, eval=T, results='asis', echo = F}
sum_cali <- cali_dat %>% 
  skim()
stargazer(sum_cali, type = 'html')


library(ggcorrplot)
corr <- round(cor(cali_dat), 2)

ggcorrplot(corr, lab = T, title = "Correlation Heat Map for California Housing Data")


