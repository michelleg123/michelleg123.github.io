# Test file for all ggplot questions : 
library(tidyverse)
library(lubridate)
library(scales)
ncdc_temp <- read_csv(
  'https://bcdanl.github.io/data/ncdc_temp_cleaned.csv')


# Question 1 
theme_set(theme_minimal())
p <- ggplot( data = ncdc_temp, aes(x = month, y = temperature, color = location
             )) 
  
p <- ggplot(ncdc_temp, aes(x = date, y = temperature, color = location))
p + geom_line(size = 1.1) +
  scale_x_date(labels = date_format("%b")) +
  labs(x = "month", y = "temperature (°F)")


# Question 2 
theme_set(theme_classic())
p <- ggplot( data = ncdc_temp, aes(x = factor(month), y = temperature))
p + geom_boxplot(fill = "lightgray") +
  labs(x = "month", y = "mean temperature(°F)")  + theme(panel.grid.major = element_line(linetype = "dotdash"))

# Question 3 
# install.packages("ggridges")
theme_set(theme_classic())
ggridges::geom_density_ridges()
p <- ggplot( data = ncdc_temp, aes(x = temperature, y = factor(month)))
p + ggridges::geom_density_ridges(color = "white", fill = rgb(72/255, 164/255, 227/255) )+ 
  labs(x = "mean temperature(F)", y = "month") 


# Question 4 
theme_set(theme_minimal())
mtcars <- mtcars
p <- ggplot(data = mtcars, aes(x = disp, y = mpg, color = hp))
p + geom_point() + 
  labs(x = "displacement(cu. in.)", y = "fuel efficiency(mpg)")

# Question 5 
popgrowth_df <- read_csv(
  'https://bcdanl.github.io/data/popgrowth.csv')
p <- ggplot(data = popgrowth_df, aes(x = popgrowth , y = reorder(state,popgrowth), fill = 
                                       region))
p + geom_col() + 
  scale_x_continuous(labels = scales::percent) + 
  theme(legend.position = c(.7,.35)) + 
  labs(x = "population growth, 2000 to 2010") 
 


# Question 6 
theme_set(theme_minimal())
male_Aus <- read_csv(
  'https://bcdanl.github.io/data/aus_athletics_male.csv')
p <- ggplot(data = male_Aus, aes(x = height, y = pcBfat, fill = sport, shape = sport )) 
p + geom_point(alpha = 0.8, size = 3) + 
  scale_shape_manual(values = 21:25)+
  scale_fill_manual(values = c(rgb(188/255,77/255,72/255), 'grey','grey','grey','grey'))+
  labs(x = "height (cm)", y = "% body fat")


# Question 7 
titanic <- read_csv(
  'https://bcdanl.github.io/data/titanic_cleaned.csv')
titanic <- titanic %>%
  mutate(all_passengers = age)  
p <- ggplot(data = titanic, aes(x = age, y = after_stat(count), 
                                fill = sex))
p + geom_density() +
  geom_density(data = titanic,  aes(x = age, y = after_stat(count)))+
  facet_wrap(~sex, nrow = 1) +
  theme_minimal() +
  labs(x = "passenger age (years)") +
  theme(legend.position = 'bottom', legend.title=element_blank())

# Question 8 
cows_filtered <- read_csv(
  'https://bcdanl.github.io/data/cows_filtered.csv')

p <- ggplot(cows_filtered, aes(butterfat/100, color = breed, fill = breed))
p + geom_density(alpha = 0.2) +
  scale_x_continuous(labels = scales::percent) + 
  labs(x = 'butterfat contents')

