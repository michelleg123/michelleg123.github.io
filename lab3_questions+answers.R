# Lab session: 4/4/2023 

library(tidyverse)
library(socviz)
library(lubridate)
library(geofacet)
theme_set(theme_minimal()) 

# Lab 3 : 

# Question 1 
titanic <- read_csv(
  'https://bcdanl.github.io/data/titanic_cleaned.csv') 

colors = c("orange", "blue")

p <- ggplot(data = titanic, aes(x = sex, fill = sex))
p + geom_bar() + facet_grid(class~survived) + guides(fill = "none") + 
theme(strip.background.x = element_rect(fill = "gray"),
      strip.background.y = element_rect(fill = "gray"))


?guides


################################
# Choe's answers : 

titanic <- titanic %>% 
  mutate(surv = ifelse(survived == 0, "Died", "Survived")) 

ggplot(data = titanic,
       aes(x = sex, fill = sex)) + 
  geom_bar() +
  facet_grid(class ~ surv) +
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(limits = c(0, 195)) +
  scale_fill_manual(values = c("#D55E00D0", "#0072B2D0"), 
                    guide = "none") +
  theme_bw() + 
  theme(axis.text.y = element_text(margin = margin(7, 7, 7, 7))) +
  labs( y = "Number of passengers")











# Question 2 

nyc_flights <- read_csv(
  'https://bcdanl.github.io/data/nyc_flights_grouped.csv')

# reorder function

p1 <- ggplot(data = nyc_flights, aes(y = carrier_full)) 
p1 + geom_bar()

# In order to use re_order, we need numerical variables : 
q2 <- nyc_flights %>% 
  mutate()




########################
# Choe's answers ; 

# You can use count() OR tally()
# geom_bar(stat = identity) ... equal ... geom_col()

nyc_flights %>%
  group_by(carrier_full) %>%
  tally() %>% # since reorder can only take in numerical values 
  mutate(highlight = ifelse(carrier_full %in% c("Delta", "American"), "yes", "no")) %>%
  ggplot(aes(x=reorder(carrier_full, n), # variable "n" is from tally()
             y=n,
             fill = highlight)) + # fill the highlighted values 
  scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"),
                    guide = "none") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(name = NULL) +
  geom_col() +
  geom_text(aes(label = n), hjust = -.1, vjust = -.5,
            size = 4) +
  coord_flip(clip = "off") +
  theme_wsj() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ylim(c(0, 60000)) +
  
  # Note: for caption, "\n" changes the line 
  labs( caption = "Sources: U.S. Department of Transportation,\nBureau of Transportation Statistics",
        y = "Number of flights",
        title = "Number of flights from NYC",
        subtitle = "Year 2013")


# Question 3 
stock = read_csv('https://bcdanl.github.io/data/stocks2013_2023.csv') 


# Whenever using geom_line, do color mapping ! 
# Instead of using seq, you can manually map the labels into a vector. 


library(lubridate)
q3 <- stock %>% 
  group_by(company) %>% 
  mutate(normal_close =  1 * Close / first(Close) )

p <- ggplot(q3, aes(x = Date, y = log(normal_close), color = company))

p + 
  geom_line() +
  geom_hline(yintercept = 0, color = 'red', lty = 2) +
  scale_x_date(breaks = seq(as.Date(min(q3$Date)), as.Date(max(q3$Date)), by = "year")
  ) +
  ylim(c(min(log(q3$normal_close)), max(log(q3$normal_close)))) +
  labs(y = 'Normalized Closing Price (in log)') +
  theme(axis.text.x = element_text(angle = 45))


