# Classwork : 
library(tidyverse)
library(skimr)   # a better summary of data.frame
library(scales)  # scales for ggplot
library(ggthemes)  # additional ggplot themes
theme_set(theme_minimal()) # setting the minimal theme for ggplot
# setting default chunk options
knitr::opts_chunk$set(echo = T, eval = T,
                      message=F, warning = F) 

? organdata
organdata
skim(organdata$donors)
skim(organdata$roads)

# Replicate the following : 

# How road accidents are related to donor rate. What does that have to do 
# with political parties? 

p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = world))
p <- p + geom_point() 
p + scale_y_continuous(breaks = seq(5,25,10), 
                       label = c("Five", "Fifteen", "Twenty Five"))

# Same data but different labels : 

p + scale_color_discrete(labels = c("C", "L", "S", "U")) + 
  labs(color = "Welfare State", 
       x = "Road Deaths", y = "Donor Procurement")

# Informed and Presumed 
# We need to use a facet function 
# Has two rows based on values of "consent law"
p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = consent_law))
 
p + geom_point() + facet_wrap(~consent_law , ncol = 1 ) + 
  labs(x = "Road Deaths", y = "Donor Procurement") + 
  guides(color = "none")

# By Consent Law 
p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = consent_law))
p + geom_point() + 
  theme(plot.title = element_text(face = "bold", 
                                  color = "brown", 
                                  size = rel(2)), 
        legend.position = "bottom") + 
  labs(title = "By Consent Law")


############__________________###################_________________________####
# Add Labels and Make Notes : 
# We can plot the labels along with the points since we want information 
by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, list(mean = mean, 
                                sd = sd), 
               na.rm = TRUE) %>%
  ungroup()
p <- ggplot(data = by_country,
            mapping = aes(x = roads_mean, y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country))

# We can move the text so that it's not directly on the point. 
p + geom_point() + geom_text(mapping = aes(label = country), hjust = 0, vjust = 0) 
p + geom_point() + geom_text_repel(mapping = aes(label = country)) 
# Presidential Elections 
install.packages("ggrepel")
library(ggrepel)
ggrepel::geom_text_repel()
library(socviz)      
elect <- socviz::elections_historic %>% select(2:7) 


# Step 1 : 
# Let's map aestetics to variables : 
p <- ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                                    label = winner))
# Step 2: 
# Add geometric objects to ggplot:
# Note: we need to add the two axes
p <- p + 
  geom_hline(yintercept = 0.5, linewidth = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, linewidth = 1.4, color = "gray80") +
  geom_point() +
  geom_text_repel()
p

# Step 3 : 

# Step 4: 


# Label Outliers : 

# sometimes we want to pick out some points of interest
# Use the filter function 

p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))
p + geom_point() +
  geom_text_repel(data = subset(by_country, gdp_mean > 25000),
                  mapping = aes(label = country))

p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))
p + geom_point() +
  geom_text_repel(data = filter(by_country,
                                gdp_mean > 25000 | health_mean < 1500 |
                                  country %in% "Belgium"),
                  mapping = aes(label = country))


# Let's replicate the follwoing using 
mtcars <- mtcars 
mtcars$name <- rownames(mtcars)
p <- ggplot(data = mtcars, mapping = aes(x = wt, y = mpg))
p + geom_point(color = "red") + 
  geom_text_repel(aes(label = name), data = filter(mtcars, wt< 5, mpg < 15))

