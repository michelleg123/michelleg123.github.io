# Looking at data transformation : 
library(tidyverse)
library(socviz)
rel_by_region <- gss_sm %>%
  group_by( bigregion, religion ) %>%
  summarize( N = n() ) %>%
  mutate( freq = N / sum(N),
          pct = round( (freq*100), 0) )
rel_by_region


p <- ggplot( rel_by_region, 
             aes( x = bigregion, 
                  y = pct, 
                  fill = religion))
p + geom_col( position = "dodge2" ) +
  labs(x = "Region", 
       y = "Percent", 
       fill = "Religion") +
  theme(legend.position 
        = "top")

# Replicate the graph:

# - small, multiple figures : facet_grod, columnwise
# - fill the bars with color 
# - Note that the theme looks minimal 
# - THe axis looks "Null" 

library(tidyverse)
library(hrbrthemes)
p <- ggplot(rel_by_region,
            aes(x = religion, y = pct, fill = religion ))
p + geom_col() + facet_grid( .~ bigregion) +
  coord_flip() + guides(fill = "none") + theme_ipsum() +
  labs(x = NULL, y = "Percent")

#############--------------------------------------------#####################
# Continuous variables by group or category :

# Look at the country-level variation : 

# Make sure to remove "NA" values 
organdata 
p <- ggplot(organdata, aes(x = reorder(country, donors, na.rm = T), y = donors )) 
p + geom_boxplot() + coord_flip()


# Reorder the levels using fct_reorder : 

# Replicate the graph 
# Note: we mapped "fill" to "world"
# If there was no "world" variable, we'd have to create a new variable. 
# Look up: case_when()

organdata <- organdata %>%
  mutate(country = fct_reorder(country, donors, na.rm = T) ) 
p <- ggplot(data = organdata,
            mapping = aes(x = country, y = donors, fill = world))
p + geom_boxplot() +
  labs(x = NULL) +
  coord_flip() + 
  theme(legend.position = 'top')

# dotplot 
# Countries are sorted by the mean value of their donors

p <- ggplot(data = organdata, mapping = aes(x = country, y = donors)) + 
  stat_summary(fun= mean)
p + coord_flip()

# Transform given data frame so that we have a summarized data frame 
# Mean and standard deviation for each pair 

by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize(donors_mean= mean(donors, na.rm = TRUE),
            donors_sd = sd(donors, na.rm = TRUE),
            gdp_mean = mean(gdp, na.rm = TRUE),
            health_mean = mean(health, na.rm = TRUE),
            roads_mean = mean(roads, na.rm = TRUE),
            cerebvas_mean = mean(cerebvas, na.rm = TRUE))
by_country

# A more efficient way to do it: 
# We only want to apply it to numerical variable

by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, list(mean = mean, 
                                sd = sd), 
               na.rm = TRUE) %>%
  ungroup()

# Replicate the following dotplot: 
p <- ggplot(data = by_country, mapping = aes( x = donors_mean, 
                                              y = reorder(country, donors_mean,
                                                          na.rm = T), 
                                              color = consent_law) ) 
p + geom_point() + 
  labs(x= "Donor Procurement Rate", y = NULL, 
       color = "Consent Law") + theme(legend.position = "top")

# Replicate the faceted dot plot             
p + geom_point() + 
  facet_grid(consent_law ~ . ) + 
  labs(x = "Donor Procurement Rate", y = NULL)

# Error Range 
# Draw the following dot-and-whisker plot 
p <- ggplot(data = by_country, mapping = aes( x = donors_mean, 
                                              y = reorder(country, donors_mean,
                                                          na.rm = T)) 
p + geom_pointrange( aes ( xmin = donors_mean - donors_sd, xmax = 
                             donors_mean + donors_sd))


###########_---------------######################
# Understanding Scales, Guides, and Themes

# try : guides = False 

p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = world))
p + geom_point()

# The x and y scales are both continuous.
# The color scale is discrete.
# A color or fill mapping can also be a continuous quantity (colorbar).


# Try to alter the original graph : 

# This one has to do with axis : 
p + geom_point() + 
  scale_y_continuous(breaks = c(5,15,25), 
                     labels = c("Five", "Fifteen", "Twenty Five"))

# This one has to do with legend: 
p + geom_point() + 
  labs(color = "Welfare State") + 
  scale_color_discrete(labels = c("Corporatist", "Liberal", "Social Democratic", 
                                  "Unclassified"))
  
