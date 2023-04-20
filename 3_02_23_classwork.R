# Working with models 
library(tidyverse)
library(gapminder)
library(stargazer)
library(broom)
library(socviz)
gapminder <- gapminder

# Note: timeseries is very important in publication-worthy analysis 

# Linear Regression 
# output ~ input(s)
out <- lm(formula = lifeExp ~ gdpPercap + pop + continent,
          data = gapminder)
summary(out)
# stargazer(out, type = "html")

min_gdp <- min(gapminder$gdpPercap)
max_gdp <- max(gapminder$gdpPercap)
med_pop <- median(gapminder$pop)
pred_df <- expand.grid(gdpPercap = (seq(from = min_gdp,
                                        to = max_gdp,
                                        length.out = 100)), # limit observations
                                                          # to 100 
                       pop = med_pop,
                       continent = c("Africa", "Americas",
                                     "Asia", "Europe", "Oceania"))
dim(pred_df); head(pred_df)

# How can we go about training the data for machine learning? 
# Pick a specific time range... 

pred_out <- predict(object = out,
                    newdata = pred_df, # need new data to fit to the model. 
                    # make a prediction 
                    interval = "predict")
head(pred_out)
class(pred_out)

pred_df <- cbind(pred_df, pred_out)
head(pred_df)
as_tibble(pred_out)


# for geom_line and geom_ribbon, we wanna use the predicted data frame 
# Note: we are filtering the data frame so we just see Europe and Africa 
# The graph appears to be non-linear because we are using a log scale

p <- ggplot(data = filter(pred_df, continent %in% c("Europe", "Africa")),
            aes(x = gdpPercap, 
                y = fit, ymin = lwr, ymax = upr,
                color = continent, fill = continent, group = continent))
p + geom_point(data = filter(gapminder,
                             continent %in% c("Europe", "Africa")),
               aes(x = gdpPercap, y = lifeExp,
                   color = continent),
               alpha = 0.5,
               inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) + # the area covered by the prediction intervals
  scale_x_log10(labels = scales::dollar)
table(gapminder$continent)

# Why did we drop "Africa" when looking at our linear regression model??
# Because the dummy variable is dropped and that variable becomes the reference 
# level


# Tidy model objects with broom : 
out_comp <- tidy(out)
out_comp %>% round_df()
out_comp

p <- ggplot(out_comp, mapping = aes(x = term,
                                    y = estimate))
p + geom_point() + coord_flip

out_conf <- tidy(out, conf.int = TRUE) # shows confidence intervals..95%
out_conf %>% round_df()

# For presentations, it might be more useful to visualize 
# the beta estimates 

p <- ggplot(out_conf, mapping = aes(x = term,
                                    y = estimate,
                                    ymin = conf.low, 
                                    ymax = conf.high))
p + geom_pointrange() + coord_flip()

# Get component-level stats: 
out_conf <- filter(out_conf, term != "(Intercept)")
out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")


# Being in Oceania or Europe...associated with higher life expectancy 
# Remember reference level is Africa
p <- ggplot(out_conf, mapping = aes(x = reorder(nicelabs, estimate),
                                    y = estimate, ymin = conf.low, ymax = conf.high))
p + geom_pointrange() + coord_flip() + labs(x="", y="OLS Estimate")

# Get observation-level stats: 
out_aug <- augment(out)
head(out_aug)

# Create a standard regression plot 
# residual plot : 
p <- ggplot(data = out_aug,
            mapping = aes(x = .fitted, y = .resid))
p + geom_point()

# There seems to be a pattern...
p <- ggplot(data = out_aug,
            mapping = aes(x = .fitted, y = .resid))
p + geom_point(aes(color = continent), alpha = .33)
# Note: whenever we see something like this(a systematic pattern), 
# it means model is NOT doing well. Let's try to improve this model. 

glance(out) %>% round_df()
summary(out)

# trying to improve model : 
out2 <- lm(formula = log(lifeExp) ~ gdpPerCap*continent + pop, 
           data = gapminder )
# check the mean squared error 
# 
