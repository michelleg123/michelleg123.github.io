library(gapminder)
out <- lm(formula = lifeExp ~ gdpPercap + pop + continent,
          data = gapminder)
summary(out)
out_conf <- filter(out_conf, term != "(Intercept)")
out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")


out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")

p <- ggplot(out_conf, mapping = aes(x = reorder(nicelabs, estimate),
                                    y = estimate, ymin = conf.low, ymax = conf.high))
p + geom_pointrange() + coord_flip() + labs(x="", y="OLS Estimate")


# Get observation-level stats with augment()
out_aug <- augment(out)
head(out_aug)
# Note: Data is pretty much for the same for each continent 

p <- ggplot(data = out_aug,
            mapping = aes(x = .fitted, y = .resid))
p + geom_point()


glance(out) %>% round_df()
# We want to maximize the likelihood of the data 
summary(out)

# Check out the data. Grouped analysis and 

eu77 <- gapminder %>% filter(continent == "Europe", year == 1977)
fit <- lm(lifeExp ~ log(gdpPercap), data = eu77)
summary(fit)

p <- ggplot(data = filter(gapminder, continent != "Oceania"), aes(x = log(gdpPercap), y = lifeExp, color = continent))

p + geom_point() + geom_smooth(method = lm) + 
  facet_grid(year~continent)


# Nested Data Frame : 
out_le <- gapminder %>%
  group_by(continent, year) %>%
  nest()
out_le


# Look at a specific group, such as Europe 1977 : 
out_le %>% filter(continent == "Europe" & year == 1977) %>% 
  unnest()


# List columns are useful 
fit_ols <- function(df) {
  lm(lifeExp ~ log(gdpPercap), data = df)
}
out_le <- gapminder %>%
  group_by(continent, year) %>%
  nest() %>% 
  mutate(model = map(data, fit_ols)) 
out_le




# Tidyverse's way of doing for loop without actually doing a for loop. 

fit_ols <- function(df) {
  lm(lifeExp ~ log(gdpPercap), data = df)
}
out_tidy <- gapminder %>%
  group_by(continent, year) %>%
  nest() %>% 
  mutate(model = map(data, fit_ols),
         tidied = map(model, tidy)) %>%
  unnest(tidied, .drop = TRUE) %>%
  filter(term != "(Intercept)" &
           continent != "Oceania")


# Confidence Interval Plot: 
# We now have tidy regression output with an estimate of 
# the association between log GDP per capita and life expectancy 
# for each year, within continents.

p <- ggplot(data = out_tidy,
            mapping = aes(x = year, y = estimate,
                          ymin = estimate - 2*std.error,
                          ymax = estimate + 2*std.error,
                          group = continent, color = continent))
p + geom_pointrange(position = position_dodge(width = 1)) +
  scale_x_continuous(breaks = unique(gapminder$year)) + 
  theme(legend.position = "top") +
  labs(x = "Year", y = "Estimate", color = "Continent")
