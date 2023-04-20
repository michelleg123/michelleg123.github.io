library(tidyverse)

plotly::ggplotly()
install.packages("plotly")
library(plotly)
dat <- data.frame(cond = rep(c("A", "B"), each = 10),
                  xvar = 1:20 + rnorm(20, sd=3),
                  yvar = 1:20 + rnorm(20, sd=3))
p <- ggplot(dat, aes(x = xvar, y = yvar)) +
  geom_point(shape=1)      # Use hollow circles
fig <- ggplotly(p)
fig

cces <- read_csv(url("https://bcdanl.github.io/data/cces.csv"))


p <- ggplot(cces, aes(x = seniority, y = les,
                      color = party))+
  geom_point()+
  scale_color_manual(values=c("blue","red")) +
  labs(x = "Seniority", y = "Leg. Effectiveness")
p1 <- ggplotly(p)

install.packages("htmlwidgets")
library(htmlwidgets)
saveWidget(p1, "fig.html")


install.packages("vctrs")

install.packages("rlang")

install.packages("ggiraph")
library(ggiraph)
data <- mtcars
data$carname <- row.names(data)
gg_point <- ggplot(data = data) +
  geom_point_interactive(aes(x = wt, y = qsec, 
                             color = disp,
                             tooltip = carname, 
                             data_id = carname)) + 
  theme_minimal()
gg_point

install.packages('gganimate')
library(gganimate)

install.packages("gifski")
install.packages("av")

library("gifski")
library("av")




# ------------------------------------------------------------------------
gapminder <- gapminder 

library(gapminder)
p <- gapminder %>%
  ggplot() + 
  geom_point(aes(x = gdpPercap, y = lifeExp, 
                 color = continent, size = pop), 
             alpha = 0.75) + 
  theme_minimal() + theme(legend.position = "top") + 
  guides(size = "none") + 
  labs(x = "GDP per Capita", y = "Life Expetancy", color = "")
p 

# animate(p, renderer = gifski_renderer())

library(gganimate)
p +
  transition_time(year)




p +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

p +
  geom_text(aes(x = min(gdpPercap), 
                y = min(lifeExp), 
                label = as.factor(year)) , 
            hjust=-2, vjust = -0.2, alpha = 0.2,  
            color = "gray", size = 20) +
  transition_states(as.factor(year), state_length = 0)


gapminder_sum <- gapminder %>%
  group_by(year) %>%
  arrange(year, desc(gdpPercap)) %>% # top 15 ranking for each year, based on gdpPercap
  mutate(ranking = row_number()) %>%
  filter(ranking <=15)


anim <- gapminder_sum %>%
  
  
  
  ggplot() +
  geom_col(aes(x = ranking, y = gdpPercap, fill = country)) +
  geom_text(aes(x = ranking, y = gdpPercap, label = round(gdpPercap, 0)), 
            hjust=-0.1) +
  geom_text(aes(x = ranking, y = 0 , 
                label = country), hjust=1.1) + 
  geom_text(aes(x = 15, 
                y = max(gdpPercap), 
                label = as.factor(year)), 
            vjust = 0.2, alpha = 0.5,  color = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) + 
  scale_x_reverse() +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(1, 4, 1, 3, "cm")
  ) +
  transition_states(year, state_length = 0, transition_length = 2) +
  enter_fade() +
  exit_fade() + 
  ease_aes('quadratic-in-out') 
p_anim <- animate(anim, 
                  width = 700, height = 432, 
                  fps = 25, duration = 15, 
                  rewind = FALSE)
p_anim
