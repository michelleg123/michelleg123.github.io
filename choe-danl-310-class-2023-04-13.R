library(tidyverse)
# install.packages("plotly")
library(plotly)
dat <- data.frame(cond = rep(c("A", "B"), each = 10),
                  xvar = 1:20 + rnorm(20, sd=3),
                  yvar = 1:20 + rnorm(20, sd=3))
p <- ggplot(dat, aes(x = xvar, y = yvar)) +
  geom_point(shape=1)      # Use hollow circles
fig <- ggplotly(p)
fig


cces <- read_csv(url("https://bcdanl.github.io/data/cces.csv"))
cces <- cces %>% 
  mutate(party = recode(dem, `1` = "Democrat", `0` = "Republican"))

p <- ggplot(cces, aes(x = seniority, y = les,
                      color = party))+
  geom_point()+
  scale_color_manual(values=c("blue","red")) +
  labs(x = "Seniority", y = "Leg. Effectiveness")
p
p1 <- ggplotly(p)
p1

# install.packages("htmlwidgets")
library(htmlwidgets)
saveWidget(p1, "fig.html")




# ggiraph -----------------------------------------------------------------

# install.packages("ggiraph")
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
p2 <- girafe(ggobj = gg_point)
p2
saveWidget(p2, "girafe_fig.html")




# gganimate ---------------------------------------------------------------
library(tidyverse)
data(mtcars)
p <- ggplot(data = mtcars,
            mapping = aes(x = factor(cyl), y = mpg)) +
  geom_boxplot()
p
p + facet_wrap(~gear)

library(gganimate)
my_anim <- p + 
  transition_states(gear)
my_anim

table(mtcars$gear)

my_anim2 <- p + 
  transition_states(gear,
                    transition_length = 3,  # relative length
                    state_length = 2)  # relative length
my_anim2




cces <- read_csv(url("https://bcdanl.github.io/data/cces.csv"))
cces <- cces %>% 
  mutate(party = recode(dem,`1`="Democrat",`0`="Republican"))
cong_dat <- cces %>% 
  group_by(year, party) %>%
  summarise( seats =n() )

p <- ggplot(cong_dat, 
            aes(x = year, y= seats, 
                fill = party)) +
  geom_col() +
  geom_hline(yintercept = 217) +  # threshold for having a majority of seats in the house.
  scale_fill_manual(values=c("blue","red"))
p


ggplot(cong_dat, 
       aes(x = year, y= seats, 
           color = party)) +
  geom_line()

anim2 <- p + transition_time(year)
anim2




p <- ggplot() + 
  geom_jitter(data = filter(cces, congress==115 & party=="Democrat"),
                            aes(x = seniority, y = all_pass,
                                color = party) ) +
  geom_jitter(data = filter(cces, 
                            congress==115 & party=="Republican"),
              aes(x = seniority, y = all_pass,
                  color = party) ) +
  geom_smooth(data = filter(cces, 
                            congress==115 & party=="Democrat"),
              aes(x = seniority, y = all_pass,
                  color = party) ) +
  geom_smooth(data = filter(cces, 
                            congress==115 & party=="Republican"),
              aes(x = seniority, y = all_pass,
                  color = party) ) +
  scale_color_manual(values=c("blue","red"))
p

anim2 <- p + transition_layers()
anim2


anim <- ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  transition_states(factor(cyl))
# Fade-in, fade-out
anim1 <- anim +
  enter_fade() +
  exit_fade()
anim1
# There are also shadow_mark(), shadow_null(), and shadow_trail().

p <- ggplot(cong_dat,
            aes(x = year, y = seats, fill = party)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 217) +
  scale_fill_manual(values = c("blue","red"))
anim3 <- p + transition_time(year) +
  shadow_wake(wake_length = 1,
              alpha = TRUE,
              wrap = TRUE)
anim3



# gapminder animation -----------------------------------------------------


library(gapminder)
gapminder <- gapminder
p <- gapminder %>%
  ggplot() + 
  geom_point(aes(x = gdpPercap, y = lifeExp, 
                 color = continent, size = pop), 
             alpha = 0.75) + 
  theme_minimal() + theme(legend.position = "top") + 
  guides(size = "none") + 
  labs(x = "GDP per Capita", y = "Life Expetancy", color = "")
p


# library(gganimate)
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
  transition_states(as.factor(year), 
                    state_length = 0)


gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(x = year, y = pop)) + 
  geom_point() + geom_line() +
  theme_minimal() +
  transition_reveal(year)



gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(x = year, y = pop)) + 
  geom_point() + geom_line() + 
  geom_text(aes(x = min(year), y = min(pop), 
                label = as.factor(year)) , 
            hjust=-2, vjust = -0.2, alpha = 0.5,  
            color = "gray", size = 20) +
  theme_minimal() +
  transition_reveal(year) + 
  view_follow()





# bar chart race ----------------------------------------------------------

gapminder_sum <- gapminder %>%
  group_by(year) %>%
  arrange(year, desc(gdpPercap)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <=15)



anim <- gapminder_sum %>%
  ggplot() +
  geom_col(aes(x = ranking, y = gdpPercap, fill = country)) +
  guides(fill = "none") +
  facet_wrap(year ~.)
anim


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

# p_anim <- p_anim  + exit_shrink()
anim_save("first_saved_animation.gif", animation = p_anim)
