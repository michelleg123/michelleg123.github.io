out <- lm(formula = lifeExp ~ gdpPercap + pop + continent,
          data = gapminder)
summary(out)

library(gapminder)
gapminder <- gapminder
out2 <- lm(formula = log(lifeExp) ~ log(gdpPercap)*continent + pop ,
           data = gapminder)
out2_aug <- augment(out2)
out2_conf <- tidy(out2, conf.int = T)

p2 <- ggplot(data = out2_aug,
             mapping = aes(x = .fitted, y = .resid))
p2 + geom_point(aes(color = continent), alpha = .33) +
  geom_hline(yintercept = 0, color = 'red', lty = 3) +
  geom_smooth(se = F)


fit_ols <- function(df) {
  lm(lifeExp ~ log(gdpPercap) + log(pop), data = df)
}
out_le <- gapminder %>%
  group_by(continent, year) %>%
  nest() %>% 
  mutate(model = map(data, fit_ols)) 
out_le


library(RColorBrewer)
library(tidyverse); library(socviz); library(RColorBrewer)
p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = world))

p + geom_point(size = 2) + scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")
p + geom_point(size = 2) + scale_color_brewer(palette = "Pastel2") +
  theme(legend.position = "top")

p + geom_point(size = 2, alpha = .5) + scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "top")

install.packages("dichromat")
library(dichromat)
Default <- brewer.pal(5, "Set2") # RColorBrewer::brewer.pal() makes the color palettes
types <- c("deutan", "protan", "tritan")
names(types) <- c("Deuteronopia", "Protanopia", "Tritanopia")
color_table <- types %>%
  purrr::map(~ dichromat(Default, .x)) %>%
  as_tibble() %>%
  add_column(Default, .before = TRUE)
color_table

brewer.pal.info


# Layer color and text together: 
party_colors <- c("#2E74C0", "#CB454A") # DEM Blue and REP Red
p0 <- ggplot(data = filter(county_data, flipped == "No"),
             mapping = aes(x = pop, y = black/100) )
p1 <- p0 + geom_point(alpha = 0.15, color = "gray50") +
  scale_x_log10(labels=scales::comma) 
p1

p2 <- p1 + geom_point(data = subset(county_data,
                                    flipped == "Yes"),
                      mapping = aes(x = pop, y = black/100,
                                    color = partywinner16)) +
  scale_color_manual(values = party_colors)
p2

p3 <- p2 + scale_y_continuous(labels=scales::percent) +
  labs(color = "County flipped to ... ",
       x = "County Population (log scale)",
       y = "Percent Black Population",
       title = "Flipped counties, 2016",
       caption = "Counties in gray did not flip.")
p3

p4 <- p3 + geom_text_repel(data = filter(county_data,
                                         flipped == "Yes" &
                                           black  > 25),
                           mapping = aes(x = pop,
                                         y = black/100,
                                         label = state), size = 2)
p4 + theme_minimal() + theme(legend.position="top")

library(ggthemes)
p4 + theme_economist() +
  theme(legend.position="top")

p4 + theme_wsj() +
  theme(plot.title = element_text(size = rel(0.6)),
        legend.title = element_text(size = rel(0.35)),
        plot.caption = element_text(size = rel(0.35)),
        legend.position = "top")

p4 + theme(legend.position = "top",
           plot.title = element_text(size=rel(2),
                                     lineheight=.5,
                                     family="Times",
                                     face="bold.italic",
                                     colour="orange"),
           axis.text.x = element_text(size=rel(1.1),
                                      family="Courier",
                                      face="bold",
                                      color="purple"))


yrs <- c(seq(1972, 1988, 4), 1993, seq(1996, 2016, 4))
mean_age <- gss_lon %>%
  filter( !is.na(age), year %in% yrs) %>%
  group_by(year) %>%
  summarize(xbar = round(mean(age, na.rm = TRUE), 0))
mean_age$y <- 0.3 
yr_labs <- data.frame(x = 85, y = 0.8, year = yrs)  # to position the age as a text label

p <- ggplot(data = filter(gss_lon, year %in% yrs),
            mapping = aes(x = age))
p1 <- p + geom_density(fill = "black", color = FALSE,
                       alpha = 0.9, mapping = aes(y = ..scaled..))
p1

p2 <- p1 + geom_vline(data = filter(mean_age, year %in% yrs),
                      aes(xintercept = xbar), color = "white", size = 0.5) + 
  geom_text(data = filter(mean_age, year %in% yrs),
            aes(x = xbar, y = y, label = xbar), nudge_x = 7.5,
            color = "white", size = 3.5, hjust = 1) +
  geom_text(data = filter(yr_labs, year %in% yrs),
            aes(x = x, y = y, label = year))

p3 <- p2  + facet_grid(year ~ ., switch = "y")
p3


p2a <- p3 + theme(plot.title = element_text(size = 16),
                  axis.text.x= element_text(size = 12),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  strip.background = element_blank(),
                  strip.text.y = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
  labs(x = "Age", y = NULL,
       title = "Age Distribution of\nGSS Respondents")
p2a


library(ggridges)
p <- ggplot(data = gss_lon,
            mapping = aes(x = age, 
                          y = factor(year, levels = rev(unique(year)), ordered = TRUE)))



p2b <- p + geom_density_ridges(alpha = 0.6, fill = "lightblue", scale = 1.5) +  
  scale_x_continuous(breaks = c(25, 50, 75)) +
  scale_y_discrete(expand = c(0.01, 0)) + 
  labs(x = "Age", y = NULL, title = "Age Distribution of\nGSS Respondents") +
  theme_ridges() +  # make labels aligned properly
  theme(title = element_text(size = 16, face = "bold"))
p2b

library(gridExtra)
grid.arrange(p2a, p2b, nrow = 1)   # sub-figures

studebt



p_xlab <- "Amount Owed, in thousands of Dollars"
p_title <- "Outstanding Student Loans"
p_subtitle <- "44 million borrowers owe a total of $1.3 trillion"
p_caption <- "Source: FRB NY"
# for as_labeller() in the facet_*()
f_labs <- c(`Borrowers` = "Percent of\nall Borrowers",
            `Balances` = "Percent of\nall Balances") 
p <- ggplot(studebt, aes(x = Debt, y = pct/100, fill = type) )


p + geom_bar(stat = "identity") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = FALSE) +
  theme(strip.text.x = element_text(face = "bold")) +
  labs(y = NULL, x = p_xlab,
       caption = p_caption,
       title = p_title,
       subtitle = p_subtitle) +
  facet_grid(~ type, labeller = as_labeller(f_labs)) +
  coord_flip()


library(viridis)
p <- ggplot(studebt, aes(y = pct/100, x = type, fill = Debtrc))
p1 <- p + geom_bar(stat = "identity", color = "gray80") 
p1
p2 <- p1 + scale_x_discrete(labels = as_labeller(f_labs)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) 
p2
p3 <- p2 + guides(fill = guide_legend(reverse = TRUE,
                                      title.position = "top",
                                      label.position = "bottom",
                                      keywidth = 3, nrow = 1)) +
  labs(x = NULL, y = NULL,
       fill = "Amount Owed, in thousands of dollars",
       caption = p_caption,
       title = p_title,
       subtitle = p_subtitle)
p3
p4 <- p3 + theme(legend.position = "top",
                 axis.text.y = element_text(face = "bold", hjust = 1, size = 12),
                 axis.ticks.length = unit(0, "cm"),
                 panel.grid.major.y = element_blank()) +
  coord_flip()
p4
