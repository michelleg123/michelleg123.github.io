# Homework 2 draft file
library(tidyverse)
library(skimr)   # a better summary of data.frame
library(scales)  # scales for ggplot
library(ggthemes)  # additional ggplot themes
library(socviz)
library(gapminder)
library(ggrepel)
library(stargazer)
library(broom)
library(hrbrthemes)  # it provides theme_ipsum()
theme_set(theme_ipsum())


# Question 2a: 
hdi_corruption <- read_csv(
  'https://bcdanl.github.io/data/hdi_corruption.csv')


p0 <- ggplot(data = filter(hdi_corruption, year == 2014), aes(x = cpi, y = hdi) )
p1 <- p0 + geom_point(size = 3, alpha = 0.5, mapping = aes(color = region)) + 
  labs(x= "Corruption Perceptions Index, 2014(100 = least corrupt)",
       y = "Human Development Index, 2014 \n(1.0 = most developed)") + 
  guides(color = guide_legend(nrow = 1)) + 
  theme_minimal() +
  theme(legend.position = "top",
        axis.title.y = element_text(size = rel(1))) +
  scale_x_continuous(breaks = c(20,40,60,80)) + 
  scale_y_continuous(breaks = c(0.4,0.6,0.8,1.0), lim = c(0.3,1.0)) 

p1 + geom_text_repel(data = filter(hdi_corruption, country %in% c("Argentina", 
                                                              "China", "Egypt",
                                                               "Greece", "South Africa",
                                                             "Senegal",
                                                       "United States", "Germany",
                                                       "Singapore", "Norway"), year == 2014), 
                 aes(x= cpi, y = hdi, label = country ), color = "black", 
                 box.padding = 1) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
  



# Question 2b : 
path <- "C:\\Users\\darin\\OneDrive\\Documents\\DANL310\\michelleg123.github.io\\labor_supply.csv"
cps_labor <- read_csv(path)



# Use group_by and summarize?? 

q2b <- cps_labor %>% 
  mutate(child_status = if_else(NCHLT5 == 0, "No Child Under Age 5 In Household", "Having Children Under Age 5 in Household"), SEX = if_else(SEX == 1, "Male", "Female")) %>% 
  filter( LABFORCE != 0 ) %>% 
  group_by(YEAR, SEX, child_status) %>% 
  summarise(tot = sum(ASECWT, na.rm = T),
            lab = sum(ASECWT[LABFORCE == 2], na.rm = T),
            pct = 100 * lab / tot) 


colors <- c( "red", "blue")

p <- ggplot(data = q2b, aes(x = YEAR, y = pct, color = factor(SEX)))
p1 <- p + geom_line(size = 1.5) +  labs(y = "Labor Force Participation", 
                        title = "Fertility and Labor Supply in the U.S.", 
                        subtitle = "1982-2022") + scale_x_continuous(breaks=seq(1982, 2022, 4)) + 
  facet_wrap(~(child_status)) + 
    scale_y_continuous(labels= c("50%", "60%", "70%", "80%", "90%", "100%"), limits = (c(50.00, 100.00))) + 
  guides(color = "none")

p1 + scale_color_manual(values = colors) + 
  theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45)) + 
  geom_label_repel(data = q2b, 
                  aes(x= YEAR, y = pct, label = SEX), color = "black", 
                  box.padding = 1) 





library(skimr)
cps_labor %>% group_by(LABFORCE) %>% skim(ASECWT)
q2b %>% group_by(LABFORCE) %>% skim(ASECWT)
table(cps_labor$SEX)
table(cps_labor$SEX)
table(q2b$LABFORCE)
population = sum()

# If nchlt5 is >= 1 , we know they have at least 1 child under 5 
# if nchlt5 = 0 , we know they have no children under 5 






# Question 2c: 




library(ggcorrplot) # to create correlation heatmaps using ggcorrplot()

beer_mkt <- read_csv('https://bcdanl.github.io/data/beer_markets.csv')

beer_dummies <- beer_mkt %>% select(-hh, -market) 
reg <- lm(data = beer_dummies,
          beer_floz ~ .)
beer_dummies <-  as.data.frame(model.matrix(reg))[, -1] # 
# all variables except the (intercept) variable 
beer_dummies <- cbind(beer_mkt$beer_floz ,beer_dummies)
beer_dummies <- beer_dummies %>% 
  rename(beer_floz = `beer_mkt$beer_floz`)


correlation <- cor(beer_dummies)
df_correl <- as.data.frame(correlation)


reg <- lm(dollar_spent ~ ., data = beer_mkt)
df <- as.data.frame( model.matrix(reg) ) %>% 
  select(-contains('_purchase'), -hh, -contains('Intercept'), -starts_with('market'))

# if none of the other brands, then it's bud light. 
cor(beer_mkt)
df_cor <- as.data.frame( cor(df) ) 
library(ggcorrplot)
ggcorrplot(df_cor)


###############################################
# For NY markets: 
# Albany, Buffalo-Rochester, URBAN NY, SURBURBAN NY, SYRACUSE, RURAL NEW YORK, 
# EXURBAN NY 







library(ggcorrplot) # to create correlation heatmaps using ggcorrplot()

beer_mkt <- read_csv('https://bcdanl.github.io/data/beer_markets.csv')


beer_dummies <- beer_mkt %>% select(-hh, -market) 
reg <- lm(data = beer_dummies,
          beer_floz ~ .)
beer_dummies <-  as.data.frame(model.matrix(reg))[, -1]
beer_dummies <- cbind(beer_mkt$beer_floz ,beer_dummies)
beer_dummies <- beer_dummies %>% 
  rename(beer_floz = `beer_mkt$beer_floz`)
df <- beer_dummies %>% 
  select(-contains('_purchase'), -contains('Intercept'), -starts_with('market'))



library(ggcorrplot) # to create correlation heatmaps using ggcorrplot()

beer_mkt <- read_csv('https://bcdanl.github.io/data/beer_markets.csv')

beer_mkt1 <- beer_mkt %>%
  select(brand:buyertype, children6to17, age, employment) %>%
  mutate(NRB = if_else( container == 'NON REFILLABLE BOTTLE', 1, 0),
         NaturalLight = if_else( brand == 'NATURAL LIGHT', 1, 0),
         BudLight = if_else( brand == 'BUD LIGHT', 1, 0),
         BuschLight = if_else( brand == 'BUSCH LIGHT', 1, 0),
         CoorsLight = if_else( brand == 'COORS LIGHT', 1, 0),
         MillerLite = if_else( brand == 'MILLER LITE', 1, 0),
         buyertypemarried = if_else( buyertype == 'married', 1, 0),
         employmentnone = if_else( employment == 'none', 1, 0),
         age50 = if_else( age == '50+', 1, 0),
         promotrue = if_else( promo == 'TRUE', 1, 0),
         children6to17true = if_else( children6to17 == 'TRUE', 1, 0)) %>%
  select(is.numeric)


corr <- round(cor(beer_mkt1), 2)


ggcorrplot(corr, lab = T)


