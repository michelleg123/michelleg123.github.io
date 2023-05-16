
# Original graph : 
p <- ggplot(data = cali_dat, aes(x = Distance, y = medianHouseValue)) + 
  geom_point()
p2 <- p + labs(x = "Distance to the Coast(miles)", y = "Median House Value(US Dollars)", 
               title = "Relationship Between Distance to the Coast and Median House Value")
p2





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

# install.packages("htmlwidgets")
library(htmlwidgets)
saveWidget(p2, "girafe_fig.html")


p <- ggplot(data = cali_dat, aes(x = Distance, y = medianHouseValue)) + 
  geom_point()
p2 <- p + labs(x = "Distance to the Coast(miles)", y = "Median House Value(US Dollars)", 
               title = "Relationship Between Distance to the Coast and Median House Value")
p2

library(plotly)
p <- ggplot(data = cali_dat, aes(x = Distance, y = medianHouseValue,
                           text = paste("Distance: ", round(Distance,digits = 3),"miles",
                                        "<br>Median House Value: $", medianHouseValue
                                        ))) +
              
              geom_point() + 
  labs(x = "Distance to the Coast(miles)", y = "Median House Value(US Dollars)", 
       title = "Relationship Between Distance to the Coast and Median House Value")

ggplotly(p, tooltip = "text")







p <- ggplot(df, aes(x = Date, y = Revenue/1000000, group = 1, 
                    text = paste("Date: ", format(Date, "%B %Y), 
"<br>Revenue: $", round(Revenue/1000000, digits = 1), "million", 
"<br>Target: $", round(Target/1000000, digits = 1), "million")
)) +
scale_y_continuous(labels=dollar_format(prefix="$",suffix=" m"))+
geom_line(colour = "grey", aes(Date, Target/1000000)) +
geom_line(colour = "#408FA6") +
                                                  ylab("Revenue")
                                                  ggplotly(p, tooltip = "text")
