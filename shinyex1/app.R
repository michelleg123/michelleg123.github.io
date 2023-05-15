#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(viridis)
faithful <- faithful

# Color Palette 
# Change theme to theme_minimal()



# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 60,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = heat.colors(5), border = 'white', 
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      ylab = 'Count',
        #      main = 'Histogram of waiting times')
      ggplot(faithful, aes(x=waiting)) + 
        geom_histogram(fill =  viridis(input$bins, option = "B"),
                       bins = input$bins
                       ) + 
        labs( x = 'Waiting time to next eruption (in mins)', y = 'Count', 
              title = 'Histogram of waiting times') + 
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5))
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
