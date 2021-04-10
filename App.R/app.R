library(shiny)
library(tidyverse)

Open_Athletes_2019 <- read.csv("2019_opens_athletes.csv", stringsAsFactors = T)
Open_Athletes_2020 <- read.csv("2020_opens_athletes.csv", stringsAsFactors = T)

Open_2019 <- Open_Athletes_2019 %>%
    select(competitorname, division, gender, overallscore, overallrank, postcompstatus) %>%
    filter(postcompstatus == "accepted") %>%
    arrange(overallrank)

Open_2019$Year <- 2019

Open_2020 <- Open_Athletes_2020 %>%
    select(competitorname, division, gender, overallscore, overallrank, postcompstatus) %>%
    filter(postcompstatus == "accepted") %>%
    arrange(overallrank)

Open_2020$Year <- 2020

Open_19vs20 <- rbind(Open_2019, Open_2020)
Open_19vs20$Year <- as.factor(Open_19vs20$Year)



# Define UI 
ui <- fluidPage(
    
    # Title Panel
    titlePanel("Crossfit Open"),
    
    # Side Bar
    sidebarLayout(sidebarPanel( 
        # Slider Input
        selectInput(inputId = "selectOne",
                    label = "Year:",
                    choices = c("2019", "2020")),
        selectInput(inputId = "selectTwo",
                    label = "Division:",
                    choices = c("Men", "Women", "Men (35-39)", "Men (40-44)",
                                "Men (45-49)", "Men (50-54)", "Men (55-59)",
                                "Men (60+)", "Women (35-39)", "Women (40-44)",
                                "Women (45-49)", "Women (50-54)",
                                "Women (55-59)", "Women (60+)")),
        sliderInput(inputId = "pointSize",
                    label = "Point Size:",
                    min = 0,
                    max = 10,
                    value = 3)
    ),
    # Main Panel
    mainPanel(
        plotOutput("myCrossfitPlot")
    )))


# Define server logic
server <- function(input, output) {
    
    output$myCrossfitPlot <- renderPlot({
        ggplot(data = Open_19vs20 %>% filter(Year == input$selectOne, division == input$selectTwo),
               mapping = aes(x = overallrank, y = overallscore, color = division, alpha = 0.2)) +
            geom_point(size = input$pointSize) +
            theme_bw() +
            guides(alpha = FALSE) +
            theme(axis.text.x = element_text(face = "bold", size = 8)) +
            theme(axis.text.y = element_text(face = "bold", size = 8))  +
            scale_y_continuous(labels = scales::number_format()) +
            scale_x_continuous(labels = scales::number_format()) +
            labs(x = "Overall Rank",
                 y = "Overall Score",
                 color = "Division",
                 title = "Overall Scores during The Crossfit Open",
                 subtitle = "2019 vs 2020",
                 caption = "Source: kaggle.com")
        
        
    })
}


# Run the application
shinyApp(ui = ui, server = server)