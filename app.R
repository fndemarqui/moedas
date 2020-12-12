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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simulação de lançamentos de uma moeda"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "prob",
                        label = "Probabilidade de face cara:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            numericInput(inputId = "n",
                         label = "Número de lançamentos",
                         value = 10),
            actionButton("lancar", "Lançar!!!"),
            
            textInput(inputId = "codigo",
                      label = "Código disponível em:",
                      value = "https://github.com/fndemarqui/moedas/blob/master/app.R ")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    resultado <- reactive({
        input$lancar
        resultado <- tibble(
            "lançamento" = 1:input$n,
            face = sample(faces, 
                          size = input$n, 
                          prob = c(input$prob, 1 - input$prob), 
                          replace = TRUE)
        )
    })
        
        
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        ggplot(resultado(), aes(x = face, y = ..prop.., group = 1)) +
            geom_bar() +
            geom_text(stat = 'count', 
                      aes(label = 100*round(..prop.., 3)), vjust = -0.5) +
            ylab("probabilidade de face cara (%)")
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
