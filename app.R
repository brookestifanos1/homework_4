#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(shiny)


# --- 1. USER INTERFACE (UI) ---
ui <- fluidPage(
  
  titlePanel("Brook Estifanos - Homework 4"),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      
      # UI_1, Slider for number of observations "n"
      sliderInput(
        inputId = "n",
        label = "Choose Number fo Observations (n):",
        min = 10,
        max = 5000,
        value = 100,
        step = 50
      ),
      
      br(),
      
      # UI_2, option to display hisotgram or boxplot
      radioButtons(
        inputId = "plot_option",
        label = "Select Plot Type:",
        choices = c("Histogram" = "hist", "Box Plot" = "box"),
        selected = "hist"
      ),
      
      br(),
      # UI_3, option to change plot color
      
      selectInput(
        inputId = "color_option",
        label = "Choose Plot Color:",
        choices = c(
          "Gray" = "lightgray",
          "Orange" = "orange",
          "Sky Blue" = "skyblue",
          "Orchid" = "orchid",
          "Dark Green" = "darkgreen"
        ),
        selected = "orange"
      )
    ),
    
    # Panel for displaying plot
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

# --- 2. SERVER LOGIC ---

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    # assigning the runif function to a tibble 
    random <- tibble(x = runif(input$n))
    
    # enabling the user choice for histogram or boxplot
    if (input$plot_option == "hist") 
      
    {
      # if user chose histogram render the histogram for the tibble
      random |> 
        ggplot(aes(x = x)) +
        geom_histogram(bins = 10, fill = input$color_option, color = "black") +
        theme_minimal()+
        labs(title = "Histogram of runif(input$n) ",
             subtitle = "Generated using runif() with n = 200",
             x = "runif(input$n)",
             y = "Frequency")
    } 
    
    # if user chose boxplot render the box plot for the tibble
    
    else
    {
      
      random |> ggplot(aes(x = "", y = x)) +
        geom_boxplot(fill = input$color_option, color = "black", linewidth = 1) +
        
        #jitter added to show actual individual points on plot
        #helps user understand better what is happening
        
        geom_jitter(color = "steelblue", width = 0.1, alpha = 0.5, size = 1.5) +
        
        labs(
          title = "Histogram of runif(input$n) with individual points",
          
          #dynamic x axis label based on slidebar input 
          
          x = paste("Number of Observations:", input$n),
          y = "runif(input$n)"
        ) +
        theme_minimal() 
    }
    
  })
}

# --- 3. RUN APPLICATION ---
shinyApp(ui = ui, server = server)


