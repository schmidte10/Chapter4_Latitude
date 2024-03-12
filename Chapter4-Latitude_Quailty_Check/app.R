#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#--- import libraries ---#
library(tidyverse)
library(shiny)
library(shinythemes)
#--- import data ---# 
load("C:/Users/jc527762/OneDrive - James Cook University/PhD dissertation/Data/Chapter4_Latitude/import_files/lat_resp_dat.Rda") 

lat_resp_dat <-  lat_resp_dat |> 
  mutate(dev.temp = as.factor(dev.temp), 
         replicate = str_sub(sampleID, -1,-1), 
         population = factor(population))  
  
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    navbarPage("Respirometry Quailty Checks", theme = shinytheme("yeti"),
                tabPanel("Individuals", fluid = TRUE, icon = icon("fish"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("sampleID", 
                      "Individual", 
                      choices=c(unique(lat_resp_dat$sampleID),"NULL"), 
                      selected = NULL),
          div(style ="display:inline-block;", 
                checkboxGroupInput(
                  inputId = "dev.temp",
                  label = "Temperature C",
                  choices = c("28"="28",
                              "30"="30",
                              "31"="31", 
                              "OFF" = "NULL"))), 
          selectizeInput("population", 
                      "Population", 
                      choices = (unique(lat_resp_dat$population)),
                      options = list(plugins= list('remove_button')), 
                      multiple = TRUE)

            ),
            
            
            

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"))
        )
    ) 
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    FinderInd <- reactive({  
      req(input$sampleID)
      filter(lat_resp_dat, sampleID %in% input$sampleID)
      })
    
    FinderTemp <- reactive({ 
      req(input$dev.temp)
      req(input$population)
      filter(lat_resp_dat, 
             dev.temp %in% input$dev.temp, 
             population %in% input$population) 
      })
    

  
    output$plot <- renderPlot({
        input$sampleID 
        input$dev.temp
        input$population
      isolate({ 
        ggplot(data =lat_resp_dat, aes(x=(time_lag_min/60), y=rate.output2)) + 
          geom_point(color="grey72", alpha=0.4) +  
          geom_point(data = FinderInd(), mapping = aes(x=(time_lag_min/60), y=rate.output2, color=time_lag_min/60),
                    size=2.1) +
          geom_point(data = FinderTemp(), mapping = aes(x=(time_lag_min/60), y=rate.output2, color=dev.temp), 
                     size=2.1) +
          theme_bw() + 
          theme(legend.position = "right")
        
      
        })
    }, height = 420, width = 700) 
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
