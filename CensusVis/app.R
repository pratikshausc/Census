library(shiny)
library(ggplot2)
library(ggmap)
library(dplyr)


ui <- fluidPage(
  titlePanel("USA Census Visualization", windowTitle = "CensusVis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with oinformation from the 2010 Census"),
      selectInput(inputId = "var",
                  label = "Choose a variable to display",
                  choices = list("Percent White","Percent Black","Percent Hispanic","Percent Asian"),
                  selected = "Percent White")
    ),
   
    mainPanel(
      ##textOutput(outputId = "selected_var") ##rendering text output 
      plotOutput(outputId = "plot")
    )
    )
    
    )
  

server <- function(input,output){
  
output$plot = renderPlot({
  
  counties <- reactive({
    race = readRDS("Data/counties.rds")
    
    counties_map = map_data("county")
    View(counties_map)
    
    ##In order to join both tables, we need to make sure we are combining them by
    ##both state and county as unique identifier.
    ##so we can combine region and sub region in Counties_map in a new variable name since its called name as well
    ##in the other data set
    
    counties_map = counties_map %>%
      mutate(name = paste(region,subregion,sep = ","))
    
    left_join(counties_map,race,by = "name")
  })
  
  Race = switch(input$var,
                "Percent White" = counties()$white,
                "Percent Black" = counties()$black,
                "Percent Hispanic" = counties()$hispanic,
                "Percent Asian" = counties()$asian
  )
    ggplot(counties(),aes(x=long,y=lat,group=group,fill = Race))+
        geom_polygon(color = "black") +
        scale_fill_gradient(high = "red",low = "white")
  
})
}

shinyApp(ui,server)