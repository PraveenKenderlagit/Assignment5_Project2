
library(shinydashboard)
library(shiny)

source("part1.R")
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "NOAA Weather Station buoy 46035",
                  titleWidth = 350,
                  dropdownMenu(type = "messages",
               messageItem(
                 from = "Support",
                 message = "Welcome to our shiny app!",
                 icon = icon("life-ring")
               ),
               messageItem(
                 from = "Support",
                 message = "The app is finished!",
                 icon = icon("life-ring"),
                 time = "2018-03-18"
               )
  )),
  
  dashboardSidebar(
    
    radioButtons("type", "plot you want to see:",
                 c("Air Temperature" = "Air Temperature",
                   "Water Temperature" = "Water Temperature")),
    
    sliderInput("years", "Year Range:", 1988, 2017, c(1995,2010)),
    sidebarMenu(
      menuItem("Raw data", tabName = "rawdata")
    ),

    hr(),
    helpText("ref: http://www.ndbc.noaa.gov/")
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = "Plot",status = "success", solidHeader = TRUE,
          collapsible = FALSE,
          width = 10,
          plotOutput("plot1"))
    ),
    fluidRow(
      box(title = "Summary",status = "success", solidHeader = TRUE,
          collapsible = TRUE,
          width = 10,
          verbatimTextOutput("summary")
          )
      ),
    tabItems(
    tabItem("rawdata",
            verbatimTextOutput("rawtable"),
            downloadButton("downloadCsv", "Download as CSV")
    ))

  )
)



server <- function(input, output) {
  # Generate a summary of the dataset 
  output$summary <- renderPrint({
    dataset <- annualTemprature
    summary(dataset)
  })
  output$rawtable <- renderPrint({
    dataset <- annualTemprature
    dataset
  })
  
  output$plot1 <- renderPlot({
    dataset <- annualTemprature
    rownames(dataset) <- dataset[,1]
    dataset <- dataset[as.character(c(input$years[1]:input$years[2])),input$type]
    barplot(dataset,
            main=paste("Annual Mean",input$type,sep=" "),
            ylab="Degrees Celsius",
            xlab="Year",
            names.arg = c(input$years[1]:input$years[2]))
    })
}

shinyApp(ui, server)