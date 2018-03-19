library(shinydashboard)
library(shiny)

source("part2.R")
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "chemicals in brocoli and cauliflower",
                  titleWidth = 350,
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Support",
                                 message = "Welcome to our part 2 shiny app!",
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
    
    sidebarMenu(
      menuItem("DashBoard", tabName = "dashboard"),
      menuItem("Data Download", tabName = "rawdata")
    ),
    
    hr(),
    helpText("ref: https://quickstats.nass.usda.gov/")
  ),
  
  dashboardBody(
    tabItems(
    # Boxes need to be put in a row (or column)
      tabItem("dashboard",
              fluidRow(
                box(title = "Cauliflower chart",status = "success", solidHeader = TRUE,
                    collapsible = FALSE,
                    width = 10,
                    plotOutput("plotcaul"))
                ),
              fluidRow(
                box(title = "Brocoli chart",status = "success", solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 10,
                    plotOutput("plotbroc")
      )
    )),

      tabItem("rawdata",
              verbatimTextOutput("rawvegdata"),
              downloadButton("downloadCsv", "Download as CSV")
      ))
    
  )
)



server <- function(input, output) {
  # Generate a summary of the dataset 
  output$rawvegdata <- renderPrint({
    dataset <- vegdata3
    dataset
  })
  output$downloadCsv <- downloadHandler(
    filename = "vegdata.csv",
    content = function(file) {
      write.csv(vegdata3, file)
    },
    contentType = "text/csv"
  )
  
  output$plotcaul <- renderPlot({
    caulplot
  })
  output$plotbroc <- renderPlot({
    brocplot
  })
}

shinyApp(ui, server)