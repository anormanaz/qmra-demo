library(shiny)
library(bslib)
install.packages("shinylive")   
library(shinylive)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e2e9eb;
      }
      .input-row {
        background-color: #0c234b;
        color: white;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
      }
      .input-row label, .input-row .control-label, .input-row .shiny-input-container {
        color: white !important;
      }
      .output-card {
        background-color: white;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
        border-radius: 8px;
      }
    "))
  ),
  
  titlePanel("Airborne Infection Risk Demo"),
  
  tabsetPanel(
    tabPanel("Demo 1: Co-Occupancy Risk",
             div(class = "input-row",
                 fluidRow(
                   column(4,
                          selectInput("room", "Room Type:",
                                      choices = c("Office", "Classroom", "Waiting Room"))
                   ),
                   column(4,
                          sliderInput("duration_hr", "Exposure Duration (hours):",
                                      min = 0.5, max = 4, value = 1, step = 0.5,
                                      ticks = TRUE)
                   ),
                   column(4,
                          actionButton("go1", "Run Model", class = "btn btn-light", style = "margin-top: 25px;")
                   )
                 )
             ),
             
             div(class = "output-card",
                 h4("Model Assumptions"),
                 verbatimTextOutput("assumptions_txt")
             ),
             
             div(class = "output-card",
                 h4("Risk Estimate"),
                 verbatimTextOutput("summary_txt")
             ),
             
             fluidRow(
               column(6,
                      div(class = "output-card",
                          h4("Time Series Graph"),
                          img(
                            src = "https://ifhpath.arizona.edu/sites/default/files/2025-10/graph-example.PNG",
                            width = "100%", alt = "Risk Over Time"
                          )
                      )
               ),
               column(6,
                      div(class = "output-card",
                          h4("Risk Summary Table"),
                          img(
                            src = "https://ifhpath.arizona.edu/sites/default/files/2025-10/table-example.PNG",
                            width = "100%", alt = "Risk Table"
                          )
                      )
               )
             )
    ),
    
    tabPanel("Demo 2: Coming Soon",
             div(class = "output-card",
                 h4("Demo 2 Placeholder"),
                 p("This tab will contain a second QMRA demo.")
             )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$go1, {
    output$assumptions_txt <- renderPrint({
      duration_min <- input$duration_hr * 60
      paste("Room:", input$room, "\nDuration:", duration_min, "minutes")
    })
    
    output$summary_txt <- renderPrint({
      "Risk estimate: 1.23E-03 (placeholder)"
    })
  })
}

shinyApp(ui, server)
