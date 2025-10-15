#to update run in console
#shinylive::export(appdir = "app", destdir = "docs")

#in terminal:
#git add docs
#git commit -m "Deploy updated Shiny app"
#git push

library(shiny)
library(bslib)
install.packages("shinylive")   
library(shinylive)
library(truncdist)
library(tibble)
library(scales)
library(glue)

install.packages("remotes")

remotes::install_version("gt", version = "0.11.1")

ui <- fluidPage(
  titlePanel("Airborne Infection Risk Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("room", "Select Room Size:",
                  choices = c(
                    "Small Room (10×10×8 ft, ~22.6 m³)" = 22.65,
                    "Medium Room (24×40×8 ft, ~217.5 m³)" = 217.47,
                    "Large Room (60×30×25 ft, ~1274.3 m³)" = 1274.25
                  )),
      selectInput("ach", "Select Air Exchange Rate (ACH):",
                  choices = c("3 ACH" = 3, "6 ACH" = 6, "12 ACH" = 12)),
      sliderInput("duration", "Time in Room (hours):", min = 0.25, max = 4, value = 1, step = 0.25)
    ),
    mainPanel(
      h4("Model Assumptions"),
      p("This calculator estimates airborne infection risk using the Wells-Riley equation. Assumptions include:"),
      tags$ul(
        tags$li("One infectious individual present"),
        tags$li("Uniform mixing of air in the room"),
        tags$li("Constant pathogen emission rate over time"),
        tags$li("Ventilation modeled via selected ACH (Air Changes per Hour)")
      ),
      h4("Risk Estimates"),
      textOutput("summary_txt"),
      br(),
      gt_output("risk_table")
    )
  )
)

server <- function(input, output, session) {
  iterations <- 10000
  set.seed(103106)
  
  # Pulmonary ventilation (m³/h)
  IR <- rtrunc(iterations, "norm", a = 0, b = Inf, mean = 0.012, sd = 0.0025) * 60
  
  i <- 1  # infectious person
  
  # Quanta emission function
  erq_function <- function(med, log10_sd) {
    meanlog <- log(10) * log10(med)
    sdlog <- log(10) * log10_sd
    rlnorm(n = iterations, meanlog = meanlog, sdlog = sdlog)
  }
  
  q_H1N1 <- erq_function(med = 3, log10_sd = 0.84)
  q_covid <- erq_function(med = 46, log10_sd = 1.2)
  
  # Wells-Riley function
  co_occupy_baseline <- function(q, V, ACH, t){
    1 - exp(-(i * q * IR * t) / (V * ACH))
  }
  
  # Summary output (text)
  output$summary_txt <- renderText({
    V <- as.numeric(input$room)
    ACH <- as.numeric(input$ach)
    t <- input$duration
    
    H1N1_risk <- co_occupy_baseline(q_H1N1, V, ACH, t)
    covid_risk <- co_occupy_baseline(q_covid, V, ACH, t)
    
    glue("After spending {t} hours in the selected room with one infectious person:",
         "\n• Influenza (H1N1): ", percent(mean(H1N1_risk), accuracy = 0.01),
         "\n• Covid-19 (SARS-CoV-2): ", percent(mean(covid_risk), accuracy = 0.01))
  })
  
  # GT Table
  output$risk_table <- render_gt({
    V <- as.numeric(input$room)
    ACH <- as.numeric(input$ach)
    t <- input$duration
    
    H1N1_risk <- co_occupy_baseline(q_H1N1, V, ACH, t)
    covid_risk <- co_occupy_baseline(q_covid, V, ACH, t)
    
    table_data <- tibble(
      Pathogen = c("Influenza (H1N1)", "Covid-19 (SARS-CoV-2)"),
      Min = c(min(H1N1_risk), min(covid_risk)),
      Median = c(median(H1N1_risk), median(covid_risk)),
      Max = c(max(H1N1_risk), max(covid_risk)),
      Mean = c(mean(H1N1_risk), mean(covid_risk))
    )
    
    table_data |>
      gt() |>
      tab_header(title = "Estimated Infection Risk by Pathogen") |>
      fmt_scientific(
        columns = c(Min, Median, Max, Mean),
        decimals = 2,
        exp_style = "E",
        force_sign_n = TRUE
      ) |>
      sub_missing(columns = everything(), missing_text = "") |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      )
  })
}

shinyApp(ui, server)

