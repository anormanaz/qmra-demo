#to update run in console
#shinylive::export(appdir = "app", destdir = "docs")

#in terminal:
#git add docs
#git commit -m "Deploy updated Shiny app"
#git push

install.packages("shinylive")
library(shiny)
library(bslib)
library(plotly)

# =============================
# Helper functions
# =============================

# Converts numeric risk to a readable "1 in ..." string
risk_text <- function(risk) {
  if (risk < 1e-5) {
    "< 1 in 100,000"
  } else if (risk < 1e-4) {
    inv <- round(1 / risk, -3)
    paste0("≈ 1 in ", format(inv, big.mark = ","))
  } else {
    inv <- round(1 / risk, -2)
    paste0("> 1 in ", format(inv, big.mark = ","))
  }
}

# Create ring segments for colored bands
make_ring <- function(start, end, r_outer = 1, r_inner = 0.7, n = 100) {
  theta_outer <- seq(start, end, length.out = n)
  theta_inner <- rev(theta_outer)
  data.frame(
    x = c(r_outer * cos(theta_outer), r_inner * cos(theta_inner)),
    y = c(r_outer * sin(theta_outer), r_inner * sin(theta_inner))
  )
}

# --- Gauge helper (keeps median label under arcs) ----
render_custom_gauge <- function(value, title) {
  value <- pmin(pmax(value, 0), 1e-3)
  val_ratio <- value / 1e-3
  angle <- pi * (1 - val_ratio)
  
  # new thresholds: green <1e-4, yellow 1e-4–5e-4, red >5e-4
  green  <- make_ring(pi, pi*(1 - 1e-4/1e-3))
  yellow <- make_ring(pi*(1 - 1e-4/1e-3), pi*(1 - 5e-4/1e-3))
  red    <- make_ring(pi*(1 - 5e-4/1e-3), 0)
  
  label_text <- sprintf("Median risk: %.1e (%s)", value, risk_text(value))
  
  plot_ly() %>%
    add_polygons(data = green,  x = ~x, y = ~y, fillcolor = "#8fd19e",
                 line = list(color = "transparent"), showlegend = FALSE) %>%
    add_polygons(data = yellow, x = ~x, y = ~y, fillcolor = "#ffd480",
                 line = list(color = "transparent"), showlegend = FALSE) %>%
    add_polygons(data = red,    x = ~x, y = ~y, fillcolor = "#ff726f",
                 line = list(color = "transparent"), showlegend = FALSE) %>%
    add_segments(x = 0, y = 0, xend = cos(angle)*0.9, yend = sin(angle)*0.9,
                 line = list(color = "black", width = 3), showlegend = FALSE) %>%
    add_markers(x = 0, y = 0, marker = list(color = "black", size = 6),
                showlegend = FALSE) %>%
    # median label below
    add_annotations(x = 0, y = -0.22, text = label_text,
                    showarrow = FALSE, font = list(size = 14)) %>%
    layout(
      title = list(text = title, y = 0.9, font = list(size = 16)),
      xaxis = list(title = list(text = NULL), showticklabels = FALSE,
                   zeroline = FALSE, showgrid = FALSE,
                   range = c(-1.2, 1.2), automargin = TRUE),
      yaxis = list(title = list(text = NULL), showticklabels = FALSE,
                   zeroline = FALSE, showgrid = FALSE,
                   range = c(-0.4, 1.2), automargin = TRUE),
      margin = list(t = 40, b = 20, l = 0, r = 0)
    )
}

# =============================
# UI
# =============================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #e2e9eb; }
      .input-row {
        background-color: #0c234b; color: white;
        padding: 15px; margin-bottom: 20px; border-radius: 5px;
      }
      .input-row label, .input-row .control-label, .input-row .shiny-input-container {
        color: white !important;
      }
      .output-card {
        background-color: white; padding: 20px; margin-bottom: 20px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1); border-radius: 8px;
        word-wrap: break-word; white-space: normal;
      }
      table {
        width: 100%; border-collapse: collapse; text-align: left;
      }
      th, td {
        padding: 8px; border-bottom: 1px solid #ddd;
      }
      th { background-color: #f5f7f8; font-weight: bold; }
    "))
  ),
  
  tabsetPanel(
    tabPanel("Airborne Risk Model",
             div(class = "input-row",
                 fluidRow(
                   column(4,
                          selectInput("room", "Room Dimensions:",
                                      choices = c(
                                        "10×10×8 ft (22.7 m³)" = "V_10x10x8",
                                        "24×40×8 ft (217.5 m³)" = "V_24x40x8",
                                        "60×30×25 ft (1274.3 m³)" = "V_60x30x25"
                                      )
                          )
                   ),
                   column(4,
                          selectInput("ach", "Air Exchange Rate (ACH):",
                                      choices = c(3, 6, 12), selected = 3)
                   ),
                   column(4,
                          sliderInput("duration_hr", "Exposure Duration (hours):",
                                      min = 0.5, max = 4, value = 1, step = 0.5)
                   )
                 ),
                 actionButton("go1", "Run Model",
                              class = "btn btn-light", style = "margin-top: 10px;")
             ),
             
             # Risk summary
             div(class = "output-card",
                 h4("Risk Summary"),
                 htmlOutput("summary_txt")
             ),
             
             # Gauges row
             fluidRow(
               column(
                 6,
                 div(class = "output-card",
                     plotlyOutput("gauge_H1N1", height = "280px"))
               ),
               column(
                 6,
                 div(class = "output-card",
                     plotlyOutput("gauge_COVID", height = "280px"))
               )
             ),
             
             # Table
             div(class = "output-card",
                 h4("Estimated Infection Risk by Pathogen"),
                 tableOutput("risk_table")
             ),
             
             # Assumptions
             div(class = "output-card",
                 h4("Model Assumptions"),
                 htmlOutput("assumptions_txt")
             )
    ),
    
    tabPanel("Surface Risk Model (Coming Soon)",
             div(class = "output-card",
                 h4("Demo 2 Placeholder"),
                 p("This tab will contain a surface contamination QMRA demo."))
    )
  )
)

# =============================
# Server
# =============================
# --- Server ----
server <- function(input, output, session) {
  
  co_occupy_baseline <- function(q, V, ACH, t, IR, i = 1) {
    1 - exp(-(i * q * IR * t) / (V * ACH))
  }
  
  # --- reactive model results ---
  results <- eventReactive(input$go1, {
    
    iterations <- 10000
    set.seed(103106)
    IR <- pmax(rnorm(iterations, mean = 0.012, sd = 0.0025), 0) * 60
    i <- 1
    
    # Room volumes (m³)
    V <- switch(input$room,
                "V_10x10x8" = 22.65,
                "V_24x40x8" = 217.47,
                "V_60x30x25" = 1274.25
    )
    ACH <- as.numeric(input$ach)
    t <- input$duration_hr
    
    erq_function <- function(med, log10_sd) {
      meanlog <- log(10) * log10(med)
      sdlog   <- log(10) * log10_sd
      rlnorm(n = iterations, meanlog = meanlog, sdlog = sdlog)
    }
    
    q_H1N1  <- erq_function(3, 0.84)
    q_covid <- erq_function(46, 1.2)
    
    H1N1_vals  <- co_occupy_baseline(q_H1N1,  V, ACH, t, IR)
    COVID_vals <- co_occupy_baseline(q_covid, V, ACH, t, IR)
    
    list(
      H1N1 = H1N1_vals,
      COVID = COVID_vals,
      med_inf = median(H1N1_vals),
      med_cov = median(COVID_vals),
      V = V, ACH = ACH, t = t
    )
  })
  
  # --- summary text ---
  output$summary_txt <- renderText({
    req(results())
    dim_ft <- switch(input$room,
                     "V_10x10x8" = c(10, 10, 8),
                     "V_24x40x8" = c(24, 40, 8),
                     "V_60x30x25" = c(60, 30, 25)
    )
    sprintf(
      "After spending <strong>%.1f hours</strong> in a <strong>%.0f ft × %.0f ft × %.0f ft room</strong> (≈ %.1f m³) shared with a contagious person under <strong>%s ACH</strong>, the <strong>median estimated infection risk</strong> is <strong>%.2f%%</strong> for Influenza (H1N1) and <strong>%.2f%%</strong> for COVID-19 (SARS-CoV-2).",
      results()$t, dim_ft[1], dim_ft[2], dim_ft[3],
      results()$V, results()$ACH,
      results()$med_inf * 100, results()$med_cov * 100
    )
  })
  
  # --- table ---
  output$risk_table <- renderTable({
    req(results())
    data.frame(
      Pathogen = c("Influenza (H1N1)", "COVID-19 (SARS-CoV-2)"),
      Min = formatC(c(min(results()$H1N1), min(results()$COVID)), format="e", digits=2),
      Median = formatC(c(results()$med_inf, results()$med_cov), format="e", digits=2),
      Max = formatC(c(max(results()$H1N1), max(results()$COVID)), format="e", digits=2),
      Mean = formatC(c(mean(results()$H1N1), mean(results()$COVID)), format="e", digits=2)
    )
  }, striped = TRUE, bordered = TRUE)
  
  # --- gauges ---
  observeEvent(results(), {
    output$gauge_H1N1 <- renderPlotly({
      render_custom_gauge(results()$med_inf, "Influenza (H1N1)")
    })
    output$gauge_COVID <- renderPlotly({
      render_custom_gauge(results()$med_cov, "COVID-19 (SARS-CoV-2)")
    })
  })
  
  # --- assumptions ---
  output$assumptions_txt <- renderUI({
    HTML("
      Infection risk calculated using the Wells-Riley equation under the following assumptions:
      <ul>
        <li>One contagious individual is present during the exposure period.</li>
        <li>The room air is well-mixed (uniform quanta concentration).</li>
        <li>Quanta generation remains constant throughout the exposure period.</li>
        <li>Air removal is modeled solely by ventilation (based on ACH).</li>
      </ul>
      Room dimensions are converted from feet to cubic meters automatically.
      <br><br>
      The risk threshold used in this demo is based on a benchmark of 
      <strong>1 infection per 10,000 exposures</strong>. 
      For higher-risk or immunocompromised individuals, a more protective target of 
      <strong>1 infection per 100,000 exposures</strong> may be appropriate.
    ")
  })
  # --- placeholder plots (so gauges show up immediately) ---
  empty_gauge <- plot_ly() %>%
    layout(
      xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, range = c(-1, 1)),
      yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, range = c(-0.5, 1)),
      annotations = list(text = "Click 'Run Model' to generate risk", x = 0, y = 0, showarrow = FALSE)
    )
  
  output$gauge_H1N1 <- renderPlotly(empty_gauge)
  output$gauge_COVID <- renderPlotly(empty_gauge)
}

shinyApp(ui, server)
