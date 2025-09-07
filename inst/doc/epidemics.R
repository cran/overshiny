## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# devtools::install_github("epiverse-trace/epidemics")
# devtools::install_github("nicholasdavies/overshiny")

## ----eval=FALSE---------------------------------------------------------------
# library(epidemics)
# library(ggplot2)
# 
# # Model run function
# run_model <- function()
# {
#     # Build parameters
#     I0 <- 0.001
#     pop_size <- 1000000
# 
#     # Build population
#     pop <- population(
#         name = "Utopia",
#         contact_matrix = matrix(1),
#         demography_vector = 1000000,
#         initial_conditions = matrix(c(1 - I0, 0, I0, 0, 0), nrow = 1)
#     )
# 
#     # Run model
#     results <- model_default(pop)
# 
#     return (results)
# }
# 

## ----eval=FALSE---------------------------------------------------------------
# results <- run_model()
# results <- results[results$compartment == "infectious", ]
# ggplot(results) +
#     geom_line(aes(x = time, y = value)) +
#     labs(x = NULL, y = "Infection prevalence")

## ----eval=FALSE---------------------------------------------------------------
# input <- list(
#     # Start and end dates of the epidemic
#     date_range = as.Date(c("2025-01-01", "2025-12-31")),
#     # Population size in millions
#     pop_size = 59
#     # Percentage (not proportion) of the population initially infected
#     init_infec = 0.1,
#     # Duration of latent period in days
#     latent_pd = 2,
#     # Duration of infectious period in days
#     infec_pd = 7,
#     # Basic reproduction number
#     R0 = 1.3,
# )

## ----eval=FALSE---------------------------------------------------------------
# # Model run function
# run_model <- function(input)
# {
#     # Transform parameters
#     I0 <- input$init_infec / 100; # Percent to proportion
#     duration <- as.numeric(input$date_range[2] - input$date_range[1]) # Dates to duration
#     infec_rate <- 1 / input$latent_pd # Latent period to infectiousness rate
#     recov_rate <- 1 / input$infec_pd  # Infectious period to recovery rate
#     trans_rate <- input$R0 * recov_rate # R0 to transmission rate (R_0 = beta / gamma)
# 
#     # Build population
#     pop <- population(
#         name = "Utopia",
#         contact_matrix = matrix(1),
#         demography_vector = input$pop_size * 1000000, # Population size in millions
#         initial_conditions = matrix(c(1 - I0, 0, I0, 0, 0), nrow = 1)
#     )
# 
#     # Run model
#     results <- model_default(pop, transmission_rate = trans_rate,
#         infectiousness_rate = infec_rate, recovery_rate = recov_rate,
#         time_end = duration)
# 
#     return (results)
# }

## ----eval = FALSE-------------------------------------------------------------
# library(epidemics)
# library(shiny)
# 
# # --- User interface ---
# ui <- fluidPage(
#     titlePanel("SEIRV model with interventions"),
# 
#     # 3 columns of inputs
#     fluidRow(
#         column(4,
#             # Basic epidemic settings
#             h3("Epidemic")
#         ),
# 
#         column(4,
#             # Pathogen settings
#             h3("Pathogen")
#         ),
# 
#         column(4,
#             # Intervention settings
#             h3("Interventions")
#         )
#     )
# )
# 
# # --- App logic ---
# server <- function(input, output)
# {
# }
# 
# # --- Run app ---
# shinyApp(ui, server)

## ----eval = FALSE-------------------------------------------------------------
# # --- User interface ---
# ui <- fluidPage(
#     ...
# )

## ----eval = FALSE-------------------------------------------------------------
# fluidRow(
#     column(4, "part one"),
#     column(4, "part two"),
#     column(4, "part three")
# )

## ----eval = FALSE-------------------------------------------------------------
# # --- User interface ---
# ui <- fluidPage(
#     titlePanel("SEIRV model with interventions"),
# 
#     # 3 columns of inputs
#     fluidRow(
#         column(4,
#             # Basic epidemic settings
#             h3("Epidemic")
#         ),
# 
#         column(4,
#             # Pathogen settings
#             h3("Pathogen")
#         ),
# 
#         column(4,
#             # Intervention settings
#             h3("Interventions")
#         )
#     )
# )

## ----eval = FALSE-------------------------------------------------------------
# library(epidemics)
# library(shiny)
# 
# # --- User interface ---
# ui <- fluidPage(
#     titlePanel("SEIRV model with interventions"),
# 
#     # 3 columns of inputs
#     fluidRow(
#         column(4,
#             # Basic epidemic settings
#             h3("Epidemic"),
#             dateRangeInput("date_range", "Date range", start = "2025-01-01", end = "2025-12-31"),
#             numericInput("pop_size", "Population size (millions)", value = 59, min = 1),
#             sliderInput("init_infec", "Initial proportion infectious (%)", value = 0.1, min = 0, max = 1, step = 0.01)
#         ),
# 
#         column(4,
#             # Pathogen settings
#             h3("Pathogen"),
#             sliderInput("R0", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"),
#                 value = 1.3, min = 0, max = 5, step = 0.05),
#             sliderInput("latent_pd", "Latent period (days)", value = 2, min = 1, max = 7, step = 0.1),
#             sliderInput("infec_pd", "Infectious period (days)", value = 7, min = 1, max = 10, step = 0.1)
#         ),
# 
#         column(4,
#             # Overlay controls: tokens that can be dragged onto the plot
#             h3("Interventions")
#         )
#     )
# )
# 
# # --- App logic ---
# server <- function(input, output)
# {
# }
# 
# # --- Run app ---
# shinyApp(ui, server)

## ----eval = FALSE-------------------------------------------------------------
# library(shiny)
# library(ggplot2)
# library(epidemics)
# 
# # Model run function
# run_model <- function(input)
# {
#     # Transform parameters
#     I0 <- input$init_infec / 100; # Percent to proportion
#     duration <- as.numeric(input$date_range[2] - input$date_range[1]) # Dates to duration
#     infec_rate <- 1 / input$latent_pd # Latent period to infectiousness rate
#     recov_rate <- 1 / input$infec_pd  # Infectious period to recovery rate
#     trans_rate <- input$R0 * recov_rate # R0 to transmission rate (R_0 = beta / gamma)
# 
#     # Build population
#     pop <- population(
#         name = "Utopia",
#         contact_matrix = matrix(1),
#         demography_vector = input$pop_size * 1000000, # Population size in millions
#         initial_conditions = matrix(c(1 - I0, 0, I0, 0, 0), nrow = 1)
#     )
# 
#     # Run model
#     results <- model_default(pop, transmission_rate = trans_rate,
#         infectiousness_rate = infec_rate, recovery_rate = recov_rate,
#         time_end = duration)
# 
#     return (results)
# }

## ----eval = FALSE-------------------------------------------------------------
# # --- User interface ---
# ui <- fluidPage(
#     titlePanel("SEIRV model with interventions"),
# 
#     # NEW PART IS HERE:
#     # Main plot
#     plotOutput("display", width = "100%", height = 400),
#     # END OF NEW PART
# 
#     # 3 columns of inputs
#     fluidRow(
#         column(4,
#             # Basic epidemic settings
#             h3("Epidemic"),
#             dateRangeInput("date_range", "Date range", start = "2025-01-01", end = "2025-12-31"),
#             numericInput("pop_size", "Population size (millions)", value = 59, min = 1),
#             sliderInput("init_infec", "Initial proportion infectious (%)", value = 0.1, min = 0, max = 1, step = 0.01)
#         ),
# 
#         column(4,
#             # Pathogen settings
#             h3("Pathogen"),
#             sliderInput("R0", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"),
#                 value = 1.3, min = 0, max = 5, step = 0.05),
#             sliderInput("latent_pd", "Latent period (days)", value = 2, min = 1, max = 7, step = 0.1),
#             sliderInput("infec_pd", "Infectious period (days)", value = 7, min = 1, max = 10, step = 0.1)
#         ),
# 
#         column(4,
#             # Overlay controls: tokens that can be dragged onto the plot
#             h3("Interventions")
#         )
#     )
# )

## ----eval=FALSE---------------------------------------------------------------
# # --- App logic ---
# server <- function(input, output)
# {
#     output$display <- renderPlot({
#         results <- run_model(input)
#         results <- results[results$compartment == "infectious", ]
#         ggplot(results) +
#             geom_line(aes(x = time, y = value)) +
#             labs(x = NULL, y = "Infection prevalence") +
#             ylim(0, NA)
#     })
# }
# 
# # --- Run app ---
# shinyApp(ui, server)

## ----eval=FALSE---------------------------------------------------------------
#     plotOutput("marmite", width = "100%", height = 400),

## ----eval=FALSE---------------------------------------------------------------
# ggplot(results) +
#     geom_line(aes(x = time + input$date_range[1], y = value / 1000)) +
#     labs(x = NULL, y = "Infection prevalence (thousands)") +
#     ylim(0, NA)

## ----eval=FALSE---------------------------------------------------------------
# library(shiny)
# library(ggplot2)
# library(epidemics)
# 
# # Model run function
# run_model <- function(input)
# {
#     # Transform parameters
#     I0 <- input$init_infec / 100; # Percent to proportion
#     duration <- as.numeric(input$date_range[2] - input$date_range[1]) # Dates to duration
#     infec_rate <- 1 / input$latent_pd # Latent period to infectiousness rate
#     recov_rate <- 1 / input$infec_pd  # Infectious period to recovery rate
#     trans_rate <- input$R0 * recov_rate # R0 to transmission rate (R_0 = beta / gamma)
# 
#     # Build population
#     pop <- population(
#         name = "Utopia",
#         contact_matrix = matrix(1),
#         demography_vector = input$pop_size * 1000000, # Population size in millions
#         initial_conditions = matrix(c(1 - I0, 0, I0, 0, 0), nrow = 1)
#     )
# 
#     # Run model
#     results <- model_default(pop, transmission_rate = trans_rate,
#         infectiousness_rate = infec_rate, recovery_rate = recov_rate,
#         time_end = duration)
# 
#     return (results)
# }
# 
# # --- User interface ---
# ui <- fluidPage(
#     titlePanel("SEIRV model with interventions"),
# 
#     # NEW PART IS HERE:
#     # Main plot
#     plotOutput("display", width = "100%", height = 400),
#     # END OF NEW PART
# 
#     # 3 columns of inputs
#     fluidRow(
#         column(4,
#             # Basic epidemic settings
#             h3("Epidemic"),
#             dateRangeInput("date_range", "Date range", start = "2025-01-01", end = "2025-12-31"),
#             numericInput("pop_size", "Population size (millions)", value = 59, min = 1),
#             sliderInput("init_infec", "Initial proportion infectious (%)", value = 0.1, min = 0, max = 1, step = 0.01)
#         ),
# 
#         column(4,
#             # Pathogen settings
#             h3("Pathogen"),
#             sliderInput("R0", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"),
#                 value = 1.3, min = 0, max = 5, step = 0.05),
#             sliderInput("latent_pd", "Latent period (days)", value = 2, min = 1, max = 7, step = 0.1),
#             sliderInput("infec_pd", "Infectious period (days)", value = 7, min = 1, max = 10, step = 0.1)
#         ),
# 
#         column(4,
#             # Overlay controls: tokens that can be dragged onto the plot
#             h3("Interventions")
#         )
#     )
# )
# 
# # --- App logic ---
# server <- function(input, output)
# {
#     output$display <- renderPlot({
#         results <- run_model(input)
#         results <- results[results$compartment == "infectious", ]
#         ggplot(results) +
#             geom_line(aes(x = time + input$date_range[1], y = value / 1000)) +
#             labs(x = NULL, y = "Infection prevalence (thousands)") +
#             ylim(0, NA)
#     })
# }
# 
# # --- Run app ---
# shinyApp(ui, server)

## ----eval = FALSE-------------------------------------------------------------
# library(overshiny)

## ----eval = FALSE-------------------------------------------------------------
#     # Main plot
#     overlayPlotOutput("display", width = "100%", height = 400),

## ----eval = FALSE-------------------------------------------------------------
#         column(4,
#             # Overlay controls: tokens that can be dragged onto the plot
#             h3("Interventions"),
#             overlayToken("vx", "Vaccination"),
#             overlayToken("tx", "Transmission")
#         )

## ----eval = FALSE-------------------------------------------------------------
# # --- App logic ---
# server <- function(input, output)
# {
#     # --- OVERLAY SETUP ---
# 
#     # Initialise 8 draggable/resizable overlays
#     ov <- overlayServer("display", 8)
# 
#     # rest of code follows as normal...

## ----eval = FALSE-------------------------------------------------------------
#     output$display <- renderPlot({
#         results <- run_model(input)
#         results <- results[results$compartment == "infectious", ]
#         ggplot(results) +
#             geom_line(aes(x = time + input$date_range[1], y = value / 1000)) +
#             labs(x = NULL, y = "Infection prevalence (thousands)") +
#             ylim(0, NA)
#     })
# 

## ----eval = FALSE-------------------------------------------------------------
#     output$display <- renderPlot({
#         results <- run_model(input)
#         results <- results[results$compartment == "infectious", ]
#         plot <- ggplot(results) +
#             geom_line(aes(x = time + input$date_range[1], y = value / 1000)) +
#             labs(x = NULL, y = "Infection prevalence (thousands)") +
#             ylim(0, NA)
# 
#         overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
#     })

## ----eval = FALSE-------------------------------------------------------------
# library(shiny)
# library(ggplot2)
# library(epidemics)
# library(overshiny)
# 
# # Model run function
# run_model <- function(input)
# {
#     # Transform parameters
#     I0 <- input$init_infec / 100; # Percent to proportion
#     duration <- as.numeric(input$date_range[2] - input$date_range[1]) # Dates to duration
#     infec_rate <- 1 / input$latent_pd # Latent period to infectiousness rate
#     recov_rate <- 1 / input$infec_pd  # Infectious period to recovery rate
#     trans_rate <- input$R0 * recov_rate # R0 to transmission rate (R_0 = beta / gamma)
# 
#     # Build population
#     pop <- population(
#         name = "Utopia",
#         contact_matrix = matrix(1),
#         demography_vector = input$pop_size * 1000000, # Population size in millions
#         initial_conditions = matrix(c(1 - I0, 0, I0, 0, 0), nrow = 1)
#     )
# 
#     # Run model
#     results <- model_default(pop, transmission_rate = trans_rate,
#         infectiousness_rate = infec_rate, recovery_rate = recov_rate,
#         time_end = duration)
# 
#     return (results)
# }
# 
# # --- User interface ---
# ui <- fluidPage(
#     titlePanel("SEIRV model with interventions"),
# 
#     # NEW PART IS HERE:
#     # Main plot
#     overlayPlotOutput("display", width = "100%", height = 400),
#     # END OF NEW PART
# 
#     # 3 columns of inputs
#     fluidRow(
#         column(4,
#             # Basic epidemic settings
#             h3("Epidemic"),
#             dateRangeInput("date_range", "Date range", start = "2025-01-01", end = "2025-12-31"),
#             numericInput("pop_size", "Population size (millions)", value = 59, min = 1),
#             sliderInput("init_infec", "Initial proportion infectious (%)", value = 0.1, min = 0, max = 1, step = 0.01)
#         ),
# 
#         column(4,
#             # Pathogen settings
#             h3("Pathogen"),
#             sliderInput("R0", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"),
#                 value = 1.3, min = 0, max = 5, step = 0.05),
#             sliderInput("latent_pd", "Latent period (days)", value = 2, min = 1, max = 7, step = 0.1),
#             sliderInput("infec_pd", "Infectious period (days)", value = 7, min = 1, max = 10, step = 0.1)
#         ),
# 
#         column(4,
#             # Overlay controls: tokens that can be dragged onto the plot
#             h3("Interventions"),
#             overlayToken("vx", "Vaccination"),
#             overlayToken("tx", "Transmission")
#         )
#     )
# )
# 
# # --- App logic ---
# server <- function(input, output)
# {
#     # --- OVERLAY SETUP ---
# 
#     # Initialise 8 draggable/resizable overlays
#     ov <- overlayServer("display", 8)
# 
#     # --- RENDERING OF EPI CURVES ---
# 
#     output$display <- renderPlot({
#         results <- run_model(input)
#         results <- results[results$compartment == "infectious", ]
#         plot <- ggplot(results) +
#             geom_line(aes(x = time + input$date_range[1], y = value / 1000)) +
#             labs(x = NULL, y = "Infection prevalence (thousands)") +
#             ylim(0, NA)
# 
#         overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
#     })
# }
# 
# # --- Run app ---
# shinyApp(ui, server)

## ----eval = FALSE-------------------------------------------------------------
# # Model run function
# run_model <- function(input, ...)  # <-- FIRST CHANGE HERE

## ----eval = FALSE-------------------------------------------------------------
#     # Run model
#     results <- model_default(pop, transmission_rate = trans_rate,
#         infectiousness_rate = infec_rate, recovery_rate = recov_rate,
#         time_end = duration, ...) # <-- SECOND CHANGE HERE
# 
#     return (results)

## ----eval = FALSE-------------------------------------------------------------
#     output$display <- renderPlot({
#         # Create interventions
#         tx_int <- list()
#         vax <- NULL
# 
#         # Apply overlays as interventions
#         for (i in which(ov$active)) {
#             begin <- ov$cx0[i] - as.numeric(input$date_range[1])
#             end <- ov$cx1[i] - as.numeric(input$date_range[1])
#             if (ov$label[i] == "Vaccination") {
#                 nu <- 0.01 # proportion of population vaccinated per day
#                 if (is.null(vax)) {
#                     vax <- vaccination(name = as.character(i), nu = matrix(nu),
#                         time_begin = matrix(begin), time_end = matrix(end))
#                 } else {
#                     ov$active[i] <- FALSE
#                 }
#             } else if (ov$label[i] == "Transmission") {
#                 reduc <- 0.5 # reduction in transmission
#                 tx_int[[length(tx_int) + 1]] <- intervention(name = as.character(i),
#                     type = "rate", time_begin = matrix(begin), time_end = matrix(end),
#                     reduction = reduc)
#             }
#         }
# 
#         # Put interventions in the right format
#         int <- list()
#         if (length(tx_int)) {
#             int[["transmission_rate"]] <- do.call(c, tx_int)
#         }
# 
#         # Run model
#         results <- run_model(input,
#             vaccination = vax,
#             intervention = if (length(int)) int else NULL)
# 
#         # Process results (this is the same as before)
#         results <- results[results$compartment == "infectious", ]
#         plot <- ggplot(results) +
#             geom_line(aes(x = time + input$date_range[1], y = value / 1000)) +
#             labs(x = NULL, y = "Infection prevalence (thousands)") +
#             ylim(0, NA)
# 
#         overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
#     })

## ----eval = FALSE-------------------------------------------------------------
# library(shiny)
# library(ggplot2)
# library(epidemics)
# library(overshiny)
# 
# # Model run function
# run_model <- function(input, ...)
# {
#     # Transform parameters
#     I0 <- input$init_infec / 100; # Percent to proportion
#     duration <- as.numeric(input$date_range[2] - input$date_range[1]) # Dates to duration
#     infec_rate <- 1 / input$latent_pd # Latent period to infectiousness rate
#     recov_rate <- 1 / input$infec_pd  # Infectious period to recovery rate
#     trans_rate <- input$R0 * recov_rate # R0 to transmission rate (R_0 = beta / gamma)
# 
#     # Build population
#     pop <- population(
#         name = "Utopia",
#         contact_matrix = matrix(1),
#         demography_vector = input$pop_size * 1000000, # Population size in millions
#         initial_conditions = matrix(c(1 - I0, 0, I0, 0, 0), nrow = 1)
#     )
# 
#     # Run model
#     results <- model_default(pop, transmission_rate = trans_rate,
#         infectiousness_rate = infec_rate, recovery_rate = recov_rate,
#         time_end = duration, ...)
# 
#     return (results)
# }
# 
# # --- User interface ---
# ui <- fluidPage(
#     titlePanel("SEIRV model with interventions"),
# 
#     # NEW PART IS HERE:
#     # Main plot
#     overlayPlotOutput("display", width = "100%", height = 400),
#     # END OF NEW PART
# 
#     # 3 columns of inputs
#     fluidRow(
#         column(4,
#             # Basic epidemic settings
#             h3("Epidemic"),
#             dateRangeInput("date_range", "Date range", start = "2025-01-01", end = "2025-12-31"),
#             numericInput("pop_size", "Population size (millions)", value = 59, min = 1),
#             sliderInput("init_infec", "Initial proportion infectious (%)", value = 0.1, min = 0, max = 1, step = 0.01)
#         ),
# 
#         column(4,
#             # Pathogen settings
#             h3("Pathogen"),
#             sliderInput("R0", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"),
#                 value = 1.3, min = 0, max = 5, step = 0.05),
#             sliderInput("latent_pd", "Latent period (days)", value = 2, min = 1, max = 7, step = 0.1),
#             sliderInput("infec_pd", "Infectious period (days)", value = 7, min = 1, max = 10, step = 0.1)
#         ),
# 
#         column(4,
#             # Overlay controls: tokens that can be dragged onto the plot
#             h3("Interventions"),
#             overlayToken("vx", "Vaccination"),
#             overlayToken("tx", "Transmission")
#         )
#     )
# )
# 
# # --- App logic ---
# server <- function(input, output)
# {
#     # --- OVERLAY SETUP ---
# 
#     # Initialise 8 draggable/resizable overlays
#     ov <- overlayServer("display", 8)
# 
#     # --- RENDERING OF EPI CURVES ---
# 
#     output$display <- renderPlot({
#         # Create interventions
#         tx_int <- list()
#         vax <- NULL
# 
#         # Apply overlays as interventions
#         for (i in which(ov$active)) {
#             begin <- ov$cx0[i] - as.numeric(input$date_range[1])
#             end <- ov$cx1[i] - as.numeric(input$date_range[1])
#             if (ov$label[i] == "Vaccination") {
#                 nu <- 0.01 # proportion of population vaccinated per day
#                 if (is.null(vax)) {
#                     vax <- vaccination(name = as.character(i), nu = matrix(nu),
#                         time_begin = matrix(begin), time_end = matrix(end))
#                 } else {
#                     ov$active[i] <- FALSE
#                 }
#             } else if (ov$label[i] == "Transmission") {
#                 reduc <- 0.5 # reduction in transmission
#                 tx_int[[length(tx_int) + 1]] <- intervention(name = as.character(i),
#                     type = "rate", time_begin = matrix(begin), time_end = matrix(end),
#                     reduction = reduc)
#             }
#         }
# 
#         # Put interventions in the right format
#         int <- list()
#         if (length(tx_int)) {
#             int[["transmission_rate"]] <- do.call(c, tx_int)
#         }
# 
#         # Run model
#         results <- run_model(input,
#             vaccination = vax,
#             intervention = if (length(int)) int else NULL)
# 
#         # Process results (this is the same as before)
#         results <- results[results$compartment == "infectious", ]
#         plot <- ggplot(results) +
#             geom_line(aes(x = time + input$date_range[1], y = value / 1000)) +
#             labs(x = NULL, y = "Infection prevalence (thousands)") +
#             ylim(0, NA)
# 
#         overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
#     })
# }
# 
# # --- Run app ---
# shinyApp(ui, server)

## ----eval = FALSE-------------------------------------------------------------
#     output$display <- renderPlot({
#         results <- run_model(input)
#         results <- results[results$compartment == "infectious", ]
#         plot <- ggplot(results) +
#             geom_line(aes(x = time + input$date_range[1], y = value / 1000)) +
#             labs(x = NULL, y = "Infection prevalence (thousands)") +
#             ylim(0, NA)
# 
#         overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
#     })

## ----eval = FALSE-------------------------------------------------------------
#         # Create interventions
#         tx_int <- list()
#         vax <- NULL

## ----eval = FALSE-------------------------------------------------------------
#         # Apply overlays as interventions
#         for (i in which(ov$active)) {
#             begin <- ov$cx0[i] - as.numeric(input$date_range[1])
#             end <- ov$cx1[i] - as.numeric(input$date_range[1])
#             if (ov$label[i] == "Vaccination") {
#                 nu <- 0.01 # proportion of population vaccinated per day
#                 if (is.null(vax)) {
#                     vax <- vaccination(name = as.character(i), nu = matrix(nu),
#                         time_begin = matrix(begin), time_end = matrix(end))
#                 } else {
#                     ov$active[i] <- FALSE
#                 }
#             } else if (ov$label[i] == "Transmission") {
#                 reduc <- 0.5 # reduction in transmission
#                 tx_int[[length(tx_int) + 1]] <- intervention(name = as.character(i),
#                     type = "rate", time_begin = matrix(begin), time_end = matrix(end),
#                     reduction = reduc)
#             }
#         }

## ----eval = FALSE-------------------------------------------------------------
#         # Put interventions in the right format
#         int <- list()
#         if (length(tx_int)) {
#             int[["transmission_rate"]] <- do.call(c, tx_int)
#         }
# 
#         # Run model
#         results <- run_model(input,
#             vaccination = vax,
#             intervention = if (length(int)) int else NULL)

## ----eval = FALSE-------------------------------------------------------------
# library(epidemics)
# library(shiny)
# library(overshiny)
# library(ggplot2)
# 
# # --- User interface ---
# ui <- fluidPage(
#     titlePanel("SEIRV model with interventions"),
# 
#     # Main plot with support for overlays
#     overlayPlotOutput("display", width = "100%", height = 400),
# 
#     # 3 columns of inputs
#     fluidRow(
#         column(4,
#             # Basic epidemic settings
#             h3("Epidemic"),
#             dateRangeInput("date_range", "Date range", start = "2025-01-01", end = "2025-12-31"),
#             numericInput("pop_size", "Population size (millions)", value = 59, min = 1),
#             sliderInput("init_infec", "Initial proportion infectious (%)", value = 0.1, min = 0, max = 1, step = 0.01)
#         ),
# 
#         column(4,
#             # Pathogen settings
#             h3("Pathogen"),
#             sliderInput("R0", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"),
#                 value = 1.3, min = 0, max = 5, step = 0.05),
#             sliderInput("latent_pd", "Latent period (days)", value = 2, min = 1, max = 7, step = 0.1),
#             sliderInput("infec_pd", "Infectious period (days)", value = 7, min = 1, max = 10, step = 0.1)
#         ),
# 
#         column(4,
#             # Overlay controls: tokens that can be dragged onto the plot
#             h3("Interventions"),
#             overlayToken("vx", "Vaccination"),
#             overlayToken("tx", "Transmission")
#         )
#     )
# )
# 
# # --- App logic ---
# server <- function(input, output)
# {
#     # --- OVERLAY SETUP ---
# 
#     # Dropdown menu for overlays
#     menu <- function(ov, i) {
#         if (ov$label[i] == "Vaccination") {
#             numericInput("display_vac_rate", "Vaccines per day (thousands)",
#                 value = ov$data$vac_rate[i], min = 0, max = 10000)
#         } else if (ov$label[i] == "Transmission") {
#             sliderInput("display_int_strength", "Transmission reduction (%)",
#                 value = ov$data$int_strength[i], min = 0, max = 100)
#         }
#     }
# 
#     # Initialise 8 draggable/resizable overlays
#     ov <- overlayServer("display", 8, width = 56, # 56 days = 8 weeks default width
#         data = list(vac_rate = 10, int_strength = 20),
#         snap = snapGrid(),
#         heading = dateHeading("%b %e"),
#         select = TRUE,
#         menu = menu)
# 
#     # --- EPIDEMIC MODEL RUNS BASED ON OVERLAY POSITIONS ---
# 
#     # Model run function
#     run_model <- function(...)
#     {
#         # Transform parameters
#         I0 <- input$init_infec / 100;
#         duration <- as.numeric(input$date_range[2] - input$date_range[1])
#         infec_rate <- 1 / input$latent_pd
#         recov_rate <- 1 / input$infec_pd
#         trans_rate <- input$R0 * recov_rate
# 
#         # Build population
#         pop <- population(
#             name = "Utopia",
#             contact_matrix = matrix(1),
#             demography_vector = input$pop_size * 1000000,
#             initial_conditions = matrix(c(1 - I0, 0, I0, 0, 0), nrow = 1)
#         )
# 
#         # Run model (with additional parameters from ...)
#         results <- model_default(pop, transmission_rate = trans_rate,
#             infectiousness_rate = infec_rate, recovery_rate = recov_rate,
#             time_end = duration, ...)
# 
#         # Transform results -- construct date and only return infection prevalence
#         results$date <- results$time + input$date_range[1]
#         results <- results[results$compartment == "infectious", ]
#         return (results)
#     }
# 
#     # Unmitigated epidemic
#     epi_unmitigated <- reactive({
#         run_model()
#     })
# 
#     # Mitigated epidemic
#     epi_mitigated <- reactive({
#         # Create interventions
#         tx_int <- list()
#         vax <- NULL
#         for (i in which(ov$active)) {
#             begin <- ov$cx0[i] - as.numeric(input$date_range[1])
#             end <- ov$cx1[i] - as.numeric(input$date_range[1])
#             if (ov$label[i] == "Vaccination") {
#                 nu <- ov$data$vac_rate[i] * 1000 / (input$pop_size * 1000000)
#                 if (is.null(vax)) {
#                     vax <- vaccination(name = as.character(i), nu = matrix(nu),
#                         time_begin = matrix(begin), time_end = matrix(end))
#                 } else {
#                     ov$active[i] <- FALSE
#                 }
#             } else if (ov$label[i] == "Transmission") {
#                 reduc <- ov$data$int_strength[i] / 100
#                 tx_int[[length(tx_int) + 1]] <- intervention(name = as.character(i),
#                     type = "rate", time_begin = matrix(begin), time_end = matrix(end),
#                     reduction = reduc)
#             }
#         }
# 
#         # Get mitigated model results
#         int <- list()
#         if (length(tx_int)) {
#             int[["transmission_rate"]] <- do.call(c, tx_int)
#         }
#         run_model(vaccination = vax,
#             intervention = if (length(int)) int else NULL)
#     })
# 
#     # --- RENDERING OF EPI CURVES ---
# 
#     # Render plot and align overlays to current axis limits
#     output$display <- renderPlot({
#         plot <- ggplot() +
#             geom_line(data = epi_unmitigated(),
#                 aes(x = date, y = value/1000), alpha = 0.5) +
#             geom_line(data = epi_mitigated(),
#                 aes(x = date, y = value/1000)) +
#             labs(x = NULL, y = "Infection prevalence (thousands)") +
#             ylim(0, NA)
# 
#         overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
#     })
# }
# 
# # --- Run app ---
# shinyApp(ui, server)

