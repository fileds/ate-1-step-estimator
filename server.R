library(shiny)

source("R/dgps.R")
source("R/estimate_ate_along_path.R")
source("R/influence_function_ate.R")
source("R/targeted_learning.R")
source("R/plots.R")

server <- function(input, output) {
  # Reactive values
  ## Data generating process'
  dgps <- reactiveValues(true = NULL,
                         estimated = NULL,
                         df = NULL)
  
  ## Data
  observations <- reactiveValues(df = NULL,
                           eps = seq(0, 1, by = 0.001))
  
  ## Estimators
  estimators <- reactiveValues(ate = NULL,
                              one_step = NULL,
                              df = NULL)
  
  # TODO: Rename this function
  set_estimated_dgp <- reactive({
    req(observations$df)
    
    # Estimating m0, m1, and propensity score
    dgps$estimated <- estimate_dgps(observations, input)
    
    # Get targeted density
    dgps$estimated[["Targeted"]] <- targeted_learning(observations$df, 
        dgps$estimated[[input$targetedBase]])
    
    # Estimate the DGPs in the range of the observations
    dgps$df <- calculate_dgp_values(observations, dgps)
    
    # Calculating ATE along parametrized path
    ate <- estimate_ate_along_path(observations, dgps)
    
    # Create data frame for estimators along path and one step estimators.
    estimators$df <- one_step_ate(observations, dgps, ate)
  })
  
  # Generate observations, calculate estimator and influence functions
  observeEvent(input$generateButton, {
    showModal(modalDialog(
      "Generating observations, calculating ATE and influence functions...", 
      footer=NULL))
    
    print(1)
    # Define DGP according to input
    dgps$true <- define_dgp(input)
    
    # Generating observations
    observations$df <- dgps$true$random_generator(input$observationsSize)
    
    # Set the estimated DGP
    set_estimated_dgp()    
    
    removeModal()
  })
  
  # Update model
  observeEvent(input$updateBandwidth, {
    req(observations$df)
    showModal(modalDialog(
      "Calculating ATE and influence functions...", 
      footer=NULL))
    
    # Set the estimated DGP
    set_estimated_dgp()    
    
    removeModal()
  })
  
  # Plotting
  ## Data plot
  output$dataPlot <- renderPlot({
    req(observations$df)
    data_plot(observations, dgps, input)
  })
  
  ## IF plot
  output$ifPlot <- renderPlot({
    req(estimators$df, length(input$modelTypes) > 0)
    if_plot(estimators, input)
  })
}