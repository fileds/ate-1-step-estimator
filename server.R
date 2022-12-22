library(shiny)

source("R/dgps.R")
source("R/estimate_ate_along_path.R")
source("R/influence_function_ate.R")
source("R/plots.R")

server <- function(input, output) {
  # Reactive values
  ## Data generating process'
  dgps <- reactiveValues(true = NULL,
                         estimated = NULL,
                         df = NULL)
  
  ## Data
  sample <- reactiveValues(df = NULL,
                           eps = seq(0, 1, by = 0.001))
  
  ## Estimators
  estimators <- reactiveValues(ate = NULL,
                              one_step = NULL,
                              df = NULL)
  
  # TODO: Rename this function
  set_estimated_dgp <- reactive({
    req(sample$df)
    
    # Estimating m0, m1, and propensity score
    dgps$estimated <- estimate_dgps(sample, input)
    
    # Estimate the DGPs in the range of the sample
    dgps$df <- calculate_dgp_values(sample, dgps)
    
    # Calculating ATE along parametrized path
    ate <- estimate_ate_along_path(sample, dgps)
    
    # Create data frame for estimators along path and one step estimators.
    estimators$df <- one_step_ate(sample, dgps, ate)
  })
  
  # Generate sample, calculate estimator and influence functions
  observeEvent(input$generateButton, {
    showModal(modalDialog(
      "Generating sample, calculating ATE and influence functions...", 
      footer=NULL))
    
    # Define DGP according to input
    dgps$true <- define_dgp(input)
    
    # Generating sample
    sample$df <- dgps$true$random_generator(input$sampleSize)
    
    # Set the estimated DGP
    set_estimated_dgp()    
    
    removeModal()
  })
  
  # Update model
  observeEvent(input$updateModel, {
    req(sample$df)
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
    req(sample$df)
    data_plot(sample, dgps, input)
  })
  
  ## IF plot
  output$ifPlot <- renderPlot({
    req(estimators$df, length(input$modelTypes) > 0)
    if_plot(estimators, input)
  })
}