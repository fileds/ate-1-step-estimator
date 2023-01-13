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
    
    ## Get iterated targeted density
    #dgps$estimated[["Targeted2"]] <- iterated_targeted_learning(observations$df, 
    #    dgps$estimated[[input$targetedBase]], 2)
    #
    ## Get iterated targeted density
    #dgps$estimated[["Targeted3"]] <- iterated_targeted_learning(observations$df, 
    #    dgps$estimated[[input$targetedBase]], 3)
    #
    ## Get iterated targeted density
    #dgps$estimated[["Targeted4"]] <- iterated_targeted_learning(observations$df, 
    #    dgps$estimated[[input$targetedBase]], 4)
    #
    ## Get iterated targeted density
    #dgps$estimated[["Targeted5"]] <- iterated_targeted_learning(observations$df, 
    #    dgps$estimated[[input$targetedBase]], 5)
    #
    ## Get iterated targeted density
    #dgps$estimated[["Targeted10"]] <- iterated_targeted_learning(observations$df, 
    #    dgps$estimated[[input$targetedBase]], 10)
    #
    ## Get iterated targeted density
    #dgps$estimated[["Targeted20"]] <- iterated_targeted_learning(observations$df, 
    #    dgps$estimated[[input$targetedBase]], 20)
    
    # Get iterated targeted density
    dgps$estimated[["Targeted100"]] <- iterated_targeted_learning(observations$df, 
        dgps$estimated[[input$targetedBase]], input$targetedBase, 100)
    
    
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
  
  # Update model
  observeEvent(input$updateTargeted, {
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