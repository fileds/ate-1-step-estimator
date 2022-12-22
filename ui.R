library(shiny)
library(shinydashboard)

source("R/dgps.R")
source("R/estimate_ate_along_path.R")
source("R/influence_function_ate.R")
source("R/plots.R")

header <- dashboardHeader(
  title = "ATE 1-Step Estimator"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, title = "1-Step Estimator", status = "primary",
               plotOutput("ifPlot"),
               p(
                 class = "text-muted",
                 paste("Figure showing the ATE as a function of the ",
                       "parametrized path (solid), the 1-step estimator ",
                       "(dashed), and the empirical estimate (rightmost ",
                       "point(s)). The path is a path the DGP space ",
                       "from the empirical DGP to the true DGP as a convex ",
                       "combination between the two. The empirical estimate ",
                       "is based on model assumption and the 1-step estimator ",
                       "corrects the bias introduced by the ",
                       "model assumption asymptotically. Note: The true ATE ",
                       "is calculated on the covariate range, [-6, 6].")
               ),
           ),
               
           hr(),
               
           box(width = NULL, title = "DGPs and Sample", status = "primary",
               plotOutput("dataPlot"),
               p(
                 class = "text-muted",
                 paste("Figure showing the generated sample (points), the ", 
                 "true DGP (solid line), and the estimated DGP (dashed line).")
               ),
           )
    ),
    column(width = 3,
           box(width = NULL, status = "warning", title = "Generate Sample",
               sliderInput("sampleSize", "Sample size", min = 40, max = 1000, 
                       value = 100, step = 10),
               
               hr(),
               
               actionButton(inputId = "generateButton", 
                            label = "Generate sample")
           ),
           box(width = NULL, status = "warning", title = "Data Generating Process",
               radioButtons('dgpType', 'DGP Type', 
                            choiceNames = c("Linear",
                                            "Quadratic",
                                            "2nd deg. polynomial",
                                            "3rd deg. polynomial"),
                            choiceValues = c(1, 2, 3, 4),
                            selected = 1, 
                            inline = FALSE),
               p(
                 class = "text-muted",
                 paste("Select the underlying data generating process (DGP). ",
                 "You can visualize the DGP as the 'True' curve in the 'DGPs ",
                 "and Sample' figure.")
               ),
               
               hr(),
               
               sliderInput("varianceControl", "Variance of error of M0", 
                           min = 0.1, max = 1, value = 0.5, step = 0.1),
               
               sliderInput("varianceTreated", "Variance of error of M1", 
                           min = 0.1, max = 1, value = 0.5, step = 0.1),
               p(
                 class = "text-muted",
                 paste("Increase or decrease the variance in the treated and ",
                 "control population.")
               ),
           ),
           box(width = NULL, status = "warning", title = "Model",
               checkboxGroupInput('modelTypes', 'Model', 
                            choices = c("True",
                                        "Linear",
                                       "Quadratic",
                                       "Kernel"),
                            selected = c("True", "Linear"), 
                            inline = FALSE),
               
               p(
                 class = "text-muted",
                 paste("Select the model(s) to use in estimation of M0 and M1. ",
                 "You can visualize the estimation in the 'DGPs and Sample' ",
                 "figure.")
               ),
               
               hr(),
               
               sliderInput("kernelBw", "Kernel Bandwidth", 
                           min = 0.1, max = 24, value = 2, step = 0.1),
               
               p(
                 class = "text-muted",
                 paste("Select bandwith used in Kernel regression. ",
                       "Has no effect on 'Linear' or 'Quadratic'.")
               ),
               
               actionButton(inputId = "updateBandwidth", 
                            label = "Update Bandwidth")
               
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = T),
  body
)