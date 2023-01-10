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
                 paste("Figure showing the ATE as a function of the",
                       "parametrized path (solid), the 1-step estimator",
                       "(dashed), and the empirical estimate (rightmost",
                       "point(s)). The path is a path the DGP space",
                       "from the empirical DGP to the true DGP as a convex",
                       "combination between the two. The empirical estimate",
                       "is based on model assumption and the 1-step estimator",
                       "corrects the bias introduced by the",
                       "model assumption asymptotically. Note: The true ATE",
                       "is calculated on the covariate range, [-6, 6].")
               ),
           ),
               
           box(width = NULL, title = "DGPs and Sample", status = "primary",
               plotOutput("dataPlot"),
               p(
                 class = "text-muted",
                 paste("Figure showing the generated sample (points), the", 
                 "true DGP (solid line), and the estimated DGP (dashed line).")
               ),
           )
    ),
    column(width = 3,
           box(width = NULL, status = "warning", title = "Generate Sample",
               sliderInput("observationsSize", "Sample size", min = 40, 
                           max = 1000, value = 100, step = 10),
               
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
                 paste("Select the underlying data generating process (DGP).",
                 "You can visualize the DGP as the 'True' curve in the 'DGPs",
                 "and Sample' figure.")
               ),
               
               hr(),
               
               sliderInput("stdControl", "Standard deviation of error of M0", 
                           min = 0.1, max = 1, value = 0.5, step = 0.1),
               
               sliderInput("stdTreated", "Standard deviation of error of M1", 
                           min = 0.1, max = 1, value = 0.5, step = 0.1),
               p(
                 class = "text-muted",
                 paste("Increase or decrease the standard deviation in the ",
                 "treated and control population.")
               ),
           ),
           box(width = NULL, status = "warning", title = "Model",
               fluidRow(
                 column(6, 
                 checkboxGroupInput('modelTypes', 'Model',
                                    choiceNames = c("True",
                                                    "Linear",
                                                    "2nd deg. polynomial",
                                                    "Kernel",
                                                    "Targeted"),
                                    choiceValues = c("True",
                                                     "Linear",
                                                     "Quadratic",
                                                     "Kernel",
                                                     "Targeted"),
                              selected = c("True", "Linear"), 
                              inline = FALSE),
                 ),
                 
                 column(6,
                 radioButtons('targetedBase', 'Targeted base',
                                    choiceNames = c("Linear",
                                                    "2nd deg. polynomial",
                                                    "Kernel"),
                                    choiceValues = c("Linear",
                                                     "Quadratic",
                                                     "Kernel"),
                              selected = "Linear", 
                              inline = FALSE),
                 )
               ),
               
               p(
                 class = "text-muted",
                 paste("Select the model(s) to use in estimation of M0 and M1.",
                 "You can visualize the estimation in the 'DGPs and Sample'",
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

#footer_text <- p("Filip Edström and Mohammad Ghasempour, ", 
#                 a(href="https://www.stat4reg.se/home", "Stat4Reg"), 
#                 ", Umeå University, 2022.", "For more information, feedback and contact please visit ",
#                 a(href="https://github.com/fileds/ate-1-step-estimator", "GitHub/fileds")) 
  

ui <- tagList(
  dashboardPage(
    header,
    dashboardSidebar(disable = T),
    body)
  #tags$footer(p(footer_text), 
  #            align = "center", 
  #            style = "
  #              position:relative;
  #              text-align: center;
  #              bottom:0;
  #              width:100%;
  #              height:50px;   /* Height of the footer */
  #              color: #666;
  #              padding: 10px;
  #              background-color: #ECF0F5;
  #              z-index: 1000;")
)