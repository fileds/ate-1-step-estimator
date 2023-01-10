# Generate the DGP functions. dmix is the distribution function and rmix is the 
# sample generator. This should work scope-wise: https://cran.r-project.org/doc/manuals/R-lang.html#Scope-of-variables
define_dgp <- function(input) 
{
  # The selected DGP type
  dgp_type <- input$dgpType
  # Treated noise std
  std_treated <- force(input$stdTreated)
  # Control noise std
  std_control <- force(input$stdControl)
  
  # Define density
  # TODO: Change dgp type to strings.
  density_ = switch (dgp_type,
                     '1' = list(m0 = function(x){ return(x / 3 - 1) },
                                m1 = function(x){ return(x / 3 + 1) },
                                p =  function(x){ return(1 / (1 + exp(.2 * x)) ) }),
                     
                     '2' = list(m0 = function(x){ return(-x^2 / 18 - 1) },
                                m1 = function(x){ return(x^2 / 18 + 1) },
                                p =  function(x){ return(1 / (1 + exp(.2 * x)) ) }),
                     
                     '3' = list(m0 = function(x){ return(-(x-2)^2 / 24 - 1) },
                                m1 = function(x){ return((x+3)^2 / 36 + 1) },
                                p =  function(x){ return(1 / (1 + exp(.2 * x)) ) }),
                     
                     '4' = list(m0 = function(x){ return(-(x+2)^3 / 288 - 1) },
                                m1 = function(x){ return((x-3)^3 / 378 + 1) },
                                p =  function(x){ return(1 / (1 + exp(.2 * x)) ) }))
  
  random_generator <- function(n)
  {
    # Noise
    e0 = rnorm(n, 0, std_control)
    e1 = rnorm(n, 0, std_treated)
    
    # Covariate
    x = runif(n, -6, 6)
    
    # Potential outcomes
    y0 = density_$m0(x) + e0
    y1 = density_$m1(x) + e1
    
    # Propensity score
    p = density_$p(x)
    
    # Treatment assignment
    tr <- rbinom(n, 1, p)
    
    # Observed outcomes
    y = tr * y1 + (1 - tr) * y0
    
    return(data.frame(y = y, tr = tr, x = x))
  }
  
  return(list(density = density_, random_generator = random_generator))
}

estimate_linear <- function(df)
{
  # Predict values for m0, m1, and propensity score (ps)
  m0_values <- predict(lm('y ~ x', filter(df, tr == 0)), 
                       newdata = data.frame(x = df$x))
  m1_values <- predict(lm('y ~ x', filter(df, tr == 1)), 
                       newdata = data.frame(x = df$x))
  
  # Create functions, dgps, based on the predicted values. The domain of the 
  # functions are the range of the sample.
  m0 <- approxfun(x = df$x, y = m0_values)
  m1 <- approxfun(x = df$x, y = m1_values)
  
  # Create function for propensity score
  ps_values <- glm('tr ~ x', df, family = 'binomial')$fitted.values
  propensity_score <- approxfun(x = df$x,
                                y = ps_values)
  
  return(list(m0 = m0, m1 = m1, propensity_score = propensity_score))
}

estimate_quadratic <- function(df)
{
  # Predict values for m0, m1, and propensity score (ps)
  m0_values <- predict(lm('y ~ x + I(x^2)', filter(df, tr == 0)), 
                       newdata = data.frame(x = df$x))
  m1_values <- predict(lm('y ~ x + I(x^2)', filter(df, tr == 1)), 
                       newdata = data.frame(x = df$x))
  
  # Create functions, dgps, based on the predicted values. The domain of the 
  # functions are the range of the sample.
  m0 <- approxfun(x = df$x, y = m0_values)
  m1 <- approxfun(x = df$x, y = m1_values)
  
  # Create function for propensity score
  ps_values <- glm('tr ~ x', df, family = 'binomial')$fitted.values
  propensity_score <- approxfun(x = df$x,
                                y = ps_values)
  
  return(list(m0 = m0, m1 = m1, propensity_score = propensity_score))
}

estimate_kernel <- function(df, bw = 0.5)
{
  # Select x, y for treated and control
  x0 <- unlist(select(filter(df, tr == 0), x))
  y0 <- unlist(select(filter(df, tr == 0), y))
  
  x1 <- unlist(select(filter(df, tr == 1), x))
  y1 <- unlist(select(filter(df, tr == 1), y))
  
  # Create functions based on kernel estimation
  m0 <- approxfun(
    ksmooth(x0, y0, bandwidth = bw, range.x = range(df$x)), rule = 2)
  m1 <- approxfun(
    ksmooth(x1, y1, bandwidth = bw, range.x = range(df$x)), rule = 2)
  
  # Create function for propensity score
  ps_values <- glm('tr ~ x', df, family = 'binomial')$fitted.values
  propensity_score <- approxfun(x = df$x,
                                y = ps_values)
  
  return(list(m0 = m0, m1 = m1, propensity_score = propensity_score))
}

estimate_dgps <- function(sample, input)
{
  estimated <- list()
  estimated[["Linear"]] <- estimate_linear(sample$df)
  estimated[["Quadratic"]] <- estimate_quadratic(sample$df)
  estimated[["Kernel"]] <- estimate_kernel(sample$df, input$kernelBw)
  
  return(estimated)
}

calculate_dgp_values <- function(sample, dgps)
{
  # m0 true
  m0 <- dgps$true$density$m0
  # m1 true
  m1 <- dgps$true$density$m1
  
  # Linear space over the sample covariate range
  x_linspace <- seq(min(sample$df$x), max(sample$df$x), length.out = 1000)
  
  # Data frame containing the linear space over the sample covariate range, the
  # outcomes for true and estimated DGPs, the treatment indicator, and the type
  # of DGP.
  df_dgps <- rbind(
    data.frame(x = x_linspace, 
               y = m0(x_linspace),
               tr = rep("Control", length(x_linspace)), 
               model = rep("True", length(x_linspace))), 
    data.frame(x = x_linspace, 
               y = m1(x_linspace),
               tr = rep("Treated", length(x_linspace)), 
               model = rep("True", length(x_linspace)))
  )
  # Adding all estimated DGPs.
  for (model in names(dgps$estimated))
  {
    # m0 estimated
    m0_hat <- dgps$estimated[[model]]$m0
    # m1 estimated
    m1_hat <- dgps$estimated[[model]]$m1
    
    df_dgps <- rbind(
      df_dgps,
      data.frame(x = x_linspace, 
                 y = m0_hat(x_linspace),
                 tr = rep("Control", length(x_linspace)), 
                 model = rep(model, length(x_linspace))), 
      data.frame(x = x_linspace, 
                 y = m1_hat(x_linspace),
                 tr = rep("Treated", length(x_linspace)), 
                 model = rep(model, length(x_linspace)))
      )
  }
  
  # Sort DGPs to ensure True is first DGP
  df_dgps$model <- factor(df_dgps$model, 
                        levels = c("True", names(dgps$estimated)))
  
  return(df_dgps)
}