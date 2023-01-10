influence_function_ate <- function(df, estimated){
  # Covariate
  x <- df$x
  # Treatment assignment
  tr <- as.logical(df$tr)
  # Observed outcome
  y <- df$y
  # m0 estimated values
  m0 <- estimated$m0
  # m1 estimated values
  m1 <- estimated$m1
  # Propensity score values
  ps <- estimated$propensity_score
  
  # Efficient influence function
  eif = mean(
    tr * (y -  m1(x)) / ps(x) 
    - (1 - tr) * (y - m0(x))/(1 - ps(x))
    + m1(x) - m0(x)
  )
  
  return(eif)
}

one_step_ate <- function(sample, dgps, ate)
{
  # Calculate influence functions
  one_step_estimators <- data.frame(eps = numeric(), ate = numeric(), 
                                    type = character(), model = character())
  for (model in names(dgps$estimated))
  {
    # Calculate influence function
    influence_function <- influence_function_ate(sample$df, dgps$estimated[[model]])
    
    # Get the empirical estimate
    ate_empirical <- tail(ate[[model]], 1)
    
    # Calculate the one step estimator along the path (a line).
    one_step <- seq(influence_function, ate_empirical, 
                    length.out = length(sample$eps))
    
    # Store the result
    one_step_estimators <- rbind(
      one_step_estimators,
      data.frame(eps = sample$eps, 
                 ate = ate[[model]], 
                 type = rep("ATE", length(sample$eps)),
                 model = rep(model, length(sample$eps))),
      data.frame(eps = sample$eps, 
                 ate = one_step, 
                 type = rep("1-Step", length(sample$eps)),
                 model = rep(model, length(sample$eps)))
    )
  }
  
  return(one_step_estimators)
}