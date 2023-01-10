influence_function_ate <- function(sample, estimated){
  # Covariate
  x <- sample$df$x
  # Treatment assignment
  tr <- sample$df$tr
  # Observed outcome
  y <- sample$df$y
  # m0 estimated values
  m0_hat <- estimated$m0(x)
  # m1 estimated values
  m1_hat <- estimated$m1(x)
  # Propensity score values
  propensity_score <- estimated$propensity_score(x)
  
  # Influence function
  # TODO: Rewrite without ifelse function. It is confusing
  if_ate = mean(
    ifelse(tr,
           (y -  m1_hat) / propensity_score,
           - (y - m0_hat)/(1 - propensity_score))
    + m1_hat - m0_hat
    )
  
  return(if_ate)
}

one_step_ate <- function(sample, dgps, ate)
{
  # Calculate influence functions
  one_step_estimators <- data.frame(eps = numeric(), ate = numeric(), 
                                    type = character(), model = character())
  for (model in names(dgps$estimated))
  {
    # Calculate influence function
    influence_function <- influence_function_ate(sample, dgps$estimated[[model]])
    
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