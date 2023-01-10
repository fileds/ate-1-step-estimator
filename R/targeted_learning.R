targeted_learning <- function(df, estimated)
{
  # Covariate
  x <- df$x
  # Treatment assignment
  tr <- df$tr
  # Observed outcome
  y <- df$y
  # m0 estimated values
  m0_hat <- estimated$m0(x)
  # m1 estimated values
  m1_hat <- estimated$m1(x)
  # Propensity score values
  propensity_score <- estimated$propensity_score(x)
  
  targeted <- list()
  
  epsilon_0 <- (mean((1 - tr) * (y - m0_hat) / (1 - propensity_score)) / 
              mean((1 - tr) * 1 / (1 - propensity_score) ^ 2))
  targeted$m0 <- approxfun(x, m0_hat + epsilon_0 / (1 - propensity_score))
  
  epsilon_1 <- (mean(tr * (y - m1_hat) / propensity_score) / 
              mean(tr * 1 / propensity_score ^ 2))
  targeted$m1 <- approxfun(x, m1_hat + epsilon_1 / propensity_score)
  
  targeted$propensity_score <- estimated$propensity_score
  
  
  return(targeted)
}