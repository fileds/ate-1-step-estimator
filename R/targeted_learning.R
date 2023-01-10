# Targeted learning as suggested in Demystifying
targeted_learning <- function(df, estimated)
{
  # Covariate
  x <- df$x
  # Treatment assignment
  tr <- df$tr
  # Observed outcome
  y <- df$y
  # m0 estimated values
  m0 <- estimated$m0
  # m1 estimated values
  m1 <- estimated$m1
  # Propensity score values
  ps <- estimated$propensity_score
  
  epsilon_0 <- (mean((1 - tr) * (y - m0(x)) / (1 - ps(x))) / 
              mean((1 - tr) * 1 / (1 - ps(x)) ^ 2))
  
  epsilon_1 <- (mean(tr * (y - m1(x)) / ps(x)) / 
              mean(tr * 1 / ps(x) ^ 2))
  
  targeted <- list(
    m0 = approxfun(x, m0(x) + epsilon_0 / (1 - ps(x))),
    m1 = approxfun(x, m1(x) + epsilon_1 / ps(x)),
    propensity_score = ps)
  
  return(targeted)
}