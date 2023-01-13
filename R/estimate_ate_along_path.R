# Calculates the values of the estimator along the parametrized path.
estimate_ate_along_path <- function(sample, dgps) {
  # Covariate
  x <- sample$df$x
  # Path parametrization
  eps <- sample$eps
  # m0 true
  m0 <- dgps$true$density$m0
  # m1 true
  m1 <- dgps$true$density$m1
  
  ate_along_paths <- list()
  for (model in names(dgps$estimated))
  {
    print(model)
    # m0 estimated
    m0_hat <- dgps$estimated[[model]]$m0
    # m1 estimated
    m1_hat <- dgps$estimated[[model]]$m1
  
    # This is the hairy part. The estimator is calculated along the path 
    # parametrized by epsilon. This calculation is split up in the following steps
    # TODO: Insert latex code for the steps.
    # TODO: THink about integration limits.
    i_1 <- (1 - eps)^2 * integrate(function(t) m1(t) - m0(t), -6, 6)$value / 12
    #i_1 <- (1 - eps)^2 * integrate(function(t) m1(t) - m0(t), min(x), max(x))$value / 12
    
    # Kernel estimation may result in failed integration. Catching error and 
    # increasing tolerance.
    # TODO: Add catch when integral is divergent. 
    i_21 <- tryCatch(
    {
      i_21 <- (1 / 12) * eps * (1 - eps) * (
        integrate(m1_hat, min(x), max(x))$value 
        - integrate(m0_hat, min(x), max(x))$value)
    },
    error = function(err)
    {
      print(sprintf("ERROR in estimate_ate_along_path.R: %s", err))
      print("Increasing tolerance")
      i_21 <- (1 / 12) * eps * (1 - eps) * (
        integrate(m1_hat, min(x), max(x), rel.tol=.Machine$double.eps^.05)$value 
        - integrate(m0_hat, min(x), max(x), rel.tol=.Machine$double.eps^.05)$value)
    })
    
    i_22 <- eps * (1 - eps) * mean(m1(x) - m0(x))
    
    i_3 <- eps^2 * mean(m1_hat(x) - m0_hat(x))
    
    ate_along_paths[[model]] <- i_1 + i_21 + i_22 + i_3
  }
  
  return(ate_along_paths)
} 