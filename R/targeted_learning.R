source("R/dgps.R")
# Targeted learning as suggested in Demystifying
targeted_learning <- function(df, estimated, step_size=1)
{
  # Covariate
  x <- df$x
  # Treatment assignment
  tr <- df$tr
  # Observed outcome
  y <- df$y
  # m0 estimated 
  m0 <- estimated$m0
  # m1 estimated 
  m1 <- estimated$m1
  # Propensity score 
  ps <- estimated$propensity_score
  
  epsilon_0 <- (mean((1 - tr) * (y - m0(x)) / (1 - ps(x))) / 
              mean((1 - tr) * 1 / (1 - ps(x)) ^ 2))
  
  epsilon_1 <- (mean(tr * (y - m1(x)) / ps(x)) / 
              mean(tr * 1 / ps(x) ^ 2))
  
  targeted <- list(
    m0 = approxfun(x, m0(x) + step_size * epsilon_0 / (1 - ps(x))),
    m1 = approxfun(x, m1(x) + step_size * epsilon_1 / ps(x)),
    propensity_score = ps)
  
  return(targeted)
}

mix_sample <- function(df, estimated, n_new, replace=FALSE)
{
  # Number of observations
  n <- nrow(df)
  # Covariate
  x <- df$x
  # Treatment assignment
  tr <- df$tr
  # Observed outcome
  y <- df$y
  y0 <- filter(df, tr == 0)$y
  y1 <- filter(df, tr == 1)$y
  # m0 estimated
  m0 <- estimated$m0
  # m1 estimated 
  m1 <- estimated$m1
  # Propensity score 
  ps <- estimated$propensity_score
  
  # Noise
  # NOTE: We are using the same noise generation as the truth. This is an 
  # assumption.
  e0 = rnorm(n_new, 0, sd(y0))
  e1 = rnorm(n_new, 0, sd(y1))
  
  # Covariate
  x_new = runif(n_new, min(x), max(x))
  
  # Potential outcomes
  y0_new = m0(x_new) + e0
  y1_new = m1(x_new) + e1
  
  # Treatment assignment
  tr_new <- rbinom(n_new, 1, ps(x_new))
  
  # Observed outcomes
  y_new = tr_new * y1_new + (1 - tr_new) * y0_new
  
  if (replace)
  {
    idx <- 1:n
    idx <- idx[! idx %in% c(which(x == min(x)), which(x == max(x)))]
    idx <- sample(idx, n_new)
    
    x[idx] <- x_new
    tr[idx] <- tr_new
    y[idx] <- y_new
  }
  else
  {
    x <- c(x, x_new)
    tr <- c(tr, tr_new)
    y <- c(y, y_new)
  }
  
  return(data.frame(y = y, tr = tr, x = x))
}

estimate_dgp <- function(df, model)
{
  if (model == "Linear")
    estimated <- estimate_linear(df)
  else if (model == "Quadratic")
    estimated <- estimate_quadratic(df)
  else
    estimated <- estimate_kernel(df, input$kernelBw)
  
  return(estimated)
}

# Iterated targeted learning???
iterated_targeted_learning <- function(df, estimated, model, n_iterations=10)
{
  n <- nrow(df)
  lr <- 0.001
  
  targeted <- targeted_learning(df, estimated, lr)
  last_ate <- mean(targeted$m1(df$x) - targeted$m0(df$x))
  diff <- 10
  while (diff > 1e-4)
  {
    df <- mix_sample(df, targeted, n / 20, replace = TRUE)
    estimated <- estimate_dgp(df, model)
    estimated$m1 <- function(x) { 0.1 * estimated$m1(x) + 0.9 * targeted$m1(x) }
    estimated$m0 <- function(x) { 0.1 * estimated$m0(x) + 0.9 * targeted$m0(x) }
    estimated$propensity_score <- function(x) { 0.1 * estimated$propensity_score(x) + 0.9 * targeted$propensity_score(x) }
    current_ate <- mean(estimated$m1(df$x) - estimated$m0(df$x))
    diff <- abs(current_ate - last_ate)
    last_ate <- current_ate
    targeted <- targeted_learning(df, estimated, lr)
    print(sprintf("Targeted ATE = %.3f\t Diff = %.6f\t Sample size = %d", current_ate, diff, nrow(df)))
  }
  cat("\n")
  
  targeted$df <- df
  
  return(targeted)
}