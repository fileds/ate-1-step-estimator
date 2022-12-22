library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(ggplot2)
library(ggrepel)

# Tableau 20 colorscheme
tableau20 <- c('#4E79A7', # Blue
               '#A0CBE8', # Light Blue
               '#F28E2B', # Orange
               '#FFBE7D', # Light Orange
               '#59A14F', # Green
               '#8CD17D', # Light Green
               '#B6992D', # Yellow-Green
               '#F1CE63', # Yellow
               '#499894', # Teal
               '#86BCB6', # Light Teal
               '#E15759', # Red
               '#FF9D9A', # Pink
               '#FABFD2', # Light Pink
               '#B07AA1', # Purple
               '#D4A6C8', # Light Purple
               '#9D7660', # Brown
               '#D7B5A6') # Tan)
               
alpha_f <- function(n, n_max = 1000, alpha_min = 0.15, alpha_max = 1)
{
  a <- (log(n_max) - log(n)) / log(n_max) + alpha_min
  if (a > alpha_max) a <- alpha_max
  return(a)
}


# Plotting the sample
data_plot <- function(sample, dgps, input)
{ 
  # Color palette
  pal <- tableau20[c(5, 11)]
  # Color palette
  #pal <- list(
  #  "Linear" = tableau20[1],
  #  "Kernel" = tableau20[3],
  #  "Quadratic" = tableau20[5],
  #  "True" = "#000000")
  
  # lineshapes
  lshp <- list(
    "True" = "solid",
    "Linear" = "dashed",
    "Quadratic" = "longdash",
    "Kernel" = "dotdash")
  
  # Sample data frame
  df_sample <- sample$df %>%
    mutate(tr = case_when(
      tr == 1 ~ "Treated",
      TRUE ~ "Control")) %>%
    mutate(tr = factor(tr, levels = c("Treated", "Control")))
  
  # Filter checked model types
  df_dgps <- filter(dgps$df, model %in% input$modelTypes)
  
  ggplot() +
    geom_point(
      data = df_sample, 
      mapping = aes(x = x, y = y, col = tr, shape = tr), 
      size = 6,
      alpha = alpha_f(nrow(df_sample))) +
    geom_line(
      data = df_dgps,
      mapping = aes(x = x, y = y, linetype = model, col = tr), 
      linewidth = 1.5,
      alpha = 0.8) + 
    scale_color_manual(name = "Treatment", 
                       values = pal) +
    scale_shape_discrete(name = "Treatment") + 
    scale_linetype_manual(name = "Model",
                          values = unlist(lshp[unique(df_dgps$model)])) + 
    xlab("Covariate") +
    ylab("Outcome") +
    theme(
      text = element_text(size = 18)
    )
}

if_plot <- function(estimators, input)
{
  # Color palette
  pal <- list(
    "Linear" = tableau20[1],
    "Kernel" = tableau20[14],
    "Quadratic" = tableau20[3])
  
  # Filter checked model types
  df <- estimators$df %>%
    filter(model %in% input$modelTypes)
  
  # Select points for plotting empirical, true, and 1-step points.
  df_points <- df %>%
    group_by(model, type) %>%
    summarise(
      x = c(first(eps), last(eps)),
      y = c(first(ate), last(ate))) %>%
    ungroup() %>%
    filter(!(type == "1-Step" & x == 1)) %>%
    mutate(type = case_when(
      type == "1-Step" ~ "1-Step",
      x == 0 & type == "ATE" ~ "True",
      TRUE ~ "Empirical"))
  
  # Select one model for text labelling.
  # TODO: Make selection consistent when adding models.
  df_repel <- df_points %>%
    filter(model == unique(model)[1])
  
  ggplot() +
    geom_line(
      data = df,
      mapping = aes(x = eps, y = ate, linetype = type, col = model), 
      linewidth = 2,
      alpha = 0.6) + 
    geom_point(
      data = df_points, 
      mapping = aes(x = x, y = y, col = model), 
      size = 8
      ) +
    geom_text_repel(
      data = df_repel,
      mapping = aes(x = x, y = y, label = type),
      size = 8,
      nudge_y = diff(range(df$ate)) / 6,
      xlim = c(0.1, 0.9),
      segment.size  = 0.2,
      segment.color = "grey50",
      inherit.aes = FALSE) +
    scale_color_manual(name = "Model", 
                       values = unlist(pal[unique(df$model)])) +
    scale_linetype_manual("Estimator", values=c("dashed", "solid", "solid")) +
    xlab("Path") +
    ylab("ATE") +
    theme(
      text = element_text(size = 18)
    )
}