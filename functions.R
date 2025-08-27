load_simple_models <- function(model_dir = "/work/Paternity-leave/01. BarselProject/niels_geospatial/analysis_code/models/") {
  
  # Get all .rds files ending with "_simple"
  simple_files <- list.files(
    path = model_dir,
    pattern = "_simple\\.rds$",
    full.names = TRUE
  )
  
  # Load each model into global environment with original names
  for (file in simple_files) {
    model_name <- basename(file)
    model_name <- gsub("\\.rds$", "", model_name)  # Remove .rds but keep _simple
    assign(model_name, readRDS(file), envir = .GlobalEnv)
  }
  
  cat("Loaded models:", paste(gsub("\\.rds$", "", basename(simple_files)), collapse = ", "), "\n")
}

extract_model_predictions <- function(model, metric_name) {
  #' Extract and format model predictions for leave status analysis
  #' 
  #' @param model A fitted Bayesian model object (e.g., from brms)
  #' @param metric_name Character string for the metric name (e.g., "Max Distance", "Radius Of Gyration")
  #' 
  #' @return A tibble with columns: value, Leave, Role, OnLeave, Metric
  #' 
  #' @examples
  #' max_distance_summary <- extract_model_predictions(model_maxDistanceFromHomeKm_simple, "Max Distance")
  #' gyration_summary <- extract_model_predictions(model_radiusOfGyrationKm_simple, "Radius Of Gyration")
  
  # Extract draws from the model
  draws <- as_draws_df(model)
  
  # Father not on leave (during maternity leave period)
  father_not_on_leave <- exp(draws$b_RoleFather) %>%
    as_tibble() %>%
    mutate(Leave = "Maternity Leave",
           Role = "Father",
           OnLeave = "Not On Leave")
  
  # Father on leave (during paternity leave period)
  father_on_leave <- exp(draws$b_RoleFather + draws$`b_RoleFather:LeavePaternityLeave`) %>%
    as_tibble() %>%
    mutate(Leave = "Paternity Leave",
           Role = "Father",
           OnLeave = "On Leave")
  
  # Mother on leave (during maternity leave period)
  mother_on_leave <- exp(draws$b_RoleMother) %>%
    as_tibble() %>%
    mutate(Leave = "Maternity Leave",
           Role = "Mother",
           OnLeave = "On Leave")
  
  # Mother not on leave (during paternity leave period)
  mother_not_on_leave <- exp(draws$b_RoleMother + draws$`b_RoleMother:LeavePaternityLeave`) %>%
    as_tibble() %>%
    mutate(Leave = "Paternity Leave",
           Role = "Mother",
           OnLeave = "Not On Leave")
  
  # Combine all scenarios
  summary_data <- rbind(father_not_on_leave, father_on_leave, 
                        mother_on_leave, mother_not_on_leave) %>%
    mutate(Metric = metric_name)
  
  return(summary_data)
}

extract_model_predictions_weekend <- function(model, metric_name) {
  #' Extract and format model predictions for leave status analysis
  #' 
  #' @param model A fitted Bayesian model object (e.g., from brms)
  #' @param metric_name Character string for the metric name (e.g., "Max Distance", "Radius Of Gyration")
  #' 
  #' @return A tibble with columns: value, Leave, Role, OnLeave, Metric
  #' 
  #' @examples
  #' max_distance_summary <- extract_model_predictions(model_maxDistanceFromHomeKm_simple, "Max Distance")
  #' gyration_summary <- extract_model_predictions(model_radiusOfGyrationKm_simple, "Radius Of Gyration")
  
  # Extract draws from the model
  draws <- as_draws_df(model)
  
  # Father not on leave (during maternity leave period)
  father_not_on_leave <- exp(draws$b_RoleFather + draws$`b_RoleFather:LeaveMaternityLeave:isWeekendWeekend`) %>%
    as_tibble() %>%
    mutate(Leave = "Maternity Leave",
           Role = "Father",
           OnLeave = "Not On Leave")
  
  # Father on leave (during paternity leave period)
  father_on_leave <- exp(draws$b_RoleFather + draws$`b_RoleFather:LeavePaternityLeave:isWeekendWeekend`) %>%
    as_tibble() %>%
    mutate(Leave = "Paternity Leave",
           Role = "Father",
           OnLeave = "On Leave")
  
  # Mother on leave (during maternity leave period)
  mother_on_leave <- exp(draws$b_RoleMother + draws$`b_RoleMother:LeaveMaternityLeave:isWeekendWeekend`) %>%
    as_tibble() %>%
    mutate(Leave = "Maternity Leave",
           Role = "Mother",
           OnLeave = "On Leave")
  
  # Mother not on leave (during paternity leave period)
  mother_not_on_leave <- exp(draws$b_RoleMother + draws$`b_RoleMother:LeavePaternityLeave:isWeekendWeekend`) %>%
    as_tibble() %>%
    mutate(Leave = "Paternity Leave",
           Role = "Mother",
           OnLeave = "Not On Leave")
  
  # Combine all scenarios
  summary_data <- rbind(father_not_on_leave, father_on_leave, 
                        mother_on_leave, mother_not_on_leave) %>%
    mutate(Metric = metric_name)
  
  return(summary_data)
}

get_subject_predictions_HPer <- function(model) {
  
  # Use the model's original data
  newdata <- model$data
  
  # Get predictions with random effects
  pred_data <- add_predicted_draws(model, newdata = newdata, allow_new_levels = TRUE)
  
  # Get median prediction for each observation
  pred_summary <- pred_data %>%
    group_by(Role, Leave, ParticipantID, isWeekend) %>%
    summarise(median_pred = mean(.prediction), .groups = "drop")
  
  return(pred_summary)
}

calculate_evidence_ratio <- function(posterior_samples, threshold = 0, direction = "greater") {
  n <- length(posterior_samples)
  
  if (direction == "greater") {
    count <- sum(posterior_samples > threshold, na.rm = TRUE)
  } else {
    count <- sum(posterior_samples < threshold, na.rm = TRUE)
  }
  
  prob <- count / n
  evidence_ratio <- prob / (1 - prob)
  
  # Format evidence ratio description
  if (evidence_ratio > 1) {
    er_description <- sprintf("%.1f", evidence_ratio)
  } else {
    er_description <- sprintf("%.1f", 1/evidence_ratio)
  }
  
  return(list(
    probability = prob,
    evidence_ratio = evidence_ratio,
    description = er_description,
    n_samples = n
  ))
}

calculate_difference_evidence_ratio <- function(mother_samples, father_samples, threshold = 0, direction = "greater") {
  # Calculate difference (mother - father)
  difference_samples <- mother_samples - father_samples
  return(calculate_evidence_ratio(difference_samples, threshold, direction))
}

format_evidence_ratio <- function(er_result) {
  if (er_result$evidence_ratio > 1) {
    return(sprintf("%.1f:1", er_result$evidence_ratio))
  } else {
    return(sprintf("1:%.1f", 1/er_result$evidence_ratio))
  }
}

get_subject_predictions_med <- function(model) {
  
  # Use the model's original data
  newdata <- model$data
  
  # Get predictions with random effects
  pred_data <- add_predicted_draws(model, newdata = newdata, allow_new_levels = TRUE)
  
  # Get median prediction for each observation
  pred_summary <- pred_data %>%
    group_by(Role, Leave, ParticipantID, isWeekend) %>%
    summarise(median_pred = median(.prediction), .groups = "drop")
  
  return(pred_summary)
}

leavediff_summarystats <- function(model){
  
  draws <- as_draws_df(model)
  father_not_on_leave <- exp(draws$b_RoleFather)
  father_on_leave <- exp(draws$b_RoleFather + draws$`b_RoleFather:LeavePaternityLeave`)
  mother_on_leave <- exp(draws$b_RoleMother)  
  mother_not_on_leave <- exp(draws$b_RoleMother + draws$`b_RoleMother:LeavePaternityLeave`)
  father_not_on_leave_summary <- median_qi(father_not_on_leave, .width = c(.95))
  father_on_leave_summary <- median_qi(father_on_leave, .width = c(.95))
  mother_not_on_leave_summary <- median_qi(mother_not_on_leave, .width = c(.95))
  mother_on_leave_summary <- median_qi(mother_on_leave, .width = c(.95))
  summary_stats <- tibble(
    Statistic = c(
      "FatherNotOnLeaveMean",
      "FatherNotOnLeaveCI_Lower",
      "FatherNotOnLeaveCI_Upper",
      "FatherOnLeaveMean",
      "FatherOnLeaveCI_Lower", 
      "FatherOnLeaveCI_Upper",
      "MotherNotOnLeaveMean",
      "MotherNotOnLeaveCI_Lower",
      "MotherNotOnLeaveCI_Upper",
      "MotherOnLeaveMean",
      "MotherOnLeaveCI_Lower",
      "MotherOnLeaveCI_Upper"
    ),
    Value = c(
      round(father_not_on_leave_summary$y, 2),
      round(father_not_on_leave_summary$ymin, 2),
      round(father_not_on_leave_summary$ymax, 2),
      round(father_on_leave_summary$y, 2),
      round(father_on_leave_summary$ymin, 2),
      round(father_on_leave_summary$ymax, 2),
      round(mother_not_on_leave_summary$y, 2),
      round(mother_not_on_leave_summary$ymin, 2),
      round(mother_not_on_leave_summary$ymax, 2),
      round(mother_on_leave_summary$y, 2),
      round(mother_on_leave_summary$ymin, 2),
      round(mother_on_leave_summary$ymax, 2)
    )
  )
  
  return(summary_stats)
}

leavediff_stats <- function(model){
  
  draws <- as_draws_df(model)
  father_not_on_leave <- exp(draws$b_RoleFather)
  father_on_leave <- exp(draws$b_RoleFather + draws$`b_RoleFather:LeavePaternityLeave`)
  mother_on_leave <- exp(draws$b_RoleMother)  
  mother_not_on_leave <- exp(draws$b_RoleMother + draws$`b_RoleMother:LeavePaternityLeave`)
  prob_father_higher_not_on_leave <- mean(father_not_on_leave > mother_not_on_leave)
  prob_father_higher_on_leave <- mean(father_on_leave > mother_on_leave)
  prob_father_decreases_on_leave <- mean(father_on_leave < father_not_on_leave)
  prob_mother_decreases_on_leave <- mean(mother_on_leave < mother_not_on_leave)
  er_father_higher_not_on_leave <- prob_father_higher_not_on_leave / (1 - prob_father_higher_not_on_leave)
  er_father_higher_on_leave <- prob_father_higher_on_leave / (1 - prob_father_higher_on_leave)
  er_father_decreases_on_leave <- prob_father_decreases_on_leave / (1 - prob_father_decreases_on_leave)
  er_mother_increases_on_leave <- prob_mother_decreases_on_leave / (1 - prob_mother_decreases_on_leave)
  
  stats_table <- tibble(
    TestDescription = c(
      "FatherHigherNotOnLeave",
      "FatherHigherOnLeave", 
      "FatherDecreasesOnLeave",
      "MotherDecreasesOnLeave"
    ),
    Evid.Ratio = c(
      round(er_father_higher_not_on_leave, 2),
      round(er_father_higher_on_leave, 2),
      round(er_father_decreases_on_leave, 2),
      round(er_mother_increases_on_leave, 2)
    ))
  
  
  return(stats_table)
}

leavediff_summarystats_HPer <- function(model) {
  
  # Get participant structure from the model data
  father_participants <- unique(model$data$ParticipantID[model$data$Role == "Father"])
  mother_participants <- unique(model$data$ParticipantID[model$data$Role == "Mother"])
  
  # Create newdata with realistic participant-role combinations
  newdata <- bind_rows(
    expand_grid(
      ParticipantID = father_participants,
      Role = "Father",
      Leave = c("Maternity Leave", "Paternity Leave"),
      isWeekend = "Weekday"
    ),
    expand_grid(
      ParticipantID = mother_participants,
      Role = "Mother", 
      Leave = c("Maternity Leave", "Paternity Leave"),
      isWeekend = "Weekday"
    )
  )
  
  # Get predictions that incorporate ALL model components (mu, sigma, hu)
  draws <- add_epred_draws(model, newdata = newdata)
  
  # Average across participants within each condition for each draw
  draws_averaged <- draws %>%
    group_by(.draw, Role, Leave) %>%
    summarise(avg_epred = mean(.epred), .groups = "drop")
  
  # Calculate summary statistics
  summary_stats <- draws_averaged %>%
    group_by(Role, Leave) %>%
    summarise(
      mean_pred = mean(avg_epred),
      ci_lower = quantile(avg_epred, 0.025),
      ci_upper = quantile(avg_epred, 0.975),
      .groups = "drop"
    ) %>%
    mutate(
      condition = case_when(
        Role == "Father" & Leave == "Maternity Leave" ~ "FatherNotOnLeave",
        Role == "Father" & Leave == "Paternity Leave" ~ "FatherOnLeave",
        Role == "Mother" & Leave == "Paternity Leave" ~ "MotherNotOnLeave",
        Role == "Mother" & Leave == "Maternity Leave" ~ "MotherOnLeave"
      )
    ) %>%
    select(condition, mean_pred, ci_lower, ci_upper) %>%
    pivot_longer(cols = c(mean_pred, ci_lower, ci_upper), 
                 names_to = "stat_type", 
                 values_to = "value") %>%
    mutate(
      Statistic = paste0(condition, case_when(
        stat_type == "mean_pred" ~ "Mean",
        stat_type == "ci_lower" ~ "CI_Lower", 
        stat_type == "ci_upper" ~ "CI_Upper"
      ))
    ) %>%
    select(Statistic, Value = value) %>%
    mutate(Value = round(Value, 2))
  
  # Extract averaged predictions for evidence ratios
  father_not_on_leave <- draws_averaged %>% filter(Role == "Father", Leave == "Maternity Leave") %>% pull(avg_epred)
  father_on_leave <- draws_averaged %>% filter(Role == "Father", Leave == "Paternity Leave") %>% pull(avg_epred)
  mother_not_on_leave <- draws_averaged %>% filter(Role == "Mother", Leave == "Paternity Leave") %>% pull(avg_epred)  
  mother_on_leave <- draws_averaged %>% filter(Role == "Mother", Leave == "Maternity Leave") %>% pull(avg_epred)
  
  # Calculate probabilities and evidence ratios
  prob_father_higher_not_on_leave <- mean(father_not_on_leave > mother_not_on_leave)
  prob_father_higher_on_leave <- mean(father_on_leave > mother_on_leave)
  prob_father_decreases_on_leave <- mean(father_not_on_leave < father_on_leave)
  prob_mother_decreases_on_leave <- mean(mother_not_on_leave < mother_on_leave)
  
  # Convert to evidence ratios
  er_father_higher_not_on_leave <- prob_father_higher_not_on_leave / (1 - prob_father_higher_not_on_leave)
  er_father_higher_on_leave <- prob_father_higher_on_leave / (1 - prob_father_higher_on_leave)
  er_father_decreases_on_leave <- prob_father_decreases_on_leave / (1 - prob_father_decreases_on_leave)
  er_mother_decreases_on_leave <- prob_mother_decreases_on_leave / (1 - prob_mother_decreases_on_leave)
  
  evidence_ratios <- tibble(
    TestDescription = c(
      "FatherHigherNotOnLeave",
      "FatherHigherOnLeave", 
      "FatherDecreasesOnLeave",
      "MotherDecreasesOnLeave"
    ),
    Evid.Ratio = c(
      round(er_father_higher_not_on_leave, 2),
      round(er_father_higher_on_leave, 2),
      round(er_father_decreases_on_leave, 2),
      round(er_mother_decreases_on_leave, 2)
    )
  )
  
  return(list(
    summary_stats = summary_stats,
    evidence_ratios = evidence_ratios
  ))
}

posterior_summarystats <- function(draws_data, metric_name) {
  metric_data <- draws_data %>% filter(Metric == metric_name)

  summary_stats <- metric_data %>%
    group_by(Role, OnLeave) %>%
    summarise(
      mean_pred = mean(value),
      ci_lower = quantile(value, 0.025),
      ci_upper = quantile(value, 0.975),
      .groups = "drop"
    ) %>%
    mutate(
      condition = case_when(
        Role == "Father" & OnLeave == "Not On Leave" ~ "FatherNotOnLeave",
        Role == "Father" & OnLeave == "On Leave" ~ "FatherOnLeave",
        Role == "Mother" & OnLeave == "Not On Leave" ~ "MotherNotOnLeave",
        Role == "Mother" & OnLeave == "On Leave" ~ "MotherOnLeave"
      )
    ) %>%
    select(condition, mean_pred, ci_lower, ci_upper) %>%
    pivot_longer(cols = c(mean_pred, ci_lower, ci_upper), 
                 names_to = "stat_type", 
                 values_to = "value") %>%
    mutate(
      Statistic = paste0(condition, case_when(
        stat_type == "mean_pred" ~ "Mean",
        stat_type == "ci_lower" ~ "CI_Lower", 
        stat_type == "ci_upper" ~ "CI_Upper"
      ))
    ) %>%
    select(Statistic, Value = value) %>%
    mutate(Value = round(Value, 2))
  
  # Extract draws for evidence ratios
  father_not_on_leave <- metric_data %>% 
    filter(Role == "Father", OnLeave == "Not On Leave") %>% 
    pull(value)
  
  father_on_leave <- metric_data %>% 
    filter(Role == "Father", OnLeave == "On Leave") %>% 
    pull(value)
  
  mother_not_on_leave <- metric_data %>% 
    filter(Role == "Mother", OnLeave == "Not On Leave") %>% 
    pull(value)
  
  mother_on_leave <- metric_data %>% 
    filter(Role == "Mother", OnLeave == "On Leave") %>% 
    pull(value)
  
  # Calculate probabilities and evidence ratios
  prob_father_higher_not_on_leave <- mean(father_not_on_leave > mother_not_on_leave)
  prob_father_higher_on_leave <- mean(father_on_leave < mother_on_leave)
  prob_father_decreases_on_leave <- mean(father_not_on_leave > father_on_leave)
  prob_mother_decreases_on_leave <- mean(mother_not_on_leave > mother_on_leave)
  
  # Convert to evidence ratios
  er_father_higher_not_on_leave <- prob_father_higher_not_on_leave / (1 - prob_father_higher_not_on_leave)
  er_father_higher_on_leave <- prob_father_higher_on_leave / (1 - prob_father_higher_on_leave)
  er_father_decreases_on_leave <- prob_father_decreases_on_leave / (1 - prob_father_decreases_on_leave)
  er_mother_decreases_on_leave <- prob_mother_decreases_on_leave / (1 - prob_mother_decreases_on_leave)
  
  evidence_ratios <- tibble(
    TestDescription = c(
      "FatherHigherNotOnLeave",
      "FatherHigherOnLeave", 
      "FatherDecreasesOnLeave",
      "MotherDecreasesOnLeave"
    ),
    Evid.Ratio = c(
      round(er_father_higher_not_on_leave, 2),
      round(er_father_higher_on_leave, 2),
      round(er_father_decreases_on_leave, 2),
      round(er_mother_decreases_on_leave, 2)
    )
  )
  
  return(list(
    summary_stats = summary_stats,
    evidence_ratios = evidence_ratios
  ))
}

extract_hypothesis_stats <- function(hyp_result, test_name) {
  
  # Extract the main results
  estimate <- hyp_result$hypothesis$Estimate[1]
  est_error <- hyp_result$hypothesis$Est.Error[1] 
  ci_lower <- hyp_result$hypothesis$CI.Lower[1]
  ci_upper <- hyp_result$hypothesis$CI.Upper[1]
  evid_ratio <- hyp_result$hypothesis$Evid.Ratio[1]
  post_prob <- hyp_result$hypothesis$Post.Prob[1]
  
  # Calculate odds ratio and its CI (for logit models)
  odds_ratio <- exp(estimate)
  or_ci_lower <- exp(ci_lower)
  or_ci_upper <- exp(ci_upper)
  
  # Create tibble with all stats
  tibble(
    Test = test_name,
    Estimate = round(estimate, 3),
    EstError = round(est_error, 3),
    CI_Lower = round(ci_lower, 3),
    CI_Upper = round(ci_upper, 3),
    OddsRatio = round(odds_ratio, 3),
    OR_CI_Lower = round(or_ci_lower, 3),
    OR_CI_Upper = round(or_ci_upper, 3),
    EvidenceRatio = round(evid_ratio, 1),
    PostProb = round(post_prob, 3)
  )
}

calculate_evidence_ratio <- function(posterior_samples, threshold = 0, direction = "greater") {
  n <- length(posterior_samples)
  
  if (direction == "greater") {
    count <- sum(posterior_samples > threshold, na.rm = TRUE)
  } else {
    count <- sum(posterior_samples < threshold, na.rm = TRUE)
  }
  
  prob <- count / n
  evidence_ratio <- prob / (1 - prob)
  
  # Format evidence ratio description
  if (evidence_ratio > 1) {
    er_description <- sprintf("%.1f", evidence_ratio)
  } else {
    er_description <- sprintf("%.1f", 1/evidence_ratio)
  }
  
  return(list(
    probability = prob,
    evidence_ratio = evidence_ratio,
    description = er_description,
    n_samples = n
  ))
}

# Function to calculate evidence ratio for differences
calculate_difference_evidence_ratio <- function(mother_samples, father_samples, threshold = 0, direction = "greater") {
  # Calculate difference (mother - father)
  difference_samples <- mother_samples - father_samples
  return(calculate_evidence_ratio(difference_samples, threshold, direction))
}

fit_simple_hurdle_model <- function(
    outcome_var,
    data = d_GPS,
    max_treedepth = 15,
    adapt_delta = 0.95,
    cores = 64,
    save_path = NULL
) {
  formula_main <- paste0(
    outcome_var, " ~ 0 + Role + Role:Leave + Role:Leave:isWeekend + (Leave | gr(ParticipantID, by = Role))"
  )
  formula_hu <- "hu ~ 0 + Role + Role:Leave + Role:Leave:isWeekend"
  formula_sigma <- "sigma ~ 0 + Role + Role:Leave + Role:Leave:isWeekend"
  
  formula_full <- bf(
    formula(formula_main),
    formula(formula_hu),
    formula(formula_sigma)
  )
  
  # Fit model
  cat("Fitting simple model for", outcome_var, "...\n")
  
  model <- brm(
    formula_full,
    family = hurdle_lognormal(),
    prior = priors_hurdle,
    data = data,
    cores = cores,
    chains = 2,
    warmup = 500,
    iter = 2000,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    backend = 'cmdstan'
  )
  
  # Save model if path provided
  if (!is.null(save_path)) {
    model_name <- paste0("model_", gsub("[^A-Za-z0-9]", "", outcome_var), "_simple.rds")
    full_path <- file.path(save_path, model_name)
    saveRDS(model, full_path)
    cat("Model saved to:", full_path, "\n")
  }
  
  # Run posterior predictive check
  cat("Running posterior predictive check...\n")
  pp_plot <- pp_check(model, ndraws = 100) + 
    xlim(c(0, quantile(data[[outcome_var]], 0.95, na.rm = TRUE))) +
    ggtitle(paste("PP Check:", outcome_var, "- Simple Model"))
  
  # Compute LOO
  cat("Computing LOO-CV...\n")
  model_loo <- loo(model)
  
  # Return list with model, pp_check, and loo
  result <- list(
    model = model,
    pp_check = pp_plot,
    loo = model_loo,
    outcome = outcome_var,
    model_type = "simple"
  )
  
  class(result) <- "simple_hurdle_result"
  return(result)
}

fit_simple_hurdle_gamma_model <- function(
    outcome_var,
    data = d_GPS,
    max_treedepth = 15,
    adapt_delta = 0.99,
    cores = 64,
    save_path = NULL
) {
  formula_main <- paste0(
    outcome_var, " ~ 0 + Role + Role:Leave + Role:Leave:isWeekend + (Leave | gr(ParticipantID, by = Role))"
  )
  formula_hu <- "hu ~ 0 + Role + Role:Leave + Role:Leave:isWeekend"
  formula_shape <- "shape ~ 0 + Role + Role:Leave + Role:Leave:isWeekend"  # Shape instead of sigma for gamma
  
  formula_full <- bf(
    formula(formula_main),
    formula(formula_hu),
    formula(formula_shape)
  )
  
  # Fit model
  cat("Fitting simple hurdle gamma model for", outcome_var, "...\n")
  
  model <- brm(
    formula_full,
    family = hurdle_gamma(),  # Changed to hurdle_gamma
    prior = priors_hurdle_gamma,  # Use gamma-specific priors
    data = data,
    cores = cores,
    chains = 2,
    warmup = 500,
    iter = 2000,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    backend = 'cmdstan'
  )
  
  # Save model if path provided
  if (!is.null(save_path)) {
    model_name <- paste0("model_", gsub("[^A-Za-z0-9]", "", outcome_var), "_simple_gamma.rds")
    full_path <- file.path(save_path, model_name)
    saveRDS(model, full_path)
    cat("Model saved to:", full_path, "\n")
  }
  
  # Run posterior predictive check
  cat("Running posterior predictive check...\n")
  pp_plot <- pp_check(model, ndraws = 100) + 
    xlim(c(0, quantile(data[[outcome_var]], 0.95, na.rm = TRUE))) +
    ggtitle(paste("PP Check:", outcome_var, "- Simple Hurdle Gamma Model"))
  
  # Compute LOO
  cat("Computing LOO-CV...\n")
  model_loo <- loo(model)
  
  # Return list with model, pp_check, and loo
  result <- list(
    model = model,
    pp_check = pp_plot,
    loo = model_loo,
    outcome = outcome_var,
    model_type = "simple_hurdle_gamma"
  )
  
  class(result) <- "simple_hurdle_result"
  return(result)
}

get_subject_predictions_with_er <- function(model) {
  
  # Use the model's original data
  newdata <- model$data
  
  # Get predictions with random effects (keeping all posterior draws)
  pred_data <- add_predicted_draws(model, newdata = newdata, allow_new_levels = TRUE)
  
  # Create leave status variable
  pred_data <- pred_data %>%
    mutate(OnLeave = case_when(
      Leave == "Paternity Leave" & Role == "Father" ~ "On Leave",
      Leave == "Maternity Leave" & Role == "Mother" ~ "On Leave",
      Leave == "Paternity Leave" & Role == "Mother" ~ "Not On Leave",
      Leave == "Maternity Leave" & Role == "Father" ~ "Not On Leave"
    ))
  
  # Get summary statistics for each group
  summary_stats <- pred_data %>%
    group_by(Role, OnLeave, .draw) %>%
    summarise(mean_pred = median(.prediction), .groups = "drop") %>%
    group_by(Role, OnLeave) %>%
    summarise(
      Mean = mean(mean_pred),
      CI_Lower = quantile(mean_pred, 0.025),
      CI_Upper = quantile(mean_pred, 0.975),
      .groups = "drop"
    ) %>%
    mutate(Statistic = paste0(Role, OnLeave, "Mean")) %>%
    select(Statistic, Value = Mean) %>%
    bind_rows(
      pred_data %>%
        group_by(Role, OnLeave, .draw) %>%
        summarise(mean_pred = median(.prediction), .groups = "drop") %>%
        group_by(Role, OnLeave) %>%
        summarise(
          CI_Lower = quantile(mean_pred, 0.025),
          CI_Upper = quantile(mean_pred, 0.975),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(CI_Lower, CI_Upper), 
                     names_to = "CI_Type", 
                     values_to = "Value") %>%
        mutate(Statistic = paste0(Role, OnLeave, CI_Type)) %>%
        select(Statistic, Value)
    )
  
  # Calculate evidence ratios
  posterior_draws <- pred_data %>%
    group_by(Role, OnLeave, .draw) %>%
    summarise(mean_pred = median(.prediction), .groups = "drop") %>%
    pivot_wider(names_from = c(Role, OnLeave), 
                values_from = mean_pred,
                names_sep = "")
  
  # Evidence ratios
  evidence_ratios <- tibble(
    TestDescription = c(
      "FatherHigherNotOnLeave",
      "FatherHigherOnLeave", 
      "FatherDecreasesOnLeave",
      "MotherDecreasesOnLeave"
    ),
    Evid.Ratio = c(
      # Father higher when not on leave vs Mother not on leave
      sum(posterior_draws$`FatherNot On Leave` > posterior_draws$`MotherNot On Leave`) / 
        sum(posterior_draws$`FatherNot On Leave` <= posterior_draws$`MotherNot On Leave`),
      
      # Father higher when on leave vs Mother on leave  
      sum(posterior_draws$`FatherOn Leave` > posterior_draws$`MotherOn Leave`) / 
        sum(posterior_draws$`FatherOn Leave` <= posterior_draws$`MotherOn Leave`),
      
      # Father decreases on leave (inverse ratio since we expect decrease)
      sum(posterior_draws$`FatherNot On Leave` > posterior_draws$`FatherOn Leave`) / 
        sum(posterior_draws$`FatherNot On Leave` <= posterior_draws$`FatherOn Leave`),
      
      # Mother decreases on leave (inverse ratio since we expect decrease)
      sum(posterior_draws$`MotherNot On Leave` > posterior_draws$`MotherOn Leave`) / 
        sum(posterior_draws$`MotherNot On Leave` <= posterior_draws$`MotherOn Leave`)
    )
  )
  
  # Also get median predictions for plotting (as before)
  median_predictions <- pred_data %>%
    group_by(Role, Leave, ParticipantID, isWeekend) %>%
    summarise(median_pred = median(.prediction), .groups = "drop")
  
  return(list(
    summary_stats = summary_stats,
    evidence_ratios = evidence_ratios
    #median_predictions = median_predictions,
    #posterior_draws = posterior_draws
  ))
}
